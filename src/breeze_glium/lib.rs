//! Render to a window created by Glutin, using Glium's OpenGL functions

#[macro_use] extern crate log;
#[macro_use] extern crate glium;
extern crate breeze_backend;

use breeze_backend::{BackendAction, BackendResult, Renderer};
use breeze_backend::ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};
use breeze_backend::viewport::Viewport;

use glium::{DisplayBuild, Surface, Rect};
use glium::backend::glutin_backend::GlutinFacade;
use glium::index::{NoIndices, PrimitiveType};
use glium::glutin::WindowBuilder;
use glium::program::Program;
use glium::texture::{ClientFormat, RawImage2d, SrgbTexture2d};
use glium::uniforms::MagnifySamplerFilter;
use glium::vertex::VertexBuffer;

use std::borrow::Cow;
use std::error::Error;

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

implement_vertex!(Vertex, position, tex_coords);

const VERTEX_SHADER_SRC: &'static str = r#"
    #version 140

    in vec2 position;
    in vec2 tex_coords;
    out vec2 v_tex_coords;

    void main() {
        v_tex_coords = tex_coords;
        gl_Position = vec4(position, 0.0, 1.0);
    }
"#;

const FRAGMENT_SHADER_SRC: &'static str = r#"
    #version 140

    in vec2 v_tex_coords;
    out vec4 color;

    uniform sampler2D tex;

    void main() {
        color = texture(tex, v_tex_coords);
    }
"#;

pub struct GliumRenderer {
    display: GlutinFacade,
    /// This vertex buffer will only ever store 4 vertices spanning the whole window
    vbuf: VertexBuffer<Vertex>,
    /// A simple shader that maps our texture onto the window
    program: Program,
    /// This texture is updated with the PPU's data every frame
    texture: SrgbTexture2d,
}

impl GliumRenderer {
    fn handle_events(&mut self) -> BackendResult<Vec<BackendAction>> {
        use glium::glutin::Event::*;

        for ev in self.display.poll_events() {
            match ev {
                Closed => {
                    info!("quit event -> exiting");
                    return Ok(vec![BackendAction::Exit]);
                }
                Resized(w, h) => {
                    resize(&mut self.vbuf, w, h);
                }
                _ => {}
            }
        }

        Ok(vec![])
    }
}

fn resize(vbuf: &mut VertexBuffer<Vertex>, win_w: u32, win_h: u32) {
    let Viewport { x, y, w, h } = Viewport::for_window_size(win_w, win_h);
    let (win_w, win_h) = (win_w as f32, win_h as f32);
    let (x, y, w, h) = (x as f32 / win_w, y as f32 / win_h, w as f32 / win_w, h as f32 / win_h);

    // Since I can't be bothered to put in a translation matrix, we have to translate the pixel
    // coords to OpenGL's [-1, 1] system.
    let vx = (x - 0.5) * 2.0;
    let vy = (y - 0.5) * 2.0;
    let rect = make_rect(vx, vy, w * 2.0, h * 2.0);
    vbuf.write(&rect);
}

/// Build 4 Vertices spanning up a rectangle. Bottom-Left corner = (-1, -1).
fn make_rect(x: f32, y: f32, w: f32, h: f32) -> [Vertex; 4] {
    // FIXME Use a matrix instead of rebuilding the geometry on every resize
    [
        Vertex { position: [x, y + h], tex_coords: [0.0, 0.0] },
        Vertex { position: [x + w, y + h], tex_coords: [1.0, 0.0] },
        Vertex { position: [x, y], tex_coords: [0.0, 1.0] },
        Vertex { position: [x + w, y], tex_coords: [1.0, 1.0] },
    ]
}

impl Renderer for GliumRenderer {
    fn create() -> Result<Self, Box<Error>> {
        let display = try!(WindowBuilder::new()
            .with_dimensions(SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
            .with_title("breeze".to_owned())
            .build_glium());

        let mut vbuf = try!(VertexBuffer::empty_dynamic(&display, 4));
        resize(&mut vbuf, SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3);

        Ok(GliumRenderer {
            vbuf: vbuf,
            program: try!(
                Program::from_source(&display, VERTEX_SHADER_SRC, FRAGMENT_SHADER_SRC, None)),
            texture: try!(SrgbTexture2d::empty(&display, SCREEN_WIDTH, SCREEN_HEIGHT)),
            display: display,
        })
    }

    fn render(&mut self, frame_data: &[u8]) -> BackendResult<Vec<BackendAction>> {
        // upload new texture data
        self.texture.write(Rect {
            left: 0,
            bottom: 0,
            width: SCREEN_WIDTH,
            height: SCREEN_HEIGHT,
        }, RawImage2d {
            data: Cow::Borrowed(frame_data),
            width: SCREEN_WIDTH,
            height: SCREEN_HEIGHT,
            format: ClientFormat::U8U8U8,
        });

        let mut target = self.display.draw();
        target.clear_color_srgb(0.0, 0.0, 0.0, 0.0);
        target.draw(
            &self.vbuf,
            &NoIndices(PrimitiveType::TriangleStrip),
            &self.program,
            &uniform! {
                tex: self.texture.sampled()
                    .magnify_filter(MagnifySamplerFilter::Nearest),
            },
            &Default::default()).unwrap();
        target.finish().unwrap();

        self.handle_events()
    }

    fn set_rom_title(&mut self, title: &str) {
        if let Some(win_ref) = self.display.get_window() {
            win_ref.set_title(title);
        }
    }
}
