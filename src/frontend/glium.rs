//! Render to a window created by Glutin, using Glium's OpenGL functions

// FIXME: Support resizing the window, like the SDL renderer does

use breeze::frontend::FrontendAction;
use breeze::ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use glium::{DisplayBuild, Surface, Rect};
use glium::backend::glutin_backend::GlutinFacade;
use glium::index::{NoIndices, PrimitiveType};
use glium::glutin::WindowBuilder;
use glium::program::Program;
use glium::texture::{ClientFormat, RawImage2d, Texture2d};
use glium::uniforms::MagnifySamplerFilter;
use glium::vertex::VertexBuffer;

use std::borrow::Cow;

/// Our vertices are extremely simple: There's no need for more than 2 dimensions and the texture
/// coordinates can be calculated from the position, so we don't need to store them.
#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
}

impl Vertex {
    fn new(x: f32, y: f32) -> Self { Vertex { position: [x, y] } }
}

implement_vertex!(Vertex, position);

const VERTEX_SHADER_SRC: &'static str = r#"
    #version 140

    in vec2 position;
    out vec2 tex_coords;

    void main() {
        tex_coords = vec2((position.x + 1) * 0.5, 1 - (position.y + 1) * 0.5);
        gl_Position = vec4(position, 0.0, 1.0);
    }
"#;

const FRAGMENT_SHADER_SRC: &'static str = r#"
    #version 140

    in vec2 tex_coords;
    out vec4 color;

    uniform sampler2D tex;

    void main() {
        color = texture(tex, tex_coords);
    }
"#;

pub struct GliumRenderer {
    display: GlutinFacade,
    /// This vertex buffer will only ever store 4 vertices spanning the whole window
    vbuf: VertexBuffer<Vertex>,
    /// A simple shader that maps our texture onto the window
    program: Program,
    /// This texture is updated with the PPU's data every frame
    texture: Texture2d,
}

impl Default for GliumRenderer {
    fn default() -> Self {
        let display = WindowBuilder::new()
            .with_dimensions(SCREEN_WIDTH * 3, SCREEN_HEIGHT * 3)
            .with_title("breeze".to_owned())
            .build_glium().unwrap();
        // Create a rectangle spanning the whole viewport/window. This is a triangle strip.
        let shape = [
            Vertex::new(-1.0, 1.0),
            Vertex::new(1.0, 1.0),
            Vertex::new(-1.0, -1.0),
            Vertex::new(1.0, -1.0),
        ];

        GliumRenderer {
            vbuf: VertexBuffer::new(&display, &shape).unwrap(),
            program: Program::from_source(&display, VERTEX_SHADER_SRC, FRAGMENT_SHADER_SRC, None)
                .unwrap(),
            texture: Texture2d::empty(&display, SCREEN_WIDTH, SCREEN_HEIGHT)
                .unwrap(),
            display: display,
        }
    }
}

impl GliumRenderer {
    fn handle_events(&mut self) -> Option<FrontendAction> {
        use glium::glutin::Event::*;

        for ev in self.display.poll_events() {
            match ev {
                Closed => {
                    info!("quit event -> exiting");
                    return Some(FrontendAction::Exit)
                }
                _ => {}
            }
        }
        None
    }
}

impl super::Renderer for GliumRenderer {
    fn render(&mut self, frame_data: &[u8]) -> Option<FrontendAction> {
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
}
