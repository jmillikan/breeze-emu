//! Render to a window created by Glutin, using Glium's OpenGL functions

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use glium::{DisplayBuild, Surface, Rect};
use glium::backend::glutin_backend::GlutinFacade;
use glium::index::{NoIndices, PrimitiveType};
use glium::glutin::WindowBuilder;
use glium::program::Program;
use glium::texture::{ClientFormat, RawImage2d, Texture2d};
use glium::vertex::VertexBuffer;

use std::process;
use std::borrow::Cow;

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
        gl_Position = vec4(position, 0.0, 1.0);
        tex_coords = (position + vec2(1,1)) * vec2(0.5, -0.5);
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
    vbuf: VertexBuffer<Vertex>,
    program: Program,
    texture: Texture2d,
}

impl Default for GliumRenderer {
    fn default() -> Self {
        let display = WindowBuilder::new()
            .with_dimensions(SCREEN_WIDTH as u32 * 3, SCREEN_HEIGHT as u32 * 3)
            .with_title("sneeze".to_owned())
            .build_glium().unwrap();
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
            texture: Texture2d::empty(&display, SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
                .unwrap(),
            display: display,
        }
    }
}

impl GliumRenderer {
    fn handle_events(&mut self) {
        use glium::glutin::Event::*;

        for ev in self.display.poll_events() {
            match ev {
                Closed => {
                    info!("quit event -> exiting");
                    process::exit(0);
                }
                _ => ()
            }
        }
    }
}

impl super::Renderer for GliumRenderer {
    fn render(&mut self, frame_data: &[u8]) {
        // upload new texture data
        self.texture.write(Rect {
            left: 0,
            bottom: 0,
            width: SCREEN_WIDTH as u32,
            height: SCREEN_HEIGHT as u32,
        }, RawImage2d {
            data: Cow::Borrowed(frame_data),
            width: SCREEN_WIDTH as u32,
            height: SCREEN_HEIGHT as u32,
            format: ClientFormat::U8U8U8,
        });

        let mut target = self.display.draw();
        target.draw(
            &self.vbuf,
            &NoIndices(PrimitiveType::TriangleStrip),
            &self.program,
            &uniform! {
                tex: &self.texture,
            },
            &Default::default()).unwrap();
        target.finish().unwrap();

        self.handle_events();
    }
}
