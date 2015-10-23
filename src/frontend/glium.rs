//! Render to a window created by Glutin, using Glium's OpenGL functions

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use glium::{DisplayBuild, Surface};
use glium::backend::glutin_backend::GlutinFacade;
use glium::index::{NoIndices, PrimitiveType};
use glium::glutin::WindowBuilder;
use glium::program::Program;
use glium::uniforms;
use glium::vertex::VertexBuffer;

use std::process;

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

    void main() {
        gl_Position = vec4(position, 0.0, 1.0);
    }
"#;

const FRAGMENT_SHADER_SRC: &'static str = r#"
    #version 140

    out vec4 color;

    void main() {
        color = vec4(1.0, 0.0, 0.0, 1.0);
    }
"#;

pub struct GliumRenderer {
    display: GlutinFacade,
    vbuf: VertexBuffer<Vertex>,
    program: Program,
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
        let mut target = self.display.draw();
        target.draw(
            &self.vbuf,
            &NoIndices(PrimitiveType::TriangleStrip),
            &self.program,
            &uniforms::EmptyUniforms,
            &Default::default()).unwrap();
        target.finish().unwrap();

        self.handle_events();
    }
}
