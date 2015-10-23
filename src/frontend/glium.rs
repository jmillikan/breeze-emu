//! Render to a window created by Glutin, using Glium's OpenGL functions

use ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};

use glium::{DisplayBuild, Surface};
use glium::backend::glutin_backend::GlutinFacade;
use glium::index::{NoIndices, PrimitiveType};
use glium::glutin::WindowBuilder;
use glium::vertex::VertexBuffer;

use std::process;

#[derive(Copy, Clone)]
struct Vertex {
    pos: [f32; 2],
}

impl Vertex {
    fn new(x: f32, y: f32) -> Self { Vertex { pos: [x, y] } }
}

implement_vertex!(Vertex, pos);

pub struct GliumRenderer {
    display: GlutinFacade,
    vbuf: VertexBuffer<Vertex>,
    index: NoIndices,
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
            Vertex::new(1.0, -1.0),
            Vertex::new(-1.0, -1.0),
        ];
        let vbuf = VertexBuffer::new(&display, &shape).unwrap();
        let index = NoIndices(PrimitiveType::TriangleStrip);

        GliumRenderer {
            display: display,
            vbuf: vbuf,
            index: index,
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
        target.finish().unwrap();

        self.handle_events();
    }
}
