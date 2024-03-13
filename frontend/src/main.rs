#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use eframe::egui;

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([800.0, 600.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Mesha",
        options,
        Box::new(|_cc| {
            Box::<App>::default()
        }),
    )
}

struct App {
    cells: lru::LruCache<usize, Cell>,
}

struct Cell {
    rect: egui::Rect,
    content: String
}

impl Default for App {
    fn default() -> Self {
        use egui::*;
        use lru::LruCache;
        use std::num::NonZeroUsize;

        let mut cells = LruCache::new(NonZeroUsize::new(25).unwrap());
        cells.put(1, Cell { rect: Rect::from_two_pos(pos2(200.0, 50.0), pos2(600.0, 150.0)), content: "Test".to_owned() });
        cells.put(2, Cell { rect: Rect::from_two_pos(pos2(200.0, 150.0), pos2(600.0, 250.0)), content: "123".to_owned() });
        cells.put(3, Cell { rect: Rect::from_two_pos(pos2(200.0, 250.0), pos2(400.0, 350.0)), content: "Parent".to_owned() });
        cells.put(4, Cell { rect: Rect::from_two_pos(pos2(400.0, 250.0), pos2(600.0, 300.0)), content: "1337".to_owned() });
        cells.put(5, Cell { rect: Rect::from_two_pos(pos2(400.0, 300.0), pos2(600.0, 350.0)), content: "Kiran".to_owned() });
        cells.put(6, Cell { rect: Rect::from_two_pos(pos2(200.0, 350.0), pos2(400.0, 450.0)), content: "List".to_owned() });
        cells.put(7, Cell { rect: Rect::from_two_pos(pos2(400.0, 350.0), pos2(450.0, 450.0)), content: "1".to_owned() });
        cells.put(8, Cell { rect: Rect::from_two_pos(pos2(450.0, 350.0), pos2(500.0, 450.0)), content: "2".to_owned() });
        cells.put(9, Cell { rect: Rect::from_two_pos(pos2(500.0, 350.0), pos2(550.0, 450.0)), content: "3".to_owned() });
        cells.put(10, Cell { rect: Rect::from_two_pos(pos2(550.0, 350.0), pos2(600.0, 450.0)), content:  "4".to_owned() });

        Self {
            cells
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        use egui::*;

        CentralPanel::default().show(ctx, |ui| {
            for (_key, cell) in &self.cells {
                let stroke = Stroke::new(3.0, Color32::WHITE);
                let rect = cell.rect;
                let text = &cell.content;
                let p = ui.painter_at(rect);
                p.rect_stroke(rect, Rounding::ZERO, stroke);
                p.text(rect.shrink(10.0).left_top(), Align2::LEFT_TOP, text, TextStyle::Heading.resolve(&ctx.style()), Color32::WHITE);
            }
        });
    }
}
