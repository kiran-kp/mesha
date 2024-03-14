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
    current_selection: usize,
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

        let test_data = vec![(Rect::from_two_pos(pos2(200.0, 50.0), pos2(600.0, 150.0)), "Test"),
                             (Rect::from_two_pos(pos2(200.0, 150.0), pos2(600.0, 250.0)), "123"),
                             (Rect::from_two_pos(pos2(200.0, 250.0), pos2(400.0, 350.0)), "Parent"),
                             (Rect::from_two_pos(pos2(400.0, 250.0), pos2(600.0, 300.0)), "1337"),
                             (Rect::from_two_pos(pos2(400.0, 300.0), pos2(600.0, 350.0)), "Kiran"),
                             (Rect::from_two_pos(pos2(200.0, 350.0), pos2(400.0, 450.0)), "List"),
                             (Rect::from_two_pos(pos2(400.0, 350.0), pos2(450.0, 450.0)), "1"),
                             (Rect::from_two_pos(pos2(450.0, 350.0), pos2(500.0, 450.0)), "2"),
                             (Rect::from_two_pos(pos2(500.0, 350.0), pos2(550.0, 450.0)), "3"),
                             (Rect::from_two_pos(pos2(550.0, 350.0), pos2(600.0, 450.0)), "4")];

        let mut cells = LruCache::new(NonZeroUsize::new(25).unwrap());
        for (i, c) in test_data.iter().enumerate() {
            cells.put(i + 1, Cell { rect: c.0, content: c.1.to_owned() });
        }

        Self {
            current_selection: 1,
            cells
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        use egui::*;

        CentralPanel::default().show(ctx, |ui| {
            for (key, cell) in &self.cells {
                let stroke = Stroke::new(3.0, Color32::WHITE);
                let rect = cell.rect;
                if ui.rect_contains_pointer(rect) {
                    self.current_selection = *key;
                }
                let text = &cell.content;
                let p = ui.painter_at(rect);
                p.rect_stroke(rect, Rounding::ZERO, stroke);
                p.text(rect.shrink(10.0).left_top(), Align2::LEFT_TOP, text, TextStyle::Heading.resolve(&ctx.style()), Color32::WHITE);
            }

            let stroke = Stroke::new(3.0, Color32::RED);
            let selected_cell = &self.cells.get(&self.current_selection).unwrap();
            let rect = selected_cell.rect;
            let p = ui.painter_at(rect.expand(2.0));
            p.rect_stroke(rect, Rounding::ZERO, stroke);
        });
    }
}
