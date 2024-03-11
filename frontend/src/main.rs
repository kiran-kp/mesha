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
    rects: Vec<egui::Rect>,
    cells: lru::LruCache<usize, String>,
}

impl Default for App {
    fn default() -> Self {
        use egui::*;
        use lru::LruCache;
        use std::num::NonZeroUsize;

        let mut cells = LruCache::new(NonZeroUsize::new(25).unwrap());
        cells.put(1, "Test".to_owned());
        cells.put(2, "123".to_owned());
        cells.put(3, "Parent".to_owned());
        cells.put(4, "1337".to_owned());
        cells.put(5, "Kiran".to_owned());
        cells.put(6, "List".to_owned());
        cells.put(7, "1".to_owned());
        cells.put(8, "2".to_owned());
        cells.put(9, "3".to_owned());
        cells.put(10, "4".to_owned());

        Self {
            rects: vec![Rect::from_two_pos(pos2(200.0, 50.0), pos2(600.0, 150.0)),
                        Rect::from_two_pos(pos2(200.0, 150.0), pos2(600.0, 250.0)),
                        Rect::from_two_pos(pos2(200.0, 250.0), pos2(400.0, 350.0)),
                        Rect::from_two_pos(pos2(400.0, 250.0), pos2(600.0, 300.0)),
                        Rect::from_two_pos(pos2(400.0, 300.0), pos2(600.0, 350.0)),
                        Rect::from_two_pos(pos2(200.0, 350.0), pos2(400.0, 450.0)),
                        Rect::from_two_pos(pos2(400.0, 350.0), pos2(450.0, 450.0)),
                        Rect::from_two_pos(pos2(450.0, 350.0), pos2(500.0, 450.0)),
                        Rect::from_two_pos(pos2(500.0, 350.0), pos2(550.0, 450.0)),
                        Rect::from_two_pos(pos2(550.0, 350.0), pos2(600.0, 450.0)),
            ],
            cells,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        use egui::*;

        CentralPanel::default().show(ctx, |ui| {
            let texts = &self.cells;

            {
                let stroke = Stroke::new(3.0, Color32::WHITE);

                for rect in &self.rects {
                    let p = ui.painter_at(*rect);
                    p.rect_stroke(*rect, Rounding::ZERO, stroke);
                }
            }

            {
                for (rect, (_key, text)) in std::iter::zip(&self.rects, texts) {
                    let p = ui.painter_at(*rect);
                    p.text(rect.shrink(10.0).left_top(), Align2::LEFT_TOP, text, TextStyle::Heading.resolve(&ctx.style()), Color32::WHITE);
                    // ui.put(*rect, egui::Label::new(text));
                }
            }
        });
    }
}
