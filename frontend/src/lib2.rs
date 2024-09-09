#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::convert::{From, Into};
use std::marker::Copy;

use eframe::egui;

#[no_mangle]
pub extern "C" fn mesha_initialize_gui() { 
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([800.0, 600.0])
            .with_decorations(true),
        run_and_return: false,
        ..Default::default()
    };

    {
        let _result = eframe::run_native(
            "Mesha",
            options,
            Box::new(|_cc| {
                Ok(Box::<App>::default())
            }),
        );
    }

    println!("Done with gui");
}

struct App {
    current_selection: usize,
    current_edit: Option<usize>,
    cells: lru::LruCache<usize, Cell>,
}

#[derive(Copy, Clone)]
struct Rect {
    x0: f32,
    y0: f32,
    x1: f32,
    y1: f32
}

impl From<egui::Rect> for Rect {
    fn from(r: egui::Rect) -> Rect {
        let x0y0 = r.left_top();
        let x1y1 = r.right_bottom();
        Rect {
            x0: x0y0.x,
            y0: x0y0.y,
            x1: x1y1.x,
            y1: x1y1.y,            
        }
    }
}

impl Into<egui::Rect> for Rect {
    fn into(self) -> egui::Rect {
        egui::Rect::from_two_pos(egui::pos2(self.x0, self.y0), egui::pos2(self.x1, self.y1))
    }
}

struct Cell {
    rect: Rect,
    content: String
}

impl Default for App {
    fn default() -> Self {
        use egui::*;
        use lru::LruCache;
        use std::num::NonZeroUsize;

        let test_data = vec![(Rect::from_two_pos(pos2(200.0, 50.0), pos2(600.0, 150.0)), "Test\n1"),
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
            cells.put(i + 1, Cell { rect: c.0.into(), content: c.1.to_owned() });
        }

        Self {
            current_selection: 1,
            current_edit: None,
            cells
        }
    }
}

const ACCEPT_CELL_EDIT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(egui::Modifiers::NONE, egui::Key::Enter);

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        use egui::*;

        CentralPanel::default().show(ctx, |ui| {
            for (&key, cell) in &self.cells {
                let stroke = Stroke::new(3.0, Color32::WHITE);
                let rect = cell.rect.into();
                if ui.rect_contains_pointer(rect) {
                    self.current_selection = key;
                }
                let text = &cell.content;
                let p = ui.painter_at(rect);
                p.rect_stroke(rect, Rounding::ZERO, stroke);
                if self.current_edit.is_none() || self.current_edit.unwrap() != key {
                    p.text(rect.shrink(10.0).
                           left_top(),
                           Align2::LEFT_TOP,
                           text,
                           TextStyle::Heading.resolve(&ctx.style()),
                           Color32::WHITE);
                }
            }

            let stroke = Stroke::new(3.0, Color32::RED);
            let selected_cell = self.cells.get_mut(&self.current_selection).unwrap();
            let rect: egui::Rect = selected_cell.rect.into();
            {
                let p = ui.painter_at(rect.expand(2.0));
                p.rect_stroke(rect, Rounding::ZERO, stroke);
            }

            let s = Sense {
                click: true,
                drag: false,
                focusable: true
            };

            let mut should_set_focus = false;

            if ui.interact(rect,
                           Id::new(format!("Cell: {0}", self.current_selection)),
                           s).double_clicked() {
                self.current_edit = Some(self.current_selection);
                should_set_focus = true;
            }

            if let Some(k) = self.current_edit {
                if ui.input_mut(|i| i.consume_shortcut(&ACCEPT_CELL_EDIT)) {
                    self.current_edit = None;
                } else {
                    
                    let cell = self.cells.get_mut(&k).unwrap();
                    let text_edit = TextEdit::multiline(&mut cell.content);
                    let rect: egui::Rect = cell.rect.into();
                    let response = ui.put(rect.shrink(2.0), text_edit);
                    if should_set_focus {
                        response.request_focus();
                    }

                    if response.lost_focus() {
                        self.current_edit = None;
                    }
                }
            }
        });
    }
}
