#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::convert::{From, Into};
use std::marker::Copy;
use std::sync::mpsc::channel;
use std::thread;

use eframe::egui;
use serde::{Deserialize, Serialize};
use websocket::client::ClientBuilder;
use websocket::{Message, OwnedMessage};

fn main() -> Result<(), eframe::Error> {
    let client = ClientBuilder::new("ws://127.0.0.1:13330")
		.unwrap()
		.connect_insecure()
		.unwrap();

    let (mut receiver, mut sender) = client.split().unwrap();

	let (tx, rx) = channel();

	let tx_1 = tx.clone();

	let send_loop = thread::spawn(move || {
		loop {
			// Send loop
			let message = match rx.recv() {
				Ok(m) => m,
				Err(e) => {
					println!("Send Loop: {:?}", e);
					return;
				}
			};
			match message {
				OwnedMessage::Close(_) => {
					let _ = sender.send_message(&message);
					// If it's a close message, just send it and then return.
					return;
				}
				_ => (),
			}
			// Send the message
			match sender.send_message(&message) {
				Ok(()) => (),
				Err(e) => {
					println!("Send Loop: {:?}", e);
					let _ = sender.send_message(&Message::close());
					return;
				}
			}
		}
	});

	let _receive_loop = thread::spawn(move || {
		// Receive loop
		for message in receiver.incoming_messages() {
			let message = match message {
				Ok(m) => m,
				Err(e) => {
					println!("Receive Loop: {:?}", e);
					let _ = tx_1.send(OwnedMessage::Close(None));
					return;
				}
			};
			match message {
				OwnedMessage::Close(_) => {
					// Got a close message, so send a close message and return
					let _ = tx_1.send(OwnedMessage::Close(None));
					return;
				}
				OwnedMessage::Ping(data) => {
					match tx_1.send(OwnedMessage::Pong(data)) {
						// Send a pong in response
						Ok(()) => (),
						Err(e) => {
							println!("Receive Loop: {:?}", e);
							return;
						}
					}
				}
				// Say what we received
				_ => println!("Receive Loop: {:?}", message),
			}
		}
	});

    // sender.send(OwnedMessage::Text("(:set-viewport 0 0 800 600)"));
    
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([800.0, 600.0]),
        ..Default::default()
    };

    let result = eframe::run_native(
        "Mesha",
        options,
        Box::new(|_cc| {
            Box::<App>::default()
        }),
    );

    let _ = tx.send(OwnedMessage::Close(None));
    result
}

struct App {
    current_selection: usize,
    current_edit: Option<usize>,
    cells: lru::LruCache<usize, Cell>,
}

#[derive(Serialize, Deserialize, Copy, Clone)]
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

#[derive(Serialize, Deserialize)]
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

const ACCEPT_CELL_EDIT: egui::KeyboardShortcut = egui::KeyboardShortcut::new(egui::Modifiers::NONE,
                                                                             egui::Key::Enter);

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

            if ui.interact_with_hovered(rect,
                                        true,
                                        Id::new(format!("Cell: {0}",
                                                        self.current_selection)),
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
