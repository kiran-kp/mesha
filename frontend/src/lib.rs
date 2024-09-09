use iced::Application;
use std::ops::DerefMut;

static CONNECTION: std::sync::Mutex<Option<MeshaConnection>> = std::sync::Mutex::new(None);

#[derive(Debug)]
struct LispMessage;

struct MeshaConnection {
    from_lisp: std::sync::mpsc::Sender<LispMessage>,
    to_lisp: std::sync::mpsc::Receiver<LispMessage>
}

#[no_mangle]
pub extern "C" fn mesha_initialize_gui() {
    
    let (from_lisp_sender, from_lisp_receiver) = std::sync::mpsc::channel();
    let (to_lisp_sender, to_lisp_receiver) = std::sync::mpsc::channel();

    CONNECTION.lock().unwrap().insert(MeshaConnection {
        to_lisp: to_lisp_receiver,
        from_lisp: from_lisp_sender
    });

    let _ = Mesha::run(iced::Settings {
        window: iced::window::Settings {
            exit_on_close_request: false,
            ..Default::default()
        },
        flags: Some(InitFlags {
            to_lisp: to_lisp_sender,
        }),
        ..Default::default()
    });
}

#[no_mangle]
pub extern "C" fn mesha_check_messages() {
    let mut guard = CONNECTION.lock().unwrap();
    let conn = guard.deref_mut().as_mut().unwrap();
    if let Ok(msg) = conn.to_lisp.try_recv() {
        println!("Got message: {:?}", msg);
    }
}

struct InitFlags {
    to_lisp: std::sync::mpsc::Sender<LispMessage>,
}

struct Mesha {
    to_lisp: std::sync::mpsc::Sender<LispMessage>,
    last: Vec<iced::event::Event>,
    enabled: bool,
}

#[derive(Debug, Clone)]
enum Message {
    EventOccurred(iced::event::Event),
    Toggled(bool),
    Exit,
}

impl Application for Mesha {
    type Message = Message;
    type Theme = iced::Theme;
    type Executor = iced::executor::Default;
    type Flags = Option<InitFlags>;

    fn new(flags: Option<InitFlags>) -> (Mesha, iced::Command<Message>) {
        let f = flags.unwrap();
        let m = Mesha {
            to_lisp: f.to_lisp,
            from_lisp: f.from_lisp,
            last: Vec::new(),
            enabled: false
        };
        
        (m, iced::Command::none())
    }

    fn title(&self) -> String {
        String::from("Mesha")
    }

    fn update(&mut self, message: Message) -> iced::Command<Message> {
        match message {
            Message::EventOccurred(event) if self.enabled => {
                self.last.push(event);

                if self.last.len() > 5 {
                    let _ = self.last.remove(0);
                }

                iced::Command::none()
            }
            Message::EventOccurred(event) => {
                if let iced::event::Event::Window(id, iced::window::Event::CloseRequested) = event
                {
                    iced::window::close(id)
                } else {
                    iced::Command::none()
                }
            }
            Message::Toggled(enabled) => {
                self.enabled = enabled;

                iced::Command::none()
            }
            Message::Exit => iced::window::close(iced::window::Id::MAIN),
        }
    }

    fn subscription(&self) -> iced::Subscription<Message> {
        iced::Subscription::batch(
            vec![iced::event::listen().map(Message::EventOccurred),
                 iced::subscription::channel(
                     std::any::TypeId::of::<Connect>(),
                     100,
                     |mut output| async move {
                         
                     }
                 )
            ]
        )
    }

    fn view(&self) -> iced::Element<Message> {
        let events = iced::widget::Column::with_children(
            self.last
                .iter()
                .map(|event| iced::widget::text(format!("{event:?}")).size(20))
                .map(iced::Element::from),
        );

        let toggle = iced::widget::checkbox("Listen to runtime events", self.enabled)
            .on_toggle(Message::Toggled);

        let exit = iced::widget::button(
            iced::widget::text("Exit")
                .width(iced::Length::Fill)
                .horizontal_alignment(iced::alignment::Horizontal::Center),
        )
        .width(100)
        .padding(10)
        .on_press(Message::Exit);

        let content = iced::widget::Column::new()
            .align_items(iced::Alignment::Start)
            .spacing(20)
            .push(events)
            .push(toggle)
            .push(exit);

        iced::widget::container(content)
            .width(iced::Length::Fill)
            .height(iced::Length::Fill)
            .into()
    }
}
