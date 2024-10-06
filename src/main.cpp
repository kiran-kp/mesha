#include <ui.h>

#include <imgui.h>
#include <janet.h>
#include <readerwriterqueue.h>

#include <fstream>
#include <string>
#include <thread>

auto slurp(std::string_view path) -> std::string {
    constexpr auto read_size = std::size_t(4096);
    auto stream = std::ifstream(path.data());
    stream.exceptions(std::ios_base::badbit);

    if (!stream) {
        throw std::ios_base::failure("File does not exist");
    }

    auto out = std::string();
    auto buf = std::string(read_size, '\0');
    while (stream.read(& buf[0], read_size)) {
        out.append(buf, 0, stream.gcount());
    }

    out.append(buf, 0, stream.gcount());
    return out;
}

struct Command {
    enum Type {
        Invalid,
        InitUi,
        Quit
    } type;

    union {
        struct {
            bool had_error = false;
        } quit;
    };

    Command() : type(Invalid) {
    }

    static auto init_ui_cmd() -> Command {
        Command cmd;
        cmd.type = InitUi;
        return cmd;
    }

    static auto quit_cmd(bool had_error) -> Command {
        Command cmd;
        cmd.type = Quit;
        cmd.quit.had_error = had_error;
        return cmd;
    }
};

struct Message {
    enum Type {
        Invalid,
        UiReady
    } type;

    union {
    };
};

using CommandQueue = moodycamel::BlockingReaderWriterQueue<Command>;
using MessageQueue = moodycamel::BlockingReaderWriterQueue<Message>;

struct Vm {
    JanetTable *env = nullptr;
    JanetArray *args = nullptr;
    JanetFiber *main = nullptr;
    CommandQueue *command_queue = nullptr;
    MessageQueue *message_queue = nullptr;
};

thread_local Vm *the_vm;

struct VmInitArgs {
    int argc;
    char **argv;
    CommandQueue *command_queue;
    MessageQueue *message_queue;
};

static Janet enqueue_command(int32_t argc, Janet *argv) {
    janet_arity(argc, 1, 6);
    bool handled = false;
    JanetKeyword command = janet_getkeyword(argv, 0);
    if (command == janet_ckeyword("init-ui")) {
        the_vm->command_queue->enqueue(Command::init_ui_cmd());
        handled = true;
    } else if (command == janet_ckeyword("quit")) {
        the_vm->command_queue->enqueue(Command::quit_cmd(false));
        handled = true;
    }

    return handled ? janet_wrap_true() : janet_wrap_false();
}

auto mesha_vm_init(Vm &vm, const VmInitArgs &args) {
    vm.command_queue = args.command_queue;
    vm.message_queue = args.message_queue;

    janet_init();

    vm.env = janet_core_env(NULL);

    // Save current executable path to (dyn :executable)
    janet_table_put(vm.env, janet_ckeywordv("executable"), janet_cstringv(args.argv[0]));

    // Create args tuple
    vm.args = janet_array(args.argc);
    for (int i = 1; i < args.argc; i++) {
        janet_array_push(vm.args, janet_cstringv(args.argv[i]));
    }

    JanetReg cfuns[] = {
        {"enqueue-command",
         enqueue_command,
         "(enqueue-command cmd)\n\nEnqueues a command and returns true if it was successfully enqueued."},
        {NULL, NULL, NULL}
    };

    janet_cfuns(vm.env, "native", cfuns);

    // Load boot script
    auto boot_file = slurp("../src/mesha.janet");
    Janet output;
    janet_dobytes(vm.env,
                  reinterpret_cast<const uint8_t*>(boot_file.c_str()),
                  static_cast<int32_t>(boot_file.length()),
                  "../src/mesha.janet",
                  &output);

    the_vm = &vm;
}

auto mesha_vm_shutdown(Vm &vm) {
    janet_deinit();
}

auto mesha_vm_run_main(Vm &vm) {
    JanetTable *env = vm.env;

    // Run startup script
    Janet mainfun;
    janet_resolve(env, janet_csymbol("main"), &mainfun);
    Janet mainargs[1] = { janet_wrap_array(vm.args) };
    JanetFiber *fiber = janet_fiber(janet_unwrap_function(mainfun), 64, 1, mainargs);
    janet_gcroot(janet_wrap_fiber(fiber));
    fiber->env = env;

    // Run the fiber in an event loop
    auto status = janet_loop_fiber(fiber);
}

auto script_thread_fn(const VmInitArgs &args) {
    Vm vm;
    mesha_vm_init(vm, args);
    mesha_vm_run_main(vm);
    mesha_vm_shutdown(vm);
}

auto show_main_menu_bar() {
    if (ImGui::BeginMainMenuBar()) {
        if (ImGui::BeginMenu("File")) {
            if (ImGui::MenuItem("Exit", "Alt+F4")) { /* Do something */ }
            ImGui::EndMenu();
        }
        
        if (ImGui::BeginMenu("Help")) {
            if (ImGui::MenuItem("About")) { /* Do something */ }
            ImGui::EndMenu();
        }
        
        ImGui::EndMainMenuBar();
    }
}

auto main(int argc, char **argv) -> int {
    CommandQueue command_queue;
    MessageQueue message_queue;

    VmInitArgs args;
    args.argc = argc;
    args.argv = argv;
    args.command_queue = &command_queue;
    args.message_queue = &message_queue;

    std::thread script_thread(script_thread_fn, args);

    Ui ui;
    bool show_demo_window = true;
    bool show_another_window = false;
    bool is_window_open = true;
    bool should_quit = false;

    while (!should_quit && !ui.should_quit) {
        // Process commands
        Command cmd;
        if (command_queue.wait_dequeue_timed(cmd, 
                                             std::chrono::milliseconds(10))) {
            switch (cmd.type) {
                case Command::InitUi:
                    mesha_ui_init(ui);
                    break;
                case Command::Quit:
                    should_quit = true;
                    break;
                case Command::Invalid:
                    break;
            }
        }

        // Update UI
        if (ui.is_initialized) {
            mesha_ui_begin_frame(ui);
            ImGui::SetNextWindowSize(ImVec2(1280, 720));
            ImGui::SetNextWindowPos(ImVec2(0, 0));
            auto flags = ImGuiWindowFlags_NoResize |
                         ImGuiWindowFlags_NoTitleBar |
                         ImGuiWindowFlags_MenuBar |
                         ImGuiWindowFlags_NoMove |
                         ImGuiWindowFlags_NoBringToFrontOnFocus;
            if (ImGui::Begin("Mesha", &is_window_open, flags)) {
                static float f = 0.0f;
                static int counter = 0;

                show_main_menu_bar();

                ImGui::Text("This is some useful text.");               // Display some text (you can use a format strings too)
                ImGui::Checkbox("Demo Window", &show_demo_window);      // Edit bools storing our window open/close state
                ImGui::Checkbox("Another Window", &show_another_window);

                ImGui::SliderFloat("float", &f, 0.0f, 1.0f);            // Edit 1 float using a slider from 0.0f to 1.0f

                if (ImGui::Button("Button")) {
                    counter++;
                }

                ImGui::SameLine();
                ImGui::Text("counter = %d", counter);

            }

            ImGui::End();

            if (!is_window_open) {
                break;
            }

            if (show_another_window) {
                if (ImGui::Begin("Another Window", &show_another_window)) {
                    ImGui::Text("Hello from another window!");
                    if (ImGui::Button("Close Me")) {
                        show_another_window = false;
                    }
                }

                ImGui::End();
            }

            mesha_ui_end_frame(ui);
        }
    }

    script_thread.join();
    return 0;
}
