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
        InitUi
    };

    union {
    };

    Type type;
};

struct Message {
    enum Type {
        UiReady
    };

    union {
    };
};

using CommandQueue = moodycamel::ReaderWriterQueue<Command>;
using MessageQueue = moodycamel::ReaderWriterQueue<Message>;

struct Vm {
    JanetTable *env = nullptr;
    JanetArray *args = nullptr;
    JanetFiber *main = nullptr;
};

thread_local CommandQueue *command_queue;
thread_local MessageQueue *message_queue;

struct VmInitArgs {
    int argc;
    char **argv;
    CommandQueue *command_queue;
    MessageQueue *message_queue;
};

auto mesha_vm_init(Vm &vm, const VmInitArgs &args) {
    command_queue = args.command_queue;
    message_queue = args.message_queue;

    janet_init();

    vm.env = janet_core_env(NULL);

    // Save current executable path to (dyn :executable)
    janet_table_put(vm.env, janet_ckeywordv("executable"), janet_cstringv(args.argv[0]));
    // Create args tuple
    vm.args = janet_array(args.argc);
    for (int i = 1; i < args.argc; i++) {
        janet_array_push(vm.args, janet_cstringv(args.argv[i]));
    }


    // Load boot script
    auto boot_file = slurp("../src/mesha.janet");
    Janet output;
    janet_dobytes(vm.env,
                  reinterpret_cast<const uint8_t*>(boot_file.c_str()),
                  static_cast<int32_t>(boot_file.length()),
                  "../src/mesha.janet",
                  &output);
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

    if (mesha_ui_init(ui)) {
        bool show_demo_window = true;
        bool show_another_window = false;
        bool is_window_open = true;
        while (mesha_ui_begin_frame(ui) && !ui.should_quit) {
            ImGui::SetNextWindowSize(ImVec2(1280, 720));
            ImGui::SetNextWindowPos(ImVec2(0, 0));
            if (ImGui::Begin("Mesha", &is_window_open, ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoMove)) {
                static float f = 0.0f;
                static int counter = 0;

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

        mesha_ui_shutdown(ui);
    }

    script_thread.join();
    return 0;
}
