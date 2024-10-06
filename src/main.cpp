#include <ui.h>
#include <vm.h>

#include <imgui.h>

#include <thread>

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
