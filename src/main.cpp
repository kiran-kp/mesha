#include <ui.h>
#include <vm.h>

#include <imgui.h>

#include <thread>

auto script_thread_fn(const VmInitArgs &args) -> void {
    Vm vm;
    mesha_vm_init(vm, args);
    mesha_vm_run_main(vm);
    mesha_vm_shutdown(vm);
}

auto show_main_menu_bar() -> void {
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
                case Command::CreateView:
                    mesha_ui_create_view(ui, cmd.create_view.bytecode);
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
            auto [width, height] = mesha_ui_get_window_size(ui);
            /*
            ImGui::SetNextWindowSize(ImVec2(width, height));
            ImGui::SetNextWindowPos(ImVec2(0, 0));
            auto flags = ImGuiWindowFlags_NoResize |
                         ImGuiWindowFlags_NoTitleBar |
                         ImGuiWindowFlags_MenuBar |
                         ImGuiWindowFlags_NoMove |
                         ImGuiWindowFlags_NoBringToFrontOnFocus;
            if (ImGui::Begin("Mesha", nullptr, flags)) {
                show_main_menu_bar();
            }

            ImGui::End();
            */
            mesha_ui_process_views(ui);

            mesha_ui_end_frame(ui);
        }
    }

    script_thread.join();
    return 0;
}
