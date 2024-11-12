#include <ui.h>
#include <vm.h>

#include <imgui.h>

#include <thread>
#include <vector>

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
    std::vector<UiMessage> messages;
    bool should_quit = false;

    while (!should_quit && !ui.should_quit) {
        // Process commands
        Command cmd;
        if (command_queue.wait_dequeue_timed(cmd, 
                                             std::chrono::milliseconds(10))) {
            switch (cmd.type) {
                case Command::Type::InitUi:
                    mesha_ui_init(ui);
                    message_queue.enqueue(Message::ui_ready_msg());
                    break;
                case Command::Type::PushView:
                    mesha_ui_push_view(ui, cmd.push_view.id, cmd.push_view.bytecode);
                    break;
                case Command::Type::Quit:
                    should_quit = true;
                    break;
                case Command::Type::Invalid:
                    break;
            }
        }

        // Update UI
        if (ui.is_initialized) {
            mesha_ui_begin_frame(ui);
            mesha_ui_process_views(ui, messages);

            for (auto& message : messages) {
                message_queue.enqueue(Message::ui_message_msg(std::move(message)));
            }

            messages.clear();

            mesha_ui_end_frame(ui);
        }
    }

    message_queue.enqueue(Message::quit_msg());
    script_thread.join();
    return 0;
}
