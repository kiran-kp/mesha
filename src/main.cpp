#include <stdio.h>

#include <ui.h>

#include <SDL.h>

#include <imgui.h>
#include <imgui_impl_sdl2.h>
#include <imgui_impl_sdlrenderer2.h>

#include <janet.h>

int main(int, char**) {
    janet_init();
    JanetTable *env = janet_core_env(NULL);

    janet_dostring(env, "(print \"hello, world!\")", "main", NULL);

    Ui ui;
    if (mesha_ui_init(ui)) {
        bool show_demo_window = true;
        bool show_another_window = false;
        while (mesha_ui_begin_frame(ui) && !ui.should_quit) {
            if (ImGui::Begin("Hello, world!")) {
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

    janet_deinit();

    return 0;
}
