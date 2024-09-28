#include <ui.h>

#include <SDL.h>

#include <imgui.h>
#include <imgui_impl_sdl2.h>
#include <imgui_impl_sdlrenderer2.h>

#include <janet.h>

#include <fstream>
#include <string>

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

auto main(int argc, char **argv) -> int {
    janet_init();
    JanetTable *env = janet_core_env(NULL);
    JanetArray *args;

    /* Create args tuple */
    args = janet_array(argc);
    for (int i = 1; i < argc; i++) {
        janet_array_push(args, janet_cstringv(argv[i]));
    }

    /* Save current executable path to (dyn :executable) */
    janet_table_put(env, janet_ckeywordv("executable"), janet_cstringv(argv[0]));

    auto boot_file = slurp("../src/mesha.janet");
    Janet output;
    janet_dobytes(env,
                  reinterpret_cast<const uint8_t*>(boot_file.c_str()),
                  static_cast<int32_t>(boot_file.length()),
                  "../src/mesha.janet",
                  &output);

    /* Run startup script */
    Janet mainfun;
    janet_resolve(env, janet_csymbol("main"), &mainfun);
    Janet mainargs[1] = { janet_wrap_array(args) };
    JanetFiber *fiber = janet_fiber(janet_unwrap_function(mainfun), 64, 1, mainargs);
    janet_gcroot(janet_wrap_fiber(fiber));
    fiber->env = env;

    /* Run the fiber in an event loop */
    auto status = janet_loop_fiber(fiber);

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
