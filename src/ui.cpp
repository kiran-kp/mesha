#include <ui.h>

#include <imgui_impl_sdl2.h>
#include <imgui_impl_sdlrenderer2.h>
#include <SDL.h>

#include <cstdio>
#include <string_view>
#include <utility>
#include <vector>

struct Ui_impl {
    SDL_Window *window;
    SDL_Renderer *renderer;
    std::vector<uint8_t*> views;
};

Ui::Ui() = default;
Ui::~Ui() = default;

auto mesha_ui_init(Ui &ui) -> bool {
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER) != 0) {
        printf("Error: %s\n", SDL_GetError());
        return false;
    }

#ifdef SDL_HINT_IME_SHOW_UI
    SDL_SetHint(SDL_HINT_IME_SHOW_UI, "1");
#endif

    ui.impl.reset(new Ui_impl);
    ui.should_quit = false;

    // Create window with SDL_Renderer graphics context
    SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
    ui.impl->window = SDL_CreateWindow("Mesha", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, window_flags);
    if (ui.impl->window == nullptr) {
        printf("Error: SDL_CreateWindow(): %s\n", SDL_GetError());
        return false;
    }

    ui.impl->renderer = SDL_CreateRenderer(ui.impl->window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
    if (ui.impl->renderer == nullptr) {
        SDL_Log("Error creating SDL_Renderer!");
        return false;
    }

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO();
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

    ImGui::StyleColorsDark();
    // ImGui::StyleColorsLight();

    ImGui_ImplSDL2_InitForSDLRenderer(ui.impl->window, ui.impl->renderer);
    ImGui_ImplSDLRenderer2_Init(ui.impl->renderer);

    ui.is_initialized = true;

    return true;
}

auto mesha_ui_shutdown(Ui &ui) -> void {
    ImGui_ImplSDLRenderer2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_DestroyRenderer(ui.impl->renderer);
    SDL_DestroyWindow(ui.impl->window);
    SDL_Quit();

    ui.impl.reset(nullptr);
}

auto mesha_ui_begin_frame(Ui &ui) -> bool {
    SDL_Event event;
    bool done = false;
    auto window = ui.impl->window;
    while (SDL_PollEvent(&event)) {
        ImGui_ImplSDL2_ProcessEvent(&event);
        if (event.type == SDL_QUIT) {
            done = true;
        }

        if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE && event.window.windowID == SDL_GetWindowID(window)) {
            done = true;
        }
    }

    if (SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED) {
        SDL_Delay(10);
        return false;
    }

    ui.should_quit = done;

    ImGui_ImplSDLRenderer2_NewFrame();
    ImGui_ImplSDL2_NewFrame();
    ImGui::NewFrame();

    return !done;
}

auto mesha_ui_end_frame(Ui &ui) -> void {
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    ImGuiIO& io = ImGui::GetIO();

    auto renderer = ui.impl->renderer;

    ImGui::Render();
    SDL_RenderSetScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);
    SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
    SDL_RenderClear(renderer);
    ImGui_ImplSDLRenderer2_RenderDrawData(ImGui::GetDrawData(), renderer);
    SDL_RenderPresent(renderer);
}

auto mesha_ui_get_window_size(Ui &ui) -> std::pair<int, int> {
    int width, height;
    SDL_GetWindowSize(ui.impl->window, &width, &height);
    return {width, height};
}

static auto read_int32(uint8_t *bytes) -> int32_t {
    return bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
}

auto mesha_ui_process_window(uint8_t *bytes) -> uint8_t* {
    int32_t num_properties = read_int32(bytes);
    uint8_t *op = bytes + 4;
    int32_t width = 100;
    int32_t height = 100;
    // Process properties
    for (uint8_t i = 0; i < num_properties; i++) {
        switch (*op) {
            case 0x00: // is-open
            {
                op += 1;
                int32_t is_open = read_int32(op);
                op += 4;
                break;
            }
            case 0x01: // flags
            {
                op += 1;
                int32_t flags = read_int32(op);
                op += 4;
                break;
            }
            case 0x02: // width
            {
                op += 1;
                width = read_int32(op);
                op += 4;
                break;
            }
            case 0x03: // height
            {
                op += 1;
                height = read_int32(op);
                op += 4;
                break;
            }
            case 0x04: // x
            {
                op += 1;
                int32_t x = read_int32(op);
                op += 4;
                break;
            }
            case 0x05: // y
            {
                op += 1;
                int32_t y = read_int32(op);
                op += 4;
                break;
            }
            default:
                break;
        }
    }

    int32_t title_length = read_int32(op);
    op += 4;
    std::string_view title_str((char*)op, title_length + 1);

    ImGui::SetNextWindowSize(ImVec2(width, height));
    // ImGui::SetNextWindowPos(ImVec2(200, 100));

    ImGui::Begin(title_str.data());
    ImGui::Text("Hello, mesha!");
    ImGui::End();
    return op;
}

auto mesha_ui_process_view(Ui &ui, uint8_t *bytes) -> void {
    uint8_t *op = bytes;
    bool done = false;
    while (!done) {
        switch (*op) {
            case 0x00:
                op = mesha_ui_process_window(op + 1);
                done = true;
                break;
            case 1:
                printf("Create view\n");
                break;
            case 0xFF:
                done = true;
                break;
            default:
                printf("Unknown opcode: %d\n", *op);
                break;
        }
    }
}

auto mesha_ui_process_views(Ui &ui) -> void {
    for (auto view : ui.impl->views) {
        mesha_ui_process_view(ui, view);
    }
}

auto mesha_ui_create_view(Ui &ui, uint8_t *bytes) -> void {
    ui.impl->views.push_back(bytes);
}