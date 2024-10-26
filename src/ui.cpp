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

static auto read_byte(uint8_t *bytes) -> std::pair<uint8_t, uint8_t*> {
    return std::make_pair(*bytes, bytes + 1);
}

static auto read_int32(uint8_t *bytes) -> std::pair<int32_t, uint8_t*> {
    return std::make_pair(bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24), bytes + 4);
}

static auto read_string(uint8_t *bytes) -> std::pair<std::string_view, uint8_t*> {
    int32_t length;
    std::tie(length, bytes) = read_int32(bytes);
    return std::make_pair(std::string_view((char*)bytes, length + 1), bytes + length + 1);
}

auto mesha_ui_begin_window(uint8_t *bytes) -> uint8_t* {
    int32_t num_properties = 0;
    std::tie(num_properties, bytes) = read_int32(bytes);
    int32_t width = 100;
    int32_t height = 100;
    int32_t x = 100;
    int32_t y = 100;
    int32_t flags = 0;
    // Process properties
    for (uint8_t i = 0; i < num_properties; i++) {
        switch (*bytes) {
            case 0x00: // is-open
            {
                bytes += 1;
                int32_t is_open = 0;
                std::tie(is_open, bytes) = read_int32(bytes);
                break;
            }
            case 0x01: // flags
            {
                bytes += 1;
                std::tie(flags, bytes) = read_int32(bytes);
                break;
            }
            case 0x02: // width
            {
                bytes += 1;
                std::tie(width, bytes) = read_int32(bytes);
                break;
            }
            case 0x03: // height
            {
                bytes += 1;
                std::tie(height, bytes) = read_int32(bytes);
                break;
            }
            case 0x04: // x
            {
                bytes += 1;
                std::tie(x, bytes) = read_int32(bytes);
                break;
            }
            case 0x05: // y
            {
                bytes += 1;
                std::tie(y, bytes) = read_int32(bytes);
                break;
            }
            default:
                break;
        }
    }

    std::string_view title_str;
    std::tie(title_str, bytes) = read_string(bytes);

    ImGui::SetNextWindowSize(ImVec2(width, height));
    ImGui::SetNextWindowPos(ImVec2(x, y), ImGuiCond_Once);

    ImGui::Begin(title_str.data(), nullptr, flags);
    return bytes;
}

auto mesha_ui_end_window() -> void {
    ImGui::End();
}

auto mesha_ui_text(uint8_t *bytes) -> uint8_t* {
    std::string_view text_str;
    std::tie(text_str, bytes) = read_string(bytes);
    int32_t num_args;
    std::tie(num_args, bytes) = read_int32(bytes);
    ImGui::Text("%s", text_str.data());
    return bytes;
}

auto mesha_ui_checkbox(uint8_t *bytes) -> uint8_t* {
    std::string_view label_str;
    std::tie(label_str, bytes) = read_string(bytes);
    int32_t value;
    std::tie(value, bytes) = read_int32(bytes);
    ImGui::Checkbox(label_str.data(), (bool*)&value);
    return bytes;
}

auto mesha_ui_process_view(Ui &ui, uint8_t *bytes) -> void {
    int32_t view_length;
    uint8_t *start = bytes;
    std::tie(view_length, bytes) = read_int32(bytes);
    uint8_t *end = bytes + view_length;
    while (bytes < end) {
        uint8_t op;
        std::tie(op, bytes) = read_byte(bytes);
        switch (op) {
            case 0x00:
                bytes = mesha_ui_begin_window(bytes);
                break;
            case 0x01:
                bytes = mesha_ui_text(bytes);
                break;
            case 0x02:
                bytes = mesha_ui_checkbox(bytes);
                break;
            case 0xFF:
                mesha_ui_end_window();
                break;
            default:
                printf("Unknown opcode: %d\n", op);
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