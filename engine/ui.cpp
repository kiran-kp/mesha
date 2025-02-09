#include <SDL3/SDL.h>
#include <imgui_impl_sdl3.h>
#include <imgui_impl_sdlrenderer3.h>
#include <ui.h>

#include <chrono>
#include <cstdio>
#include <format>
#include <iostream>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <utility>

using UiResult = std::pair<std::optional<UiMessage>, uint8_t *>;
using UiFunction = UiResult (*)(uint8_t *);

struct Ui::Impl {
    SDL_Window *window;
    SDL_Renderer *renderer;
    std::unordered_map<const uint8_t *, uint8_t *> views;
};

Ui::Ui() = default;
Ui::~Ui() = default;

auto Ui::init() -> bool {
    if (!SDL_Init(SDL_INIT_VIDEO)) {
        return false;
    }

    m_impl.reset(new Impl);
    m_should_quit = false;

    // Create window with SDL_Renderer graphics context
    SDL_WindowFlags window_flags =
        (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);

    if (!SDL_CreateWindowAndRenderer("Mesha", 800, 600, 0, &m_impl->window, &m_impl->renderer)) {
        return SDL_APP_FAILURE;
    }

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO &io = ImGui::GetIO();
    io.ConfigFlags |=
        ImGuiConfigFlags_NavEnableKeyboard;  // Enable Keyboard Controls
    io.ConfigFlags |=
        ImGuiConfigFlags_NavEnableGamepad;  // Enable Gamepad Controls

    ImGui::StyleColorsDark();
    // ImGui::StyleColorsLight();

    ImGui_ImplSDL3_InitForSDLRenderer(m_impl->window, m_impl->renderer);
    ImGui_ImplSDLRenderer3_Init(m_impl->renderer);

    m_is_initialized = true;

    return true;
}

auto Ui::shutdown() -> void {
    ImGui_ImplSDLRenderer3_Shutdown();
    ImGui_ImplSDL3_Shutdown();
    ImGui::DestroyContext();

    SDL_DestroyRenderer(m_impl->renderer);
    SDL_DestroyWindow(m_impl->window);
    SDL_Quit();

    m_impl.reset(nullptr);
}

auto Ui::begin_frame() -> bool {
    SDL_Event event;
    bool done = false;
    auto window = m_impl->window;
    while (SDL_PollEvent(&event)) {
        ImGui_ImplSDL3_ProcessEvent(&event);
        if (event.type == SDL_EVENT_QUIT) {
            done = true;
        }

        if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
            event.window.windowID == SDL_GetWindowID(window)) {
            done = true;
        }
    }

    if (SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED) {
        SDL_Delay(10);
        return false;
    }

    m_should_quit = done;

    ImGui_ImplSDLRenderer3_NewFrame();
    ImGui_ImplSDL3_NewFrame();
    ImGui::NewFrame();

    return !done;
}

auto Ui::end_frame() -> void {
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    ImGuiIO &io = ImGui::GetIO();

    auto renderer = m_impl->renderer;

    ImGui::Render();
    SDL_SetRenderScale(renderer, io.DisplayFramebufferScale.x,
                       io.DisplayFramebufferScale.y);
    SDL_SetRenderDrawColor(
        renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255),
        (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
    SDL_RenderClear(renderer);
    ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData(), renderer);
    SDL_RenderPresent(renderer);
}

auto Ui::get_window_size() -> std::pair<int, int> {
    int width, height;
    SDL_GetWindowSize(m_impl->window, &width, &height);
    return {width, height};
}

namespace {
static auto read_byte(uint8_t *bytes) -> std::pair<uint8_t, uint8_t *> {
    return std::make_pair(*bytes, bytes + 1);
}

static auto read_int32(uint8_t *bytes) -> std::pair<int32_t, uint8_t *> {
    return std::make_pair(
        bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24),
        bytes + 4);
}

static auto read_string(uint8_t *bytes)
    -> std::pair<std::string_view, uint8_t *> {
    int32_t length;
    std::tie(length, bytes) = read_int32(bytes);
    return std::make_pair(std::string_view((char *)bytes, length + 1),
                          bytes + length + 1);
}

static auto read_message_key(uint8_t *bytes)
    -> std::pair<UiMessageKey, uint8_t *> {
    int32_t length;
    uint8_t *orig_bytes = bytes;
    std::tie(length, bytes) = read_int32(bytes);
    UiMessageKey key;
    key.data = orig_bytes;
    return std::make_pair(key, bytes + length);
}

static auto write_byte(UiMessagePayload &msg, uint8_t byte) -> void {
    msg.data[msg.size] = byte;
    msg.size += 1;
}

static auto write_integer(UiMessagePayload &msg, int32_t value) -> void {
    write_byte(msg, (uint8_t)(value & 0xFF));
    write_byte(msg, (uint8_t)((value >> 8) & 0xFF));
    write_byte(msg, (uint8_t)((value >> 16) & 0xFF));
    write_byte(msg, (uint8_t)((value >> 24) & 0xFF));
}

static auto write_string(UiMessagePayload &msg, std::string_view str) -> void {
    write_integer(msg, str.size());
    for (auto c : str) {
        write_byte(msg, c);
    }
}

auto begin_window(uint8_t *bytes) -> UiResult {
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
            case 0x00:  // is-open
            {
                bytes += 1;
                int32_t is_open = 0;
                std::tie(is_open, bytes) = read_int32(bytes);
                break;
            }
            case 0x01:  // flags
            {
                bytes += 1;
                std::tie(flags, bytes) = read_int32(bytes);
                break;
            }
            case 0x02:  // width
            {
                bytes += 1;
                std::tie(width, bytes) = read_int32(bytes);
                break;
            }
            case 0x03:  // height
            {
                bytes += 1;
                std::tie(height, bytes) = read_int32(bytes);
                break;
            }
            case 0x04:  // x
            {
                bytes += 1;
                std::tie(x, bytes) = read_int32(bytes);
                break;
            }
            case 0x05:  // y
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
    return std::make_pair(std::nullopt, bytes);
}

auto end_window(uint8_t *bytes) -> UiResult {
    ImGui::End();
    return std::make_pair(std::nullopt, bytes);
}

auto text(uint8_t *bytes) -> UiResult {
    std::string_view text_str;
    std::tie(text_str, bytes) = read_string(bytes);

    int32_t num_args;
    std::tie(num_args, bytes) = read_int32(bytes);

    ImGui::Text("%s", text_str.data());

    return std::make_pair(std::nullopt, bytes);
}

auto checkbox(uint8_t *bytes) -> UiResult {
    std::string_view label_str;
    std::tie(label_str, bytes) = read_string(bytes);

    int32_t value;
    std::tie(value, bytes) = read_int32(bytes);
    assert(value == 0 || value == 1);

    bool has_changed = ImGui::Checkbox(label_str.data(), (bool *)&value);

    UiMessageKey key;
    std::tie(key, bytes) = read_message_key(bytes);

    UiResult result;
    result.first = std::nullopt;
    result.second = bytes;
    if (has_changed) {
        UiMessage message;
        message.key = key;
        message.payload.size = 0;
        message.payload.data = new uint8_t[sizeof(int32_t) * 3];
        write_integer(message.payload, 1);
        write_integer(message.payload,
                      static_cast<int32_t>(UiMessagePayload::Type::Integer));
        write_integer(message.payload, value);
        result.first = std::make_optional(message);
    }

    return result;
}

auto button(uint8_t *bytes) -> UiResult {
    std::string_view label_str;
    std::tie(label_str, bytes) = read_string(bytes);

    bool pressed = ImGui::Button(label_str.data());

    UiMessageKey key;
    std::tie(key, bytes) = read_message_key(bytes);

    UiResult result;
    result.first = std::nullopt;
    result.second = bytes;
    if (pressed) {
        UiMessage message;
        message.key = key;
        message.payload.size = 0;
        message.payload.data = new uint8_t[sizeof(int32_t)];
        write_integer(message.payload, 0);
        result.first = std::make_optional(message);
    }

    return result;
}

auto same_line(uint8_t *bytes) -> UiResult {
    ImGui::SameLine();
    return std::make_pair(std::nullopt, bytes);
}
}  // namespace

auto Ui::document() -> void {
    struct Node {
        std::string text;
        float height = 0.0f;
        int64_t timestamp = 1739140551;
    };

    struct Document {
        std::vector<Node> nodes;
    };

    static Document doc = {
        {{"Hello, world! 0"},
         {"Hello, World! 1"},
         {"This is some sample text."},
         {"This is some more sample text."},
         {"This is some longer sample text and it spans multiple lines.\nCheck "
          "out the second line of text.\nAnd a third."},
         {"This is the last sample text."},
         {"Some more lines of text.\nThis is also multiline text.\nAnd a third line."},
         {"This is another line of text."},
         {"Even more text"},
         {"This is the last line of text."},
         {"How about this really tall pice of text?\nThere are 5 lines here.\n"
          "This is the third line.\nThis is the fourth line.\nThis is the fifth line."},
         {"A little line of text."},
         {"Another line of text."},
         {"This is the last line of text."}}};

    const ImGuiIO &io = ImGui::GetIO();
    ImDrawList *draw_list = ImGui::GetForegroundDrawList();
    ImGui::SetNextWindowSize(ImVec2(io.DisplaySize.x, io.DisplaySize.y));
    ImGui::SetNextWindowPos(ImVec2(0, 0));

    auto flags = ImGuiWindowFlags_NoTitleBar |
        ImGuiWindowFlags_NoResize |
        ImGuiWindowFlags_NoMove |
        ImGuiWindowFlags_NoCollapse |
        ImGuiWindowFlags_NoBringToFrontOnFocus |
        ImGuiWindowFlags_NoNavFocus |
        ImGuiWindowFlags_NoDocking |
        ImGuiWindowFlags_NoNav |
        ImGuiWindowFlags_NoScrollbar;

    if (ImGui::Begin("Document", nullptr, flags)) {
        for (Node &node : doc.nodes) {
            float x = ImGui::GetCursorPosX();
            float y = ImGui::GetCursorPosY();

            ImGui::Dummy(ImVec2(io.DisplaySize.x, node.height));
            bool is_hovered = ImGui::IsItemHovered();

            ImGui::SetCursorPos(ImVec2(x, y));

            std::chrono::system_clock::time_point t{std::chrono::seconds{node.timestamp}};
            auto local_time = std::chrono::current_zone()->to_local(t);
            ImGui::TextColored(ImColor(155, 155, 155, 255), std::format("{:%Y-%m-%d %R}", local_time).c_str());

            ImGui::TextColored(is_hovered ? ImColor(255, 0, 0, 255)
                                          : ImColor(255, 255, 255, 255),
                               "%s", node.text.c_str());

            float end_y = ImGui::GetCursorPosY();
            constexpr float padding = 10.0f;
            node.height = (end_y - y) + padding;

            ImGui::Dummy(ImVec2(io.DisplaySize.x, padding));
            ImGui::Separator();
        }
    }

    ImGui::End();
}

auto process_view(Ui &ui, uint8_t *bytes,
                  std::vector<UiMessage> &messages) -> void {
    int32_t view_length;
    uint8_t *start = bytes;
    std::tie(view_length, bytes) = read_int32(bytes);
    uint8_t *end = bytes + view_length;
    while (bytes < end) {
        uint8_t op;
        std::tie(op, bytes) = read_byte(bytes);
        UiFunction ops[256] = {
            begin_window,  // 0x00
            text,          // 0x01
            checkbox,      // 0x02
            button,        // 0x03
            same_line,     // 0x04
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr,      nullptr, nullptr, nullptr, nullptr,
            end_window,  // 0xFF
        };

        if (ops[op] != nullptr) {
            UiResult r = ops[op](bytes);
            bytes = r.second;
            if (r.first.has_value()) {
                messages.push_back(r.first.value());
            }
        }
    }
}

auto Ui::process_views(std::vector<UiMessage> &messages) -> void {
    for (auto view : m_impl->views) {
        process_view(*this, view.second, messages);
    }
}

auto Ui::push_view(const uint8_t *id, uint8_t *bytes) -> void {
    auto &views = m_impl->views;
    if (views.find(id) != views.end()) {
        delete views[id];
    }

    m_impl->views.insert_or_assign(id, bytes);
}