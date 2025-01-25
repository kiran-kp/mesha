#pragma once

#include <vm.h>

#include <memory>
#include <vector>

struct Ui_impl;

enum class WindowState {
    Open,
    Minimized
};

struct Ui {
    std::unique_ptr<Ui_impl> impl;
    bool should_quit;
    bool is_initialized;
    Ui();
    ~Ui();
};

auto mesha_ui_init(Ui &ui) -> bool;
auto mesha_ui_shutdown(Ui &ui) -> void;

auto mesha_ui_begin_frame(Ui &ui) -> bool;
auto mesha_ui_end_frame(Ui &ui) -> void;

auto mesha_ui_get_window_size(Ui &ui) -> std::pair<int32_t, int32_t>;

auto mesha_ui_document() -> void;

auto mesha_ui_process_views(Ui &ui, std::vector<UiMessage>& messages) -> void;
auto mesha_ui_push_view(Ui &ui, const uint8_t *id, uint8_t *bytes) -> void;