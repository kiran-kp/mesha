#pragma once

#include <memory>

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

bool mesha_ui_init(Ui &ui);
void mesha_ui_shutdown(Ui &ui);

bool mesha_ui_begin_frame(Ui &ui);
void mesha_ui_end_frame(Ui &ui);

std::pair<int, int> mesha_ui_get_window_size(Ui &ui);