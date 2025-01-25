#pragma once

#include <vm.h>

#include <memory>
#include <vector>

enum class WindowState { Open, Minimized };

struct Ui {
    bool m_should_quit;
    bool m_is_initialized;

    Ui();
    ~Ui();

    auto init() -> bool;
    auto shutdown() -> void;

    auto begin_frame() -> bool;
    auto end_frame() -> void;

    auto get_window_size() -> std::pair<int32_t, int32_t>;

    auto document() -> void;

    auto process_views(std::vector<UiMessage> &messages) -> void;
    auto push_view(const uint8_t *id, uint8_t *bytes) -> void;

    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
