#pragma once
#include <readerwriterqueue.h>

#include <memory>
#include <vector>

struct Command {
    enum class Type {
        Invalid,
        InitUi,
        PushView,
        Quit
    } type;

    union {
        struct {
            const uint8_t *id;
            uint8_t *bytecode;
        } push_view;

        struct {
            bool had_error = false;
        } quit;
    };

    Command() : type(Type::Invalid) {
    }

    static auto init_ui_cmd() -> Command {
        Command cmd;
        cmd.type = Type::InitUi;
        return cmd;
    }

    static auto push_view_cmd(const uint8_t *id, uint8_t *bytes) -> Command {
        Command cmd;
        cmd.type = Type::PushView;
        cmd.push_view.id = id;
        cmd.push_view.bytecode = bytes;
        return cmd;
    }

    static auto quit_cmd(bool had_error) -> Command {
        Command cmd;
        cmd.type = Type::Quit;
        cmd.quit.had_error = had_error;
        return cmd;
    }
};

struct UiMessageKey {
    uint8_t *data = nullptr;
};

struct UiMessagePayload {
    enum class Type {
        Invalid,
        Integer
    };

    size_t size = 0;
    uint8_t *data = nullptr;
};

struct UiMessage {
    UiMessageKey key;
    UiMessagePayload payload;
};

struct Message {
    enum class Type : int32_t {
        Invalid,
        UiReady,
        UiMessage
    } type;

    union {
        UiMessage ui_message;
        intptr_t value;
    };

    Message() : type(Type::Invalid), value(0) {
    }

    static auto ui_ready_msg() -> Message {
        Message msg;
        msg.type = Type::UiReady;
        return msg;
    }

    static auto ui_message_msg(UiMessage&& ui_msg) -> Message {
        Message msg;
        msg.type = Type::UiMessage;
        msg.ui_message = std::move(ui_msg);
        return msg;
    }
};

using CommandQueue = moodycamel::BlockingReaderWriterQueue<Command>;
using MessageQueue = moodycamel::BlockingReaderWriterQueue<Message>;

struct VmImpl;

struct Vm {
    std::unique_ptr<VmImpl> impl;
    Vm();
    ~Vm();
};

struct VmInitArgs {
    int argc;
    char **argv;
    CommandQueue *command_queue;
    MessageQueue *message_queue;
};

auto mesha_vm_init(Vm &vm, const VmInitArgs &args) -> void;
auto mesha_vm_shutdown(Vm &vm) -> void;
auto mesha_vm_shutdown(Vm &vm) -> void;
auto mesha_vm_run_main(Vm &vm) -> void;
