#include <readerwriterqueue.h>

#include <memory>
#include <vector>

struct Command {
    enum Type {
        Invalid,
        InitUi,
        CreateView,
        Quit
    } type;

    union {
        struct {
            uint8_t *bytecode;
        } create_view;

        struct {
            bool had_error = false;
        } quit;
    };

    Command() : type(Invalid) {
    }

    static auto init_ui_cmd() -> Command {
        Command cmd;
        cmd.type = InitUi;
        return cmd;
    }

    static auto create_view_cmd(uint8_t *bytes) -> Command {
        Command cmd;
        cmd.type = CreateView;
        cmd.create_view.bytecode = bytes;
        return cmd;
    }

    static auto quit_cmd(bool had_error) -> Command {
        Command cmd;
        cmd.type = Quit;
        cmd.quit.had_error = had_error;
        return cmd;
    }
};

struct Message {
    enum Type {
        Invalid,
        UiReady
    } type;

    union {
    };
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