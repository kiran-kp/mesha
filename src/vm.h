#include <janet.h>
#include <readerwriterqueue.h>

struct Command {
    enum Type {
        Invalid,
        InitUi,
        Quit
    } type;

    union {
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

struct Vm {
    JanetTable *env = nullptr;
    JanetArray *args = nullptr;
    JanetFiber *main = nullptr;
    CommandQueue *command_queue = nullptr;
    MessageQueue *message_queue = nullptr;
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