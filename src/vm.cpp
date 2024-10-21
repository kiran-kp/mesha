#include <vm.h>

#include <janet.h>

#include <fstream>
#include <string>

struct VmImpl {
    JanetTable *env;
    JanetArray *args;
    CommandQueue *command_queue;
    MessageQueue *message_queue;
};

Vm::Vm() = default;
Vm::~Vm() = default;

thread_local Vm *the_vm;

static auto slurp(std::string_view path) -> std::string {
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

static auto enqueue_command(int32_t argc, Janet *argv) -> Janet {
    janet_arity(argc, 1, 6);
    bool handled = false;
    auto impl = the_vm->impl.get();
    JanetKeyword command = janet_getkeyword(argv, 0);
    if (command == janet_ckeyword("init-ui")) {
        impl->command_queue->enqueue(Command::init_ui_cmd());
        handled = true;
    } else if (command == janet_ckeyword("create-view")) {
        JanetBuffer *buffer = janet_getbuffer(argv, 1);
        auto bytes = new uint8_t[buffer->count];
        memcpy(bytes, buffer->data, buffer->count);
        impl->command_queue->enqueue(Command::create_view_cmd(bytes));
        handled = true;
    } else if (command == janet_ckeyword("quit")) {
        impl->command_queue->enqueue(Command::quit_cmd(false));
        handled = true;
    }

    return handled ? janet_wrap_true() : janet_wrap_false();
}

auto mesha_vm_init(Vm &vm, const VmInitArgs &args) -> void {
    vm.impl = std::make_unique<VmImpl>();
    auto impl = vm.impl.get();
    impl->command_queue = args.command_queue;
    impl->message_queue = args.message_queue;

    janet_init();

    impl->env = janet_core_env(NULL);

    // Save current executable path to (dyn :executable)
    janet_table_put(impl->env, janet_ckeywordv("executable"), janet_cstringv(args.argv[0]));

    // Create args tuple
    impl->args = janet_array(args.argc);
    for (int i = 1; i < args.argc; i++) {
        janet_array_push(impl->args, janet_cstringv(args.argv[i]));
    }

    JanetReg cfuns[] = {
        {"enqueue-command",
         enqueue_command,
         "(enqueue-command cmd)\n\nEnqueues a command and returns true if it was successfully enqueued."},
        {NULL, NULL, NULL}
    };

    janet_cfuns(impl->env, "native", cfuns);

    // Load boot script
    auto boot_file = slurp("../src/mesha.janet");
    Janet output;
    janet_dobytes(impl->env,
                  reinterpret_cast<const uint8_t*>(boot_file.c_str()),
                  static_cast<int32_t>(boot_file.length()),
                  "../src/mesha.janet",
                  &output);

    the_vm = &vm;
}

auto mesha_vm_shutdown(Vm &vm) -> void {
    janet_deinit();
}

auto mesha_vm_run_main(Vm &vm) -> void {
    auto impl = vm.impl.get();
    JanetTable *env = impl->env;

    // Run startup script
    Janet mainfun;
    janet_resolve(env, janet_csymbol("main"), &mainfun);
    Janet mainargs[1] = { janet_wrap_array(impl->args) };
    JanetFiber *fiber = janet_fiber(janet_unwrap_function(mainfun), 64, 1, mainargs);
    janet_gcroot(janet_wrap_fiber(fiber));
    fiber->env = env;

    // Run the fiber in an event loop
    auto status = janet_loop_fiber(fiber);
}

