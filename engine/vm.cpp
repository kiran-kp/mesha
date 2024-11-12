#include <vm.h>

#include <janet.h>

#include <fstream>
#include <string>
#include <thread>

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
    printf("command: %s\n", command);
    if (command == janet_ckeyword("init-ui")) {
        impl->command_queue->enqueue(Command::init_ui_cmd());
        handled = true;
    } else if (command == janet_ckeyword("push-view")) {
        JanetKeyword id = janet_getkeyword(argv, 1);
        JanetBuffer *buffer = janet_getbuffer(argv, 2);
        auto bytes = new uint8_t[buffer->count];
        memcpy(bytes, buffer->data, buffer->count);
        impl->command_queue->enqueue(Command::push_view_cmd(id, bytes));
        handled = true;
    } else if (command == janet_ckeyword("quit")) {
        impl->command_queue->enqueue(Command::quit_cmd(false));
        handled = true;
    }

    return handled ? janet_wrap_true() : janet_wrap_false();
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

static auto read_message_key(const UiMessageKey &key) -> Janet {
    int32_t length = 0;
    uint8_t *bytes = key.data;
    std::tie(length, bytes) = read_int32(bytes);
    int32_t num_parts = 0;
    std::tie(num_parts, bytes) = read_int32(bytes);

    auto jmsg_key = janet_tuple_begin(num_parts);
    for (int i = 0; i < num_parts; i++) {
        std::string_view part;
        std::tie(part, bytes) = read_string(bytes);
        jmsg_key[i] = janet_ckeywordv(part.data());
    }
    janet_tuple_end(jmsg_key);

    return janet_wrap_tuple(jmsg_key);
}

static auto read_ui_message_payload_item(uint8_t *bytes) -> std::pair<Janet, uint8_t*> {
    int32_t type;
    std::tie(type, bytes) = read_int32(bytes);
    switch (static_cast<UiMessagePayload::Type>(type)) {
        case UiMessagePayload::Type::Integer: {
            int32_t value;
            std::tie(value, bytes) = read_int32(bytes);
            return std::make_pair(janet_wrap_integer(value), bytes);
        }
        default:
            assert(false);
            return std::make_pair(janet_wrap_nil(), bytes);
    }
}

static auto get_message(int32_t argc, Janet *argv) -> Janet {
    janet_fixarity(argc, 0);
    auto impl = the_vm->impl.get();
    auto messages = janet_array(8);
    Message msg;
    impl->message_queue->wait_dequeue(msg);
    Janet result;
    {
        switch (msg.type) {
            case Message::Type::UiReady: {
                result = janet_wrap_keyword(janet_ckeyword("ui-ready"));
                break;
            }
            case Message::Type::UiMessage: {
                auto jmsg_key = read_message_key(msg.ui_message.key);

                int32_t num_payload_items = 0;
                uint8_t *bytes = msg.ui_message.payload.data;
                std::tie(num_payload_items, bytes) = read_int32(bytes);

                auto jmsg = janet_tuple_begin(2);
                jmsg[0] = jmsg_key;
                if (num_payload_items > 0) {
                    auto jpayload = janet_tuple_begin(num_payload_items);

                    for  (int i = 0; i < num_payload_items; i++) {
                        Janet item;
                        std::tie(item, bytes) = read_ui_message_payload_item(bytes);
                        jpayload[i] = item;
                    }

                    auto tup = janet_tuple_end(jpayload);
                    jmsg[1] = janet_wrap_tuple(tup);
                } else {
                    jmsg[1] = janet_wrap_nil();
                }

                auto tup = janet_tuple_end(jmsg);
                delete msg.ui_message.payload.data;

                result = janet_wrap_tuple(tup);

                break;
            }
            case Message::Type::Quit: {
                result = janet_wrap_keyword(janet_ckeyword("quit"));
                break;
            }
            default:
                break;
        }
    }

    return result;
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
         {"get-message",
         get_message,
         "(get-message)\n\nBlocks till a message is received and returns the message."},
        {NULL, NULL, NULL}
    };

    janet_cfuns(impl->env, "native", cfuns);

    // Load boot script
    auto boot_file = slurp("application/mesha.janet");
    Janet output;
    janet_dobytes(impl->env,
                  reinterpret_cast<const uint8_t*>(boot_file.c_str()),
                  static_cast<int32_t>(boot_file.length()),
                  "application/mesha.janet",
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

