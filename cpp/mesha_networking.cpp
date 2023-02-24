// flatbuffers
#include <flatbuffers/flatbuffers.h>
#include <message_generated.h>

// sockpp
#include <sockpp/tcp_acceptor.h>
#include <sockpp/tcp_connector.h>

#include <vector>

extern "C" {
    void mesha_networking_init() {
        sockpp::initialize();
    }

    uintptr_t mesha_networking_create_server(const char *host, uint16_t port) {
        auto listener = new sockpp::tcp_acceptor(sockpp::inet_address(host, port));
        return reinterpret_cast<uintptr_t>(listener);
    }

    void mesha_networking_close_listener(uintptr_t listener_ptr) {
        auto listener = reinterpret_cast<sockpp::tcp_acceptor*>(listener_ptr);
        delete listener;
    }

    uintptr_t mesha_networking_accept_connection(uintptr_t listener_ptr) {
        auto listener = reinterpret_cast<sockpp::tcp_acceptor*>(listener_ptr);
        auto sock = listener->accept();
        auto sock_ptr = new sockpp::tcp_socket(std::move(sock));
        return reinterpret_cast<uintptr_t>(sock_ptr);
    }

    uintptr_t mesha_networking_connect(const char *host, uint16_t port) {
        auto connector = new sockpp::tcp_connector(sockpp::inet_address(host, port));
        return reinterpret_cast<uintptr_t>(connector);
    }

    void mesha_networking_close_connection(uintptr_t socket_ptr) {
        auto socket = reinterpret_cast<sockpp::tcp_connector*>(socket_ptr);
        delete socket;
    }

    void mesha_networking_send_c2sgreeting(uintptr_t socket_ptr, const char *secret) {
        flatbuffers::FlatBufferBuilder builder;
        auto secret_string = builder.CreateString(secret);
        auto message_data = Mesha::CreateC2SGreeting(builder, secret_string).Union();
        auto message_root = Mesha::CreateMessage(builder, Mesha::MessageType_C2SGreeting, message_data);
        builder.Finish(message_root);

        uint32_t num_bytes = builder.GetSize();
        auto socket = reinterpret_cast<sockpp::tcp_connector*>(socket_ptr);
        socket->write(&num_bytes, sizeof(uint32_t));
        socket->write(builder.GetBufferPointer(), num_bytes);
    }

    void mesha_networking_send_s2cgreeting(uintptr_t socket_ptr, const char *secret) {
        flatbuffers::FlatBufferBuilder builder;
        auto secret_string = builder.CreateString(secret);
        auto message_data = Mesha::CreateS2CGreetingResponse(builder, secret_string).Union();
        auto message_root = Mesha::CreateMessage(builder, Mesha::MessageType_S2CGreetingResponse, message_data);
        builder.Finish(message_root);

        uint32_t num_bytes = builder.GetSize();
        auto socket = reinterpret_cast<sockpp::tcp_connector*>(socket_ptr);
        socket->write(&num_bytes, sizeof(uint32_t));
        socket->write(builder.GetBufferPointer(), num_bytes);
    }

    uintptr_t mesha_networking_read_message(uintptr_t socket_ptr) {
        auto socket = reinterpret_cast<sockpp::tcp_connector*>(socket_ptr);
        uint32_t num_bytes = 0;
        socket->read(&num_bytes, sizeof(uint32_t));

        if (num_bytes) {
            uint8_t *data = new uint8_t[num_bytes];
            socket->read(data, num_bytes);
            return reinterpret_cast<uintptr_t>(data);
        }

        return 0;
    }

    void mesha_networking_free_message(uintptr_t message_ptr) {
        delete[] reinterpret_cast<uint8_t *>(message_ptr);
    }

    uint8_t mesha_networking_get_message_type(uintptr_t message_ptr) {
        if (message_ptr) {
            auto msg = Mesha::GetMessage(reinterpret_cast<uint8_t *>(message_ptr));
            return static_cast<uint8_t>(msg->msg_data_type());
        }

        return 0;
    }
}
