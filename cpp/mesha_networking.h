#pragma once

#include <sockpp/tcp_connector.h>

extern "C" {
    void mesha_networking_init();

    uintptr_t mesha_networking_create_server(const char *host, uint16_t port);
    uintptr_t mesha_networking_accept_connection(uintptr_t listener_ptr);
    void mesha_networking_close_listener(uintptr_t listener_ptr);
    uintptr_t mesha_networking_connect(const char *host, uint16_t port);
    void mesha_networking_close_connection(uintptr_t socket_ptr);
    uintptr_t mesha_networking_read_message(uintptr_t socket_ptr);
    void mesha_networking_free_message(uintptr_t message_ptr);
    uint8_t mesha_networking_get_message_type(uintptr_t message_ptr);

    void mesha_networking_send_c2sgreeting(uintptr_t socket_ptr, const char *secret);
    char *mesha_networking_c2sgreeting_get_secret(uintptr_t message_ptr);
    void mesha_networking_send_s2cgreeting_response(uintptr_t socket_ptr, const char *secret);
    char *mesha_networking_s2cgreeting_response_get_secret(uintptr_t message_ptr);
}
