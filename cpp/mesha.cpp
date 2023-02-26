#include "mesha.h"

#include "mesha_networking.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/templates/vector.hpp>
#include <godot_cpp/variant/callable.hpp>

#include <assert.h>

MeshaServer *MeshaServer::singleton = nullptr;

MeshaServer::MeshaServer() {
    assert(singleton == nullptr);
    singleton = this;
}

MeshaServer *MeshaServer::get_singleton() {
    return singleton;
}

godot::Error MeshaServer::init() {
    m_mutex = memnew(godot::Mutex);

    mesha_networking_init();
    m_server_connection = mesha_networking_connect("127.0.0.1", 10977);

	if (m_server_connection) {
        printf("\nMesha - Connected!\n");
        mesha_networking_send_c2sgreeting(m_server_connection, "Mesha#1337");
        auto msg = mesha_networking_read_message(m_server_connection);
        printf("\nMesha - Got message: %s\n", mesha_networking_s2cgreeting_response_get_secret(msg));
        mesha_networking_free_message(msg);
        fflush(stdout);
	}

    return godot::OK;
}

void MeshaServer::unlock() {
    m_mutex->unlock();
}

void MeshaServer::lock() {
    m_mutex->lock();
}

void MeshaServer::shutdown() {
    memdelete(m_mutex);
    m_mutex = nullptr;
}

godot::String MeshaServer::eval(const godot::String& expr) {
    if (!expr.is_empty()) {
        auto str = expr.utf8();
    }

    return godot::String("");
}

void MeshaServer::_bind_methods() {
    godot::ClassDB::bind_method(godot::D_METHOD("eval", "expr"), &MeshaServer::eval);
}
