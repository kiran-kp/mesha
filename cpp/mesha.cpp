#include "mesha.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/templates/vector.hpp>
#include <godot_cpp/variant/callable.hpp>

#include <sockpp/tcp_connector.h>

MeshaServer *MeshaServer::singleton = nullptr;

MeshaServer::MeshaServer() {
    // assert(singleton == nullptr);
    singleton = this;
}

MeshaServer *MeshaServer::get_singleton() {
    return singleton;
}

godot::Error MeshaServer::init() {
    mutex = memnew(godot::Mutex);

    sockpp::initialize();

    sockpp::tcp_connector conn({"127.0.0.1", 10977}, std::chrono::seconds{5});
	if (conn) {
        printf("\nMesha - Connected!\n");
        godot::Vector<uint8_t> data { 'A', 'C', 'K', '\n' };
        data.resize(20);
        conn.write(data.ptrw(), 4);
        auto bytes_read = conn.read(data.ptrw(), 20);
        printf("\n Mesha - %d bytes received\n", bytes_read);
        for (int i = 0; i < data.size(); i++) {
            printf("%c(%d) ", data[i], data[i]);
        }
	}

    return godot::OK;
}

void MeshaServer::unlock() {
    // assert(mutex);
    mutex->unlock();
}

void MeshaServer::lock() {
    // assert(mutex);
    mutex->lock();
}

void MeshaServer::shutdown() {
    // assert(mutex);

    memdelete(mutex);
    mutex = nullptr;
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
