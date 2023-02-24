#ifndef MESHA_H
#define MESHA_H

#include <godot_cpp/classes/mutex.hpp>
#include <godot_cpp/classes/object.hpp>
#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/classes/thread.hpp>
#include <godot_cpp/templates/vector.hpp>

class MeshaServer : public godot::Object {
    GDCLASS(MeshaServer, godot::Object);

    static MeshaServer *singleton;

public:
    static MeshaServer *get_singleton();

public:
    MeshaServer();
    godot::Error init();
    void lock();
    void unlock();
    void shutdown();
    godot::String eval(const godot::String& expr);

protected:
    static void _bind_methods();

private:
    godot::Mutex *m_mutex;
    uintptr_t m_server_connection;
};

#endif // MESHA_H
