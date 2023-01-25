#ifndef MESHA_H
#define MESHA_H

#include <godot_cpp/classes/mutex.hpp>
#include <godot_cpp/classes/object.hpp>
#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/classes/thread.hpp>
#include <godot_cpp/templates/vector.hpp>

class LispServer : public godot::Object {
    GDCLASS(LispServer, godot::Object);

    static LispServer *singleton;

public:
    static LispServer *get_singleton();

public:
    LispServer();
    godot::Error init();
    void lock();
    void unlock();
    void shutdown();
    godot::String eval(const godot::String& expr);

protected:
    static void _bind_methods();

private:
    volatile bool thread_exited;
    mutable bool exit_thread;
    godot::Thread *thread;
    godot::Mutex *mutex;

    godot::Vector<godot::String> code_to_eval;
};

class Summator : public godot::RefCounted {
    GDCLASS(Summator, godot::RefCounted);

    int count;

protected:
    static void _bind_methods();

public:
    Summator();
    ~Summator();

    void add(int p_value);
    void reset();
    int get_total() const;
};

#endif // MESHA_H
