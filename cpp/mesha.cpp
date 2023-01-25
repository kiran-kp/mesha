#include "mesha.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/variant/callable.hpp>

#include <ecl/ecl.h>

extern "C" {
	void init_mesha_bootstrap(cl_object obj);
}

static const char* lisp_argv = "Mesha";
static char** lisp_pargv;

LispServer *LispServer::singleton = nullptr;

void run_slynk() {
	cl_object start_server = c_string_to_object("(mesha-bootstrap:start-server)");
	cl_eval(start_server);
}

LispServer::LispServer() {
    assert(singleton == nullptr);
    singleton = this;
}

LispServer *LispServer::get_singleton() {
    return singleton;
}

godot::Error LispServer::init() {
    mutex = memnew(godot::Mutex);
	lisp_pargv = const_cast<char**>(&lisp_argv);

	cl_boot(1, lisp_pargv);
	ecl_init_module(nullptr, init_mesha_bootstrap);

	// const cl_env_ptr l_env = ecl_process_env();
	// CL_CATCH_ALL_BEGIN(l_env) {
	// 	CL_UNWIND_PROTECT_BEGIN(l_env) {
	// 		run_slynk();
	// 	}
	// 	CL_UNWIND_PROTECT_EXIT {}
	// 	CL_UNWIND_PROTECT_END;
	// }
	// CL_CATCH_ALL_END;

    return godot::OK;
}

void LispServer::unlock() {
    assert(mutex);
    mutex->unlock();
}

void LispServer::lock() {
    assert(mutex);
    mutex->lock();
}

void LispServer::shutdown() {
    assert(mutex);

    cl_shutdown();
    memdelete(mutex);
    mutex = nullptr;
}

godot::String LispServer::eval(const godot::String& expr) {
    auto str = expr.utf8();
    auto obj = c_string_to_object(str.get_data());
    auto res = cl_eval(obj);
    return godot::String::num_int64(ecl_to_int64_t(res));
}

void LispServer::_bind_methods() {
    godot::ClassDB::bind_method(godot::D_METHOD("eval", "expr"), &LispServer::eval);
}


Summator::Summator() {
    count = 0;
}

Summator::~Summator() {
}

void Summator::add(int p_value) {
    count += p_value;
}

void Summator::reset() {
    count = 0;
}

int Summator::get_total() const {
    return count;
}

void Summator::_bind_methods() {
    godot::ClassDB::bind_method(godot::D_METHOD("add", "value"), &Summator::add, DEFVAL(1));
    godot::ClassDB::bind_method(godot::D_METHOD("reset"), &Summator::reset);
    godot::ClassDB::bind_method(godot::D_METHOD("get_total"), &Summator::get_total);
}
