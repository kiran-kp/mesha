#include "mesha.h"

#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/variant/callable.hpp>

#include <ecl/ecl.h>

extern "C" {
	void init_mesha_core(cl_object obj);
}

static const char* lisp_argv = "Mesha";
static char** lisp_pargv;

MeshaServer *MeshaServer::singleton = nullptr;

void run_slynk() {
	cl_object start_server = c_string_to_object("(mesha-core:start-server)");
	cl_eval(start_server);
}

MeshaServer::MeshaServer() {
    assert(singleton == nullptr);
    singleton = this;
}

MeshaServer *MeshaServer::get_singleton() {
    return singleton;
}

godot::Error MeshaServer::init() {
    mutex = memnew(godot::Mutex);
	lisp_pargv = const_cast<char**>(&lisp_argv);

	cl_boot(1, lisp_pargv);
	ecl_init_module(nullptr, init_mesha_core);

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

void MeshaServer::unlock() {
    assert(mutex);
    mutex->unlock();
}

void MeshaServer::lock() {
    assert(mutex);
    mutex->lock();
}

void MeshaServer::shutdown() {
    assert(mutex);

    cl_shutdown();
    memdelete(mutex);
    mutex = nullptr;
}

godot::String mesha_to_string(cl_object obj) {
    if (floatp(obj)) {
        return godot::String::num(ecl_to_float(obj));
    } else if (ecl_numberp(obj)) {
        return godot::String::num_int64(ecl_to_int64_t(obj));
    } else if (ecl_stringp(obj)) {
        char buffer[1024];
        cl_object utf_8 = ecl_make_keyword("UTF-8");
        ecl_encode_to_cstring(buffer, 1024, obj, utf_8);
        return godot::String(buffer);
    } else if (ECL_LISTP(obj)) {
        if (ecl_to_int64_t(cl_list_length(obj)) > 0) {
            auto car = cl_car(obj);
            return mesha_to_string(car);
        }
    } else if (ecl_keywordp(obj)) {
        return mesha_to_string(cl_string(obj));
    }


    return godot::String("<unknown>");
}

godot::String MeshaServer::eval(const godot::String& expr) {
    if (!expr.is_empty()) {
        auto str = expr.utf8();
        auto obj = c_string_to_object(str.get_data());
        auto res = cl_eval(obj);
        return mesha_to_string(res);
    }

    return godot::String("");
}

void MeshaServer::_bind_methods() {
    godot::ClassDB::bind_method(godot::D_METHOD("eval", "expr"), &MeshaServer::eval);
}
