#include "register_types.h"

#include "mesha.h"

#include <gdextension_interface.h>

#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>

using namespace godot;

static LispServer *lisp_server = nullptr;

extern "C" {
    void initialize_mesha_module(ModuleInitializationLevel p_level) {
        if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
            return;
        }

        ClassDB::register_class<LispServer>();
        ClassDB::register_class<Summator>();
    
        lisp_server = memnew(LispServer);
        Engine::get_singleton()->register_singleton("LispServer", lisp_server);

        lisp_server->init();
    }

    void uninitialize_mesha_module(ModuleInitializationLevel p_level) {
        if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
            return;
        }

        if (lisp_server) {
            lisp_server->shutdown();
            memfree(lisp_server);
            lisp_server = nullptr;
        }
    }

	GDExtensionBool GDE_EXPORT mesha_library_init(const GDExtensionInterface *p_interface,
                                                 const GDExtensionClassLibraryPtr p_library,
                                                 GDExtensionInitialization *r_initialization) {
		GDExtensionBinding::InitObject init_obj(p_interface, p_library, r_initialization);

		init_obj.register_initializer(initialize_mesha_module);
		init_obj.register_terminator(uninitialize_mesha_module);
		init_obj.set_minimum_library_initialization_level(MODULE_INITIALIZATION_LEVEL_SCENE);

		return init_obj.init();
	}
}
