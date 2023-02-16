#include "register_types.h"

#include "mesha.h"
#include "table.h"

#include <gdextension_interface.h>

#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>

using namespace godot;

static MeshaServer *mesha_server = nullptr;

extern "C" {
    void initialize_mesha_module(ModuleInitializationLevel p_level) {
        switch (p_level) {
        case MODULE_INITIALIZATION_LEVEL_SERVERS:
            GDREGISTER_CLASS(MeshaServer);

            mesha_server = memnew(MeshaServer);
            Engine::get_singleton()->register_singleton("MeshaServer", mesha_server);

            mesha_server->init();
            break;
        case MODULE_INITIALIZATION_LEVEL_SCENE:
            GDREGISTER_CLASS(MeshaCell);
            break;
        default:
            break;
        }

        // Register all classes before constructing anything
    
    }

    void uninitialize_mesha_module(ModuleInitializationLevel p_level) {
        if (p_level != MODULE_INITIALIZATION_LEVEL_SERVERS) {
            return;
        }

        if (mesha_server) {
            mesha_server->shutdown();
            memfree(mesha_server);
            mesha_server = nullptr;
        }
    }

	GDExtensionBool GDE_EXPORT mesha_library_init(const GDExtensionInterface *p_interface,
                                                 const GDExtensionClassLibraryPtr p_library,
                                                 GDExtensionInitialization *r_initialization) {
		GDExtensionBinding::InitObject init_obj(p_interface, p_library, r_initialization);

		init_obj.register_initializer(initialize_mesha_module);
		init_obj.register_terminator(uninitialize_mesha_module);
		init_obj.set_minimum_library_initialization_level(MODULE_INITIALIZATION_LEVEL_SERVERS);

		return init_obj.init();
	}
}
