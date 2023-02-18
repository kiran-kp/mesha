#include "table.h"

#include <godot_cpp/classes/ref.hpp>
#include <godot_cpp/classes/resource_loader.hpp>
#include <godot_cpp/classes/text_server.hpp>
#include <godot_cpp/classes/text_server_manager.hpp>

#include <godot_cpp/core/class_db.hpp>

MeshaValue::MeshaValue() : _type(MeshaValueType::None)
                         , integer_value(0) {
}

MeshaValue::~MeshaValue() {
}

MeshaCell::MeshaCell() {
}

MeshaCell::~MeshaCell() {
    if (_value._type == MeshaValueType::Text) {
        _value.text_value.unref();
        _value._type = MeshaValueType::None;
    }
}

void MeshaCell::set_text(const String& s) {
    _THREAD_SAFE_METHOD_;

    Ref<TextParagraph> text;

    if (_value._type == MeshaValueType::Text) {
        text = _value.text_value;
    } else {
        text.instantiate();
        _value.text_value = text;
        _value._type = MeshaValueType::Text;
    }

    text->clear();
    text->add_string(s, _defaultFontData, 20);
    queue_redraw();
}

Vector2 MeshaCell::_get_minimum_size() const {
    return Size2(20.0f, 20.0f);
}

void MeshaCell::_notification(int p_what) {
	switch (p_what) {
		case NOTIFICATION_ENTER_TREE: {
            Ref<TextServer> ts = TextServerManager::get_singleton()->get_primary_interface();
            _defaultFontData = ResourceLoader::get_singleton()->load("res://fonts/OpenSans-Regular.ttf");

            _defaultFont = ts->create_font();
            ts->font_set_data(_defaultFont, _defaultFontData->get_data());

			queue_redraw();
		} break;

		case NOTIFICATION_INTERNAL_PHYSICS_PROCESS: {
			queue_redraw();
		} break;

		case NOTIFICATION_DRAW: {
            draw();
		} break;

		case NOTIFICATION_INTERNAL_PROCESS: {
			if (is_visible_in_tree()) {
				queue_redraw();
			}
		} break;
	}
}

void MeshaCell::draw() {
    _THREAD_SAFE_METHOD_;
    Ref<TextServer> ts = TextServerManager::get_singleton()->get_primary_interface();
    RID cid = get_canvas_item();
    if (_value._type == MeshaValueType::Text) {
        _value.text_value->draw(cid, Vector2(10.0f, 10.0f));
    }
}

void MeshaCell::_bind_methods() {
    godot::ClassDB::bind_method(godot::D_METHOD("set_text", "s"), &MeshaCell::set_text);
}
