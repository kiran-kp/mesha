#ifndef TABLE_H
#define TABLE_H

#include <godot_cpp/classes/font_file.hpp>
#include <godot_cpp/classes/control.hpp>
#include <godot_cpp/classes/text_paragraph.hpp>
#include <godot_cpp/classes/mutex.hpp>
#include <godot_cpp/variant/variant.hpp>

#include <godot_cpp/core/mutex_lock.hpp>

#include <variant>

// This is annoying but the _THREAD_SAFE_CLASS_ macro requires it
using namespace godot;

enum class MeshaValueType {
    None,
    Text,
    Integer,
    Float
};

struct MeshaValue {
    MeshaValue();
    ~MeshaValue();

    MeshaValueType _type;
    union {
        Ref<TextParagraph> text_value;
        size_t integer_value;
        double floating_point_value;
    };
};

class MeshaCell : public Control {
    GDCLASS(MeshaCell, Control);
    _THREAD_SAFE_CLASS_

public:
    MeshaCell();
    virtual ~MeshaCell();
    void set_text(const String& s);

private:
    void draw();

    MeshaValue _value;
    Ref<FontFile> _defaultFontData;
    RID _defaultFont;
protected:
	void _notification(int p_notification);
    static void _bind_methods();
};

#endif // TABLE_H
