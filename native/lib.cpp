#include <blend2d.h>

extern "C" {
    struct MeshaContext {
        BLFont font;
    };

    MeshaContext *MeshaCreateContext() {
        BLFontFace face;
        BLResult result = face.createFromFile("assets/fonts/OpenSans-Regular.ttf");
        if (result != BL_SUCCESS) {
            return nullptr;
        }

        MeshaContext *app = new MeshaContext();
        app->font.createFromFace(face, 20.0f);

        return app;
    }

    void MeshaDestroyContext(MeshaContext *app) {
        delete app;
    }
    
    struct MeshaDrawTextContext {
        const char *inText;
        size_t inSize;

        int outRectX0;
        int outRectY0;
        int outRectX1;
        int outRectY1;
    };
    
    void MeshaDrawText(MeshaContext *app, MeshaDrawTextContext *data) {
    }
}
