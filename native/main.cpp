#include <blend2d.h>
#include <SDL.h>

#include <algorithm>

#define ARRAY_SIZE(X) uint32_t(sizeof(X) / sizeof(X[0]))

// ============================================================================
// [SDL_Application]
// ============================================================================

class SDL_Application {
public:
  SDL_Application() noexcept
    : _window(nullptr),
      _renderer(nullptr),
      _texture(nullptr),
      _blSurface(),
      _quitting(false),
      _exitCode(0),
      _frameCounter(0),
      _frameTicks(0) {}
  ~SDL_Application() noexcept { destroyWindow(); }

  // --------------------------------------------------------------------------
  // [Core]
  // --------------------------------------------------------------------------

  bool createWindow(int w, int h) noexcept {
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS | SDL_INIT_TIMER) != 0) {
      printf("Failed to initialize SDL: %s", SDL_GetError());
      return false;
    }

    _window = SDL_CreateWindow("SDL Window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, w, h, SDL_WINDOW_SHOWN);
    if (!_window)
      return false;

    _renderer = SDL_CreateRenderer(_window, -1, 0);
    if (!_renderer) {
      printf("FAILED to create SDL_Renderer: %s\n", SDL_GetError());
      destroyWindow();
      return false;
    }
    SDL_SetRenderDrawBlendMode(_renderer, SDL_BLENDMODE_NONE);

    if (!createSurface(w, h)) {
      destroyWindow();
      return false;
    }

    return true;
  }

  void destroyWindow() noexcept {
    destroySurface();

    if (_renderer) {
      SDL_DestroyRenderer(_renderer);
      _renderer = nullptr;
    }

    if (!_window) {
      SDL_DestroyWindow(_window);
      _window = nullptr;
    }
  }

  bool createSurface(int w, int h) noexcept {
    destroySurface();

    _texture = SDL_CreateTexture(_renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h);
    if (!_texture)
      return false;
    SDL_SetTextureBlendMode(_texture, SDL_BLENDMODE_NONE);

    if (_blSurface.create(w, h, BL_FORMAT_PRGB32) != BL_SUCCESS)
      return false;

    return true;
  }

  void destroySurface() noexcept {
    if (_texture) {
      SDL_DestroyTexture(_texture);
      _texture = nullptr;
    }

    _blSurface.reset();
  }

  int run() noexcept {
    SDL_Event event;

    for (;;) {
      while (!_quitting && SDL_PollEvent(&event))
        onEvent(event);

      if (_quitting)
        break;

      onFrame();
      onRender();
      blit();
      updateFrameCounter();

      //SDL_Delay(20);
    }

    return _exitCode;
  }

  void blit() noexcept {
    BLImageData data;
    _blSurface.getData(&data);

    SDL_UpdateTexture(_texture, nullptr, data.pixelData, int(data.stride));
    SDL_RenderCopy(_renderer, _texture, nullptr, nullptr);
    SDL_RenderPresent(_renderer);
  }

  void updateFrameCounter() noexcept {
    uint32_t ticks = SDL_GetTicks();
    if (++_frameCounter >= 100) {
      _fps = (1000.0 / double(ticks - _frameTicks)) * double(_frameCounter);
      _frameCounter = 0;
      _frameTicks = ticks;
    }
  }

  inline double fps() const noexcept { return _fps; }

  // --------------------------------------------------------------------------
  // [Interface]
  // --------------------------------------------------------------------------

  virtual void onEvent(SDL_Event& event) noexcept = 0;
  virtual void onFrame() noexcept = 0;
  virtual void onRender() noexcept = 0;

  // --------------------------------------------------------------------------
  // [Members]
  // --------------------------------------------------------------------------

  SDL_Window* _window;
  SDL_Renderer* _renderer;
  SDL_Texture* _texture;
  BLImage _blSurface;

  bool _quitting;
  int _exitCode;
  int _frameCounter;
  int _frameTicks;
  double _fps;
};

// ============================================================================
// [MyApplication]
// ============================================================================

class MyApplication : public SDL_Application {
public:
  enum : uint32_t { kNumVertices = 100 };

  MyApplication() noexcept { onInit(); }

  void onEvent(SDL_Event& event) noexcept override {
    if (event.type == SDL_QUIT) {
      _quitting = true;
    }
  }

  void onInit() noexcept {
    BLFontFace face;
    face.createFromFile("../assets/fonts/OpenSans-Regular.ttf");
    font.createFromFace(face, 20.f);

    BLRandom rnd;

    for (size_t i = 0; i < kNumVertices; i++) {
      pts[i].reset(rnd.nextDouble() * 500, rnd.nextDouble() * 500);
      dir[i].reset(rnd.nextDouble() * 5, rnd.nextDouble() * 5);
    }
  }

  void onFrame() noexcept override {
    double w = _blSurface.width();
    double h = _blSurface.height();

    for (size_t i = 0; i < kNumVertices; i++) {
      pts[i] += dir[i];

      if (pts[i].x <= 0.0 || pts[i].x >= w) {
        dir[i].x = -dir[i].x;
        pts[i].x += dir[i].x;
      }

      if (pts[i].y <= 0.0 || pts[i].y >= h) {
        dir[i].y = -dir[i].y;
        pts[i].y += dir[i].y;
      }
    }
  }

  void onRender() noexcept override {
    BLContext ctx(_blSurface);

    ctx.fillAll(BLRgba32(0xFF000000u));

    BLPath p;

    int count = 2000;
    double PI = 3.14159265359;

    double cx = 1024 / 2;
    double cy = 1024 / 2;
    double maxDist = 1000.0;
    double baseAngle = 60.0 / 180.0 * PI;

    for (int i = 0; i < count; i++) {
      double t = double(i) * 1.01 / 1000;
      double d = t * 1000.0 * 0.4 + 10;
      double a = baseAngle + t * PI * 2 * 20;
      double x = cx + std::cos(a) * d;
      double y = cy + std::sin(a) * d;
      double r = std::min(t * 8 + 0.5, 10.0);
      p.addCircle(BLCircle(x, y, r));
    }

    ctx.fillPath(p, BLRgba32(0xFFFFFFFFu));

    char buf[128];
    snprintf(buf, ARRAY_SIZE(buf), "FPS: %.2f", fps());

    ctx.setFillStyle(BLRgba32(0xFFFF0000u));
    ctx.fillUtf8Text(BLPoint(5, 20), font, buf);
  }

  void initPoints() noexcept {
  }

  BLFont font;

  BLPoint pts[kNumVertices];
  BLPoint dir[kNumVertices];
};

// ============================================================================
// [Main]
// ============================================================================

int main(int argc, char *argv[]) {
  MyApplication app;

  if (!app.createWindow(1024, 1024))
    return 1;

  return app.run();
}
