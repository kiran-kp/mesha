// On linux compile with:
// g++ -std=c++17 main.cpp -o prog -lSDL2 -ldl
// On windows compile with (if using mingw)
// g++ main.cpp -o prog.exe -lmingw32 -lSDL2main -lSDL2
// On Mac compile with:
// clang++ main.cpp -I/Library/Frameworks/SDL2.framework/Headers -F/Library/Frameworks -framework SDL2

// C++ Standard Libraries
#include <iostream>
// Third Party
#include <SDL2/SDL.h> // For Mac, use <SDL.h>

int main(int argc, char* argv[]) {
    // Create a window data type
    // This pointer will point to the 
    // window that is allocated from SDL_CreateWindow
    SDL_Window* window = nullptr;

    // Initialize the video subsystem.
    // iF it returns less than 1, then an
    // error code will be received.
    if (SDL_Init(SDL_INIT_VIDEO) < 0){
        std::cout << "SDL could not be initialized: " <<
            SDL_GetError();
    } else {
        std::cout << "SDL video system is ready to go\n";
    }

    // Request a window to be created for our platform
    // The parameters are for the title, x and y position,
    // and the width and height of the window.
    window = SDL_CreateWindow("Mesha", 20, 20, 640, 480, SDL_WINDOW_SHOWN);

    SDL_Renderer* renderer = nullptr;
    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    
    // Create a rectangle
    SDL_Rect rectangle;
    rectangle.x = 50;
    rectangle.y = 100;
    rectangle.w = 20;
    rectangle.h = 20;

    bool gameIsRunning = true;
    while(gameIsRunning) {
        SDL_Event event;

        while (SDL_PollEvent(&event)) {
            if(event.type == SDL_QUIT) {
                gameIsRunning = false;
            }
        }
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(renderer);

        SDL_SetRenderDrawColor(renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);
        SDL_RenderDrawLine(renderer, 5, 5, 100, 120);

        SDL_RenderDrawRect(renderer, &rectangle);

        SDL_RenderPresent(renderer);

    }

    SDL_DestroyWindow(window);
    
    SDL_Quit();
    return 0;
}
