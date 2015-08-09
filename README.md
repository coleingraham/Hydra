# Hydra
Graphics canvas for live coding

Hydra is a program written in Haskell for live coding OpenGL with Lua. The main goal of the project is to create a fast, lightweight "canvas" suitable for for rendering realtime (and eventually fixed media also) visuals, controlled remotely via Lua code. Emphasis is places on ease of use in a live coding environment, and the ability to control multiple instances remotely for use with distributed and/or multi-projection use.

I recommend using cabal-dev to build it currently:

cd <path/to/Hydra>
cabal-dev install

Then to run it:

./dist/build/Hydra/Hydra

If all goes well, you should see a black 800x600 window in the center of your screen! In order to make it do anything, you need to send it Lua code via OSC. As I currently do not have a better solution, I recommend using this code in SuperCollider:

(
NetAddr("127.0.0.1",57150).sendMsg(
    "/code",
    "function draw()"
    " background(0,0,0,1)"
    " x = TriWave(0.5)"
    " for i=0,10 do\n"
    "  color(TriWave(2),0,0,1)"
    "  line(-i/10*x, -1*TriWave(0.4,-1,1), 0, (i/10)*x, 1*x, 0)"
    " end\n"
    "end"
);
)

There is currently no documentation. Some of the functions are defined in the Haskell source, while others are in lib/libHydra.hydra if you are interested.

NOTE: this project is very new and will be in flux for quite a long time. Bear with me...
