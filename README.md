# Hydra
Graphics canvas for live coding

Hydra is a program written in Haskell for live coding OpenGL with Lua. The main goal of the project is to create a fast, lightweight "canvas" suitable for for rendering realtime (and eventually fixed media also) visuals, controlled remotely via Lua code. Emphasis is places on ease of use in a live coding environment, and the ability to control multiple instances remotely for use with distributed and/or multi-projection use.

I recommend using cabal-dev to build it currently:

```
cd /path/to/Hydra
cabal-dev install
```

Then to run it:

`./dist/build/Hydra/Hydra`

If all goes well, you should see a black 800x600 window in the center of your screen! In order to make it do anything, you need to send it Lua code via OSC. As I currently do not have a better solution, I recommend using this code in SuperCollider:

```
(
NetAddr("127.0.0.1",57150).sendMsg(
    "/code",
    "function draw()"
    " cameraLocation(0,0,TriWave(0.1,1,2))"
    " cameraPan(TriWave(0.12,-20,20))"
    " cameraTilt(TriWave(0.13,-20,20))"
    " background(0,0,0,1)"
    " color(0,1,1,1)"
    " pushMatrix()"
    " translate(0.5,0,0.5)"
    " rotateZ(1)"
    " rect(0.5,0.25)"
    " popMatrix()"
    " color(1,0,1,1)"
    " rect(0.5,0.25)"
    " color(0,1,0,1)"
    " for i=0,3 do\n"
    "  rotateZ(SawWave(0.1,0,6.2831853071796))"
    "  translate(TriWave(0.2,0,1),0,-1)"
    "  scale(TriWave(0.5,0.5,1),
    TriWave(0.4,0.5,1),
    TriWave(0.6,0.5,1))"
    "  rect(0.5,0.25)"
    " end\n"
    "end"
);
)
```

There is currently no documentation. Some of the functions are defined in the Haskell source, while others are in lib/libHydra.hydra if you are interested.

NOTE: this project is very new and will be in flux for quite a long time. Bear with me...
