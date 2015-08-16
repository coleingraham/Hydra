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

If all goes well, you should see a black 640x480 window in the center of your screen! In order to make it do anything, you need to send it Lua code via OSC. I currently use oscsend from the command line via a vim plugin I wrote (vim-hydra). If you have both that and oscsend installed, you can run the following code in vim:

```lua
function draw()
    cameraLocation(0,0,2)
    background(0)
    translate(SinWave(4,-0.5,0.5),SinWave(4,-0.5,0.5,0.75),0)
    color(Color:hsla(SawWave(5),1,0.5))
    stroke()
    ring(TriWave(7,3,30),TriWave(3,0.2,0.6),TriWave(2,0.01,0.3))
end
```

by selecting it (easiest is with `<shift>-v`) and pressing `<leader>h`

There is currently no documentation. Some of the functions are defined in the Haskell source, while others are in lib/libHydra.hydra if you are interested.

NOTE: this project is very new and will be in flux for quite a long time. Bear with me...
