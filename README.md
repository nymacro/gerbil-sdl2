# Gambit/Gerbil SDL2 Bindings

*WARNING: this should be considered experimental. Much of what is presented here
will likely change.*

SDL2 bindings have been derived from [sphere-sdl2](https://github.com/fourthbit/sphere-sdl2).

Changes:
* Remove reliance on Spheres infrastructure.
* Tie into Gambit's garbage collection. Windows, surfaces and more are don't need
  manual freeing. **Ongoing**
* Additional error checking.
* Make interface more Scheme-like. **Future Goal**

## Installation

### Prerequisites

* Gambit Scheme *OR* Gerbil Scheme
* SDL2
* SDL2_ttf
* ~SDL2_image~ Needs work

### Build/Install

#### Gambit

```sh
make
```

#### Gerbil

```sh
./build.ss
```

## Example

See `examples` for examples of how to use these bindings.

Try the following:

### Gambit

```sh
make examples/life
./examples/life
```

![Game of Life animation](/meta/life.gif)

### Gerbil

```sh
gxi examples/life.ss
```

## Credits

Derived from sphere-sdl2. Mangled by me.
