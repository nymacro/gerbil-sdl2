# Gerbil SDL2 Bindings

*WARNING: this is experimental*

SDL2 bindings have been derived from [sphere-sdl2](https://github.com/fourthbit/sphere-sdl2).

This fork removes all reliance on the Spheres infrastructure and
re-implements much of the FFI functionality.

While the intent of these changes is to provide SDL2 functionality within
Gerbil Scheme, the core bindings remain compatible with Gambit.

## Installation

### Prerequisites

* Gerbil Scheme
* SDL2
* SDL2_image
* SDL2_ttf

### Build/Install

```sh
./build.ss
```

## Example

See `test_sdl.ss` for an example of how to use these bindings.

```
gxi test_sdl.ss
```

## Credits

Derived from sphere-sdl2. Mangled by me.
