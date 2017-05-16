# picFun
This is a simple server for image editing.

## implementation
The server is DB-less and uses STM to 'host' the images.

The schema is defined in API.hs using types thanks to the Servant package.

Image manipulation tools are defined in Pixels.hs using JuicyPixels package.

Built on Ubuntu 16.04 with stack. Run with `stack exec picFun-exe`.
To run on platforms other than linux re-build with `stack build`.