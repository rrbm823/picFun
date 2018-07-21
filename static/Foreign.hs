{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign where

import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Web.Canvas
import Miso hiding (update)
import Miso.String (MisoString, (<>), pack)

foreign import javascript unsafe "var ctx = $1; var x = $2; var y = $3; var pixel = ctx.getImageData(x, y, 1, 1);  var data = pixel.data; $r = 'rgba(' + data[0] + ', ' + data[1] + ', ' + data[2]  + ', 255)'"
  getPixel :: Context -> Int -> Int -> IO JSString

foreign import javascript unsafe "var ctx = $1; var x = $2; var y = $3; var w = $4; var h = $5; var pixel = ctx.getImageData(x, y, w, h);  var data = pixel.data; $r = data.join()"
  getPixels :: Context -> Int -> Int -> Int -> Int -> IO JSString

foreign import javascript unsafe "$r = document.getElementById($1).getContext('2d');"
  getCtx :: MisoString -> IO Context

foreign import javascript unsafe "console.log($1);"
  log_ :: JSVal -> IO ()

foreign import javascript unsafe "$1.onload = function () {$2.drawImage($1,0,0,$1.width,$1.height,0,0,550,406)}"
  drawImage' :: Image ->  Context -> IO ()

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "window.addEventListener('keydown', function(e) { if([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) { e.preventDefault();} }, false);"
  noArrowScrolling :: IO ()

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal --FileReader
 
foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal --FileReader -> File

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: MisoString -> IO JSVal --String -> Element

{--
foreign import javascript unsafe "$1.readAsArrayBuffer($2);"
  readText :: JSVal -> JSVal -> IO () --}

foreign import javascript unsafe "$r = $1.target.result"
  readResult :: JSVal -> IO MisoString --File -> String

foreign import javascript unsafe "$1.readAsDataURL($2)"
  readResultURL :: JSVal -> JSVal -> IO MisoString --FileReader -> File -> String

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO () 

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad1 :: JSVal -> Callback (JSVal -> IO ()) -> IO () --FileReader -> (File -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onload = $2;"
  imgSetOnLoad :: Image -> Callback (IO ()) -> IO () --Image -> (File -> IO ()) -> IO ()



fun = "function draw_fill(ctx, x, y, fill_r, fill_g, fill_b, fill_a){\
\  var stack = [[x, y]];\
\  var c_width = canvas.width;\
\  var c_height = canvas.height;\
\  var id = ctx.getImageData(0, 0, c_width, c_height);\
\  pixel_pos = (y*c_width + x) * 4;\
\  var start_r = id.data[pixel_pos+0];\
\  var start_g = id.data[pixel_pos+1];\
\  var start_b = id.data[pixel_pos+2];\
\  var start_a = id.data[pixel_pos+3];\
\  \
\  if(\
\    fill_r === start_r &&\
\    fill_g === start_g &&\
\    fill_b === start_b &&\
\    fill_a === start_a\
\  ){\
\    return;\
\  }\
\  \
\  while(stack.length){\
\    var new_pos, x, y, pixel_pos, reach_left, reach_right;\
\    new_pos = stack.pop();\
\    x = new_pos[0];\
\    y = new_pos[1];\
\\
\    pixel_pos = (y*c_width + x) * 4;\
\    while(matches_start_color(pixel_pos)){\
\      y--;\
\      pixel_pos = (y*c_width + x) * 4;\
\    }\
\    reach_left = false;\
\    reach_right = false;\
\    while(true){\
\      y++;\
\      pixel_pos = (y*c_width + x) * 4;\
\      \
\      if(!(y < c_height && matches_start_color(pixel_pos))){\
\        break;\
\      }\
\      \
\      color_pixel(pixel_pos);\
\\
\      if(x > 0){\
\        if(matches_start_color(pixel_pos - 4)){\
\          if(!reach_left){\
\            stack.push([x - 1, y]);\
\            reach_left = true;\
\          }\
\        }else if(reach_left){\
\          reach_left = false;\
\        }\
\      }\
\\
\      if(x < c_width-1){\
\        if(matches_start_color(pixel_pos + 4)){\
\          if(!reach_right){\
\            stack.push([x + 1, y]);\
\            reach_right = true;\
\          }\
\        }else if(reach_right){\
\          reach_right = false;\
\        }\
\      }\
\\
\      pixel_pos += c_width * 4;\
\    }\
\  }\
\  ctx.putImageData(id, 0, 0);\
\\
\  function matches_start_color(pixel_pos){\
\    return (\
\      id.data[pixel_pos+0] === start_r &&\
\      id.data[pixel_pos+1] === start_g &&\
\      id.data[pixel_pos+2] === start_b &&\
\      id.data[pixel_pos+3] === start_a\
\    );\
\  }\
\ \
\  function color_pixel(pixel_pos){\
\    id.data[pixel_pos+0] = fill_r;\
\    id.data[pixel_pos+1] = fill_g;\
\    id.data[pixel_pos+2] = fill_b;\
\    id.data[pixel_pos+3] = fill_a;\
\  }\
\ } "

