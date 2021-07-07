module GridShader (shaders, state, draw_grid, pre, post) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Types (GridBindings, GridState)
import Data.Vector (toArray)
import Data.Vector2 (Vec2, get2X, get2Y)
import Graphics.WebGL (Shaders(..), Mode(..))
import Graphics.WebGL as GL
import Graphics.WebGLRaw as RGL
import Data.Int (toNumber)
import Constants (screen_width)


shaders :: Shaders GridBindings
shaders = Shaders
    """
    precision mediump float;

    uniform vec2 u2resolution;
    uniform vec2 u2screen_size;

    void main() {
        vec2 uv = gl_FragCoord.xy / u2resolution;

        vec2 xy = (uv - vec2(0.5)) * u2screen_size;

        vec2 grid = fract(xy);

        const float cutoff = 0.06;
        vec2 s =
            smoothstep(0.0, cutoff, grid) *
            smoothstep(0.0, cutoff, vec2(1.0) - grid);

        float i = s.x * s.y;

        vec3 color = i * vec3(0.13, 0.14, 0.16);

        gl_FragColor = vec4(color, 1.0);
    }
    """
    """
    precision mediump float;

    attribute vec2 a2position;

    void main() {
        gl_Position = vec4(a2position, 0.0, 1.0);
    }
    """

position_data :: Array Number
position_data = map toNumber [
     1, -1,
     1,  1,
    -1, -1,
    -1,  1 ]

state :: forall eff. Eff (webgl :: WebGl | eff) GridState
state = GL.makeBufferFloat position_data >>= \position -> pure { position }

draw_grid :: forall eff. GridBindings -> GridState -> Vec2 Number -> Eff (webgl :: WebGl | eff) Unit
draw_grid bindings state resolution = do
    GL.setUniformFloats bindings.u2resolution $ toArray resolution

    let screen_height = screen_width * (get2Y resolution / get2X resolution)

    GL.setUniformFloats bindings.u2screen_size [ screen_width, screen_height ]

    GL.drawArr TRIANGLE_STRIP state.position bindings.a2position

pre :: forall eff. GridBindings -> Eff (webgl :: WebGl | eff) Unit
pre bindings = do
    RGL.useProgram_ bindings.program

    GL.enableVertexAttribArray bindings.a2position

post :: forall eff. GridBindings -> Eff (webgl :: WebGl | eff) Unit
post bindings = do
    GL.disableVertexAttribArray bindings.a2position
