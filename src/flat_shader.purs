module FlatShader (flat_shaders, draw_mesh, pre, post) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Graphics.WebGL (Shaders(..), bindBufAndSetVertexAttr, Mode(..))
import Graphics.WebGL as GL
import Graphics.WebGLRaw as RGL
import Types (FlatBindings)
import Mesh (Mesh)


flat_shaders :: Shaders FlatBindings
flat_shaders = Shaders
    """
    precision mediump float;

    varying vec3 v3color;

    void main() {
        gl_FragColor = vec4(v3color, 1.0);
    }
    """
    """
    precision mediump float;

    uniform mat4 u44mvp;

    attribute vec2 a2position;
    attribute vec3 a3color;

    varying vec3 v3color;

    void main() {
        v3color = a3color;

        gl_Position = u44mvp * vec4(a2position, 0.0, 1.0);
    }
    """

draw_mesh :: forall eff. FlatBindings -> Mesh -> Array Number -> Eff (webgl :: WebGl | eff) Unit
draw_mesh bindings mesh mvp = do
    GL.setUniformFloats bindings.u44mvp mvp

    bindBufAndSetVertexAttr mesh.color bindings.a3color

    GL.drawArr TRIANGLES mesh.position bindings.a2position

pre :: forall eff. FlatBindings -> Eff (webgl :: WebGl | eff) Unit
pre bindings = do
    RGL.useProgram_ bindings.program

    GL.enableVertexAttribArray bindings.a2position
    GL.enableVertexAttribArray bindings.a3color

post :: forall eff. FlatBindings -> Eff (webgl :: WebGl | eff) Unit
post bindings = do
    GL.disableVertexAttribArray bindings.a2position
    GL.disableVertexAttribArray bindings.a3color
