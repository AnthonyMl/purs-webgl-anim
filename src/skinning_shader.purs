module SkinningShader (shaders, draw_skinning_mesh, pre, post) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Graphics.WebGL (Shaders(..), bindBufAndSetVertexAttr, Mode(..))
import Graphics.WebGL as GL
import Graphics.WebGLRaw as RGL
import Types (SkinningBindings)
import Mesh (SkinningMesh)

shaders :: Shaders SkinningBindings
shaders = Shaders
    """
    precision mediump float;

    varying vec3 v3color;

    void main() {
        gl_FragColor = vec4(v3color, 1.0);
    }
    """
    """
    precision mediump float;

    uniform mat4 u44skinning_matrices[20];

    attribute vec2 a2position;
    attribute vec3 a3color;
    attribute float a1skinning_index;

    varying vec3 v3color;

    void main() {
        v3color = a3color;

        mat4 transform = u44skinning_matrices[int(a1skinning_index)];

        gl_Position = transform * vec4(a2position, 0.0, 1.0);
    }
    """

draw_skinning_mesh :: forall eff. SkinningBindings -> SkinningMesh -> Array Number -> Eff (webgl :: WebGl | eff) Unit
draw_skinning_mesh bindings mesh skinning_array = do
    GL.setUniformFloats bindings.u44skinning_matrices skinning_array

    bindBufAndSetVertexAttr mesh.color bindings.a3color

    bindBufAndSetVertexAttr mesh.skinning_indices bindings.a1skinning_index

    GL.drawArr TRIANGLES mesh.position bindings.a2position

pre :: forall eff. SkinningBindings -> Eff (webgl :: WebGl | eff) Unit
pre bindings = do
    RGL.useProgram_ bindings.program

    GL.enableVertexAttribArray bindings.a2position
    GL.enableVertexAttribArray bindings.a1skinning_index
    GL.enableVertexAttribArray bindings.a3color

post :: forall eff. SkinningBindings -> Eff (webgl :: WebGl | eff) Unit
post bindings = do
    GL.disableVertexAttribArray bindings.a2position
    GL.disableVertexAttribArray bindings.a1skinning_index
    GL.disableVertexAttribArray bindings.a3color
