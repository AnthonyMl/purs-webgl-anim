module Renderer where

import Prelude
import Types (SkinningBindings, FlatBindings, GridBindings, GridState)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Matrix (toArray)
import Data.Matrix4 (Mat4, makeLookAt, mulM)
import Data.Vector (Vec(..), scale)
import Data.Vector2 (Vec2)
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Graphics.WebGLRaw as GLR
import Data.Traversable (traverse_)
import Constants (view_distance, global_up, global_look)
import Mesh (Mesh, SkinningMesh)
import SkinningShader as SkinningShader
import FlatShader as FlatShader
import GridShader as GridShader


type StaticState =
    { skinning_bindings :: SkinningBindings
    , grid_bindings     :: GridBindings
    , flat_bindings     :: FlatBindings
    , grid_state        :: GridState
    , projection_matrix :: Mat4
    , resolution        :: Vec2 Number
    , meshes            :: Array SkinningMesh
    , background_mesh   :: Mesh
    , bounding_box_mesh :: Mesh }

type DynamicState =
    { model_transforms :: Array Mat4
    , skinning_arrays  :: Array (Array Number) }

view_matrix :: Mat4
view_matrix = let
    eye    = scale view_distance global_look
    center = Vec [0.0, 0.0, 0.0]
    up     = global_up
    in
    makeLookAt eye center up

draw_scene :: forall eff. StaticState -> DynamicState -> Eff (webgl :: WebGl | eff) Unit
draw_scene static dynamic = do
    let view_projection = mulM static.projection_matrix view_matrix

    --GRID
    GridShader.pre static.grid_bindings

    GridShader.draw_grid static.grid_bindings static.grid_state static.resolution

    GridShader.post static.grid_bindings

    --FLAT MESHES
    FlatShader.pre static.flat_bindings

    FlatShader.draw_mesh static.flat_bindings static.background_mesh (toArray view_projection)

    let draw_bounding_box = FlatShader.draw_mesh static.flat_bindings static.bounding_box_mesh <<< toArray <<< (mulM view_projection)

    traverse_ draw_bounding_box dynamic.model_transforms

    FlatShader.post static.flat_bindings

    --SKINNING MESHES
    SkinningShader.pre static.skinning_bindings

    let draw_skinning_wrapper (Tuple skinning_mesh skinning_arrays) = SkinningShader.draw_skinning_mesh static.skinning_bindings skinning_mesh skinning_arrays

    traverse_ draw_skinning_wrapper $ Array.zip static.meshes dynamic.skinning_arrays

    SkinningShader.post static.skinning_bindings


load_shader :: forall eff. String -> Int -> Eff (webgl :: WebGl | eff) (Either String GLR.WebGLShader)
load_shader source kind = do
    shader <- GLR.createShader_ kind

    GLR.shaderSource_ shader source

    GLR.compileShader_ shader

    status <- GLR.getShaderParameter_ shader GLR._COMPILE_STATUS

    if status then pure $ Right shader else
        GLR.getShaderInfoLog_ shader >>= (\x -> pure $ Left x)


--not unit but bindings
load_program :: forall eff. String -> String -> Eff (webgl :: WebGl | eff) (Either String GLR.WebGLProgram)
load_program vertex_source fragment_source = do
    vertex   <- load_shader vertex_source GLR._VERTEX_SHADER
    fragment <- load_shader fragment_source GLR._FRAGMENT_SHADER

    case vertex, fragment of
        Left  v, Left  f -> pure $ Left $ v <> "\n\n" <> f
        Left  v, Right _ -> pure $ Left v
        Right _, Left  f -> pure $ Left f
        Right v, Right f -> do
            program <- GLR.createProgram_

            GLR.attachShader_ program v
            GLR.attachShader_ program f

            GLR.linkProgram_ program

            status <- GLR.getProgramParameter_ program GLR._LINK_STATUS

            if status then pure $ Right program else
                GLR.getProgramInfoLog_ program >>= (\x -> pure $ Left x)
