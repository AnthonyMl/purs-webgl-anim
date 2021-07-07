module Mesh (Mesh, SkinningMesh, mesh, skinning_mesh) where

import Prelude
import Data.ArrayBuffer.Types as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Graphics.WebGL (Buffer, makeBufferFloat)


type Mesh =
    { position :: Buffer T.Float32
    , color    :: Buffer T.Float32 }

type SkinningMesh =
    { position         :: Buffer T.Float32
    , color            :: Buffer T.Float32
    , skinning_indices :: Buffer T.Float32 }

mesh :: forall eff. Array Number -> Array Number -> Eff (webgl :: WebGl | eff) Mesh
mesh position_data color_data = do
    position <- makeBufferFloat position_data
    color    <- makeBufferFloat color_data
    pure { position, color }

skinning_mesh :: forall eff. Array Number -> Array Number -> Array Number -> Eff (webgl :: WebGl | eff) SkinningMesh
skinning_mesh position_data color_data skinning_indices_data = do
    position         <- makeBufferFloat position_data
    color            <- makeBufferFloat color_data
    skinning_indices <- makeBufferFloat skinning_indices_data
    pure { position, color, skinning_indices }
