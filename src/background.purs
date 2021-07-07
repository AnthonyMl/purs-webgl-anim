module Background (mesh, Line(..), geometry) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Data.Array as Array
import Data.Unfoldable as Unfoldable
import Mesh as Mesh
import Data.Vector (Vec(..))
import Data.Vector2 (Vec2, get2X, get2Y)


--shared bottom_left
data Line
    = Horizontal { left   :: Vec2 Number, width  :: Number }
    | Vertical   { bottom :: Vec2 Number, height :: Number }

mesh :: forall eff. Eff (webgl :: WebGl | eff) Mesh.Mesh
mesh = Mesh.mesh triangle_data color_data

color_data :: Array Number
color_data = Array.concat $ Unfoldable.replicate (Array.length triangle_data / 2) [0.6, 0.3, 0.3]

triangle_data :: Array Number
triangle_data = Array.concatMap to_triangle_data geometry

--small inconsistency
--horizontal.left.x is the top of the line
--vertical.bottom.y is the middle of the line
to_triangle_data :: Line -> Array Number
to_triangle_data = let t = 0.1 in case _ of
    Horizontal { left, width } -> let
        x = get2X left
        y = get2Y left
        in
        [ x + width, y
        , x        , y
        , x + width, y - t
        , x        , y - t
        , x + width, y - t
        , x        , y ]
    Vertical { bottom, height } -> let
        x = get2X bottom
        y = get2Y bottom
        s = 0.5 * t
        in
        [ x + s, y
        , x + s, y + height
        , x - s, y
        , x + s, y + height
        , x - s, y
        , x - s, y + height ]

geometry :: Array Line
geometry = let
    l = 10.0
    h = 10.0
    in
    [ Horizontal { left: Vec [-l, 0.0], width: 2.0 * l }
    , Horizontal { left: Vec [0.0,  -h], width: 2.0 * l }
    , Horizontal { left: Vec [2.0 * l, 0.0], width: l }
    , Vertical { bottom: Vec [-l, 0.0], height: h }
    , Vertical { bottom: Vec [2.0 * l, -h], height: h }
    , Vertical { bottom: Vec [3.0 * l, -h], height: h } ]
