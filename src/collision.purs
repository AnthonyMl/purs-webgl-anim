module Collision (collide_and_integrate_humanoid) where

import Prelude
import Data.Tuple (Tuple(..), snd)
import Data.Foldable (foldr)
import Integrate (integrate_humanoid)
import Background (Line(..), geometry)
import Data.Vector (Vec(..))
import Data.Vector2 (get2X, get2Y)
import BoundingVolumes (Box)
import HumanoidTypes (HumanoidState)


collide_and_integrate_humanoid :: Box -> HumanoidState -> HumanoidState
collide_and_integrate_humanoid bounding_box state = let
    state' = integrate_humanoid state
    in
    snd $ foldr (collide_line bounding_box) (Tuple state state') geometry

collide_line :: Box -> Line -> Tuple HumanoidState HumanoidState -> Tuple HumanoidState HumanoidState
collide_line bounding_box line (Tuple a b) = let
    x  = get2X a.position
    x' = get2X b.position
    y  = get2Y a.position
    y' = get2Y b.position
    in case line of
    Horizontal { left, width } -> let
        lx = get2X left - bounding_box.left
        ly = get2Y left - bounding_box.bottom
        cond = x' + 2.0 * bounding_box.right > lx && x' < lx + width && y >= ly && y' <= ly
        Tuple py vy = if cond then Tuple ly 0.0 else Tuple y' $ get2Y b.velocity
        in
        Tuple a b { position = Vec [x', py], velocity = Vec [get2X b.velocity, vy] }
    Vertical { bottom, height } -> let
        lx = get2X bottom - 1.01 * bounding_box.right
        ly = get2Y bottom - bounding_box.bottom
        --RIGHT
        cond  = y >= ly && y < ly + height && x <= lx && x' >= lx
        Tuple px vx = if cond then Tuple lx 0.0 else Tuple x' $ get2X b.velocity
        --LEFT
        lx' = get2X bottom - 1.01 * bounding_box.left
        cond' = y >= ly && y < ly + height && x >= lx' && px <= lx'
        Tuple px' vx' = if cond' then Tuple lx' 0.0 else Tuple px vx
        in
        Tuple a b { position = Vec [px', y'], velocity = Vec [vx', get2Y b.velocity] }
