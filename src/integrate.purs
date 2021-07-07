module Integrate (integrate_humanoid) where

import Prelude
import HumanoidTypes (HumanoidState)
import Data.Vector (scale)
import Data.Vector2 (Vec2)
import Constants (dt, gravity, global_up)
import Utility (truncate)


integrate_humanoid :: HumanoidState -> HumanoidState
integrate_humanoid = integrate_rk4

--all methods assume constant acceleration (and only in the vertical component)
acceleration :: Vec2 Number
acceleration = scale gravity $ truncate global_up

integrate_euler :: HumanoidState -> HumanoidState
integrate_euler state = let
    velocity = state.velocity + (scale dt acceleration)
    position = state.position + (scale dt velocity)
    in
    state { position = position, velocity = velocity }

integrate_verlet :: HumanoidState -> HumanoidState
integrate_verlet state = let
    velocity = state.velocity + (scale dt acceleration)
    position = state.position + (scale dt velocity) + (scale (0.5 * dt * dt) acceleration)
    in
    state { position = position, velocity = velocity }

integrate_rk4 :: HumanoidState -> HumanoidState
integrate_rk4 state = let
    pa  = state.velocity
    pbc = state.velocity + scale (0.5 * dt) acceleration
    pd  = state.velocity + scale        dt  acceleration

    dxdt = scale (1.0/6.0) (pa + (scale 4.0 pbc) + pd)

    position = state.position + (scale dt dxdt)
    velocity = state.velocity + (scale dt acceleration)
    in
    state { position = position, velocity = velocity }
