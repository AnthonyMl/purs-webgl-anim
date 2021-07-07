module HumanoidState (not_like_this) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Types (Command(..), HorizontalDirection(..))
import Data.Vector (Vec(..))
import Data.Vector2 (Vec2, get2X, get2Y)
import Constants (max_horizontal_speed, jump_impulse, dt)
import Collision (collide_and_integrate_humanoid)
import HumanoidTypes (HumanoidState, ActionState(..), AttackState(..))
import HumanoidSkeleton as HumanoidSkeleton


speed :: HorizontalDirection -> Number
speed = case _ of
    Left  -> -max_horizontal_speed
    Right ->  max_horizontal_speed

limit :: Number -> Number
limit vx = max (min vx max_horizontal_speed) (-max_horizontal_speed)

limit_x :: Vec2 Number -> Vec2 Number
limit_x v = Vec [limit $ get2X v, get2Y v]

pass_time :: HumanoidState -> Number -> HumanoidState
pass_time state@{ action, attack } dt = let
    attack' = case attack of
        Yes t -> let t' = t - dt in if t' < 0.0 then No else Yes t'
        No -> No
    state' = collide_and_integrate_humanoid HumanoidSkeleton.bounding_box state
    vx = get2X state'.velocity
    vy = get2Y state'.velocity
    Tuple vx' action' = case action of
        Running -> Tuple vx $
            if vy < 0.0 then Falling else
            if vx == 0.0 then Idle else Running
        Jumping -> Tuple vx $
            if vy <= 0.0 then Falling else action
        Falling ->
            if vy == 0.0 then Tuple 0.0 Idle else Tuple vx Falling
        a -> Tuple vx a
    in
    state' { action = action', attack = attack', velocity = Vec [vx', vy] }

jump_hold_threshold :: Number
jump_hold_threshold = dt * 5.0

not_like_this :: Command -> HumanoidState -> HumanoidState
not_like_this cmd state@{ action, attack, position, velocity } = case action, attack, cmd of
    _, _, PassTimeCmd dt -> pass_time state dt

    Running, No, AttackCmd t -> state { attack = Yes t, action = Idle, velocity = Vec [0.0, 0.0] }
    _      , No, AttackCmd t -> state { attack = Yes t }

    Running, No, JumpCmd (Just t) | t < jump_hold_threshold -> state { action = Jumping, velocity = velocity + jump_impulse }
    Idle   , No, JumpCmd (Just t) | t < jump_hold_threshold -> state { action = Jumping, velocity = velocity + jump_impulse }
    Jumping,  _, JumpCmd Nothing -> state { velocity = Vec [get2X velocity, 0.0] }

    Running, No, MoveCmd (Just dir) -> state { facing = dir, velocity = Vec [ speed dir, 0.0 ] }
    Idle   , No, MoveCmd (Just dir) -> state { facing = dir, velocity = Vec [ speed dir, 0.0 ], action = Running }
    Jumping,  _, MoveCmd (Just dir) -> state { velocity = limit_x $ velocity + Vec [ speed dir, 0.0 ] }
    Jumping,  _, MoveCmd Nothing    -> state
    Falling,  _, MoveCmd (Just dir) -> state { velocity = limit_x $ velocity + Vec [ speed dir, 0.0 ] }
    Falling,  _, MoveCmd Nothing    -> state
    _      ,  _, MoveCmd Nothing    -> state { action = Idle, velocity = Vec [0.0, 0.0] }

    _, _, _ -> state
