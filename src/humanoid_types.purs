module HumanoidTypes where

import Prelude
import Types (HorizontalDirection)
import Data.Vector2 (Vec2)


type HumanoidState =
    { action   :: ActionState
    , attack   :: AttackState
    , facing   :: HorizontalDirection
    , velocity :: Vec2 Number
    , position :: Vec2 Number }

data ActionState
    = Idle
    | Running
    | Jumping
    | Falling

instance eqActionState :: Eq ActionState where
    eq a b = case a, b of
        Jumping, Jumping -> true
        Idle,    Idle    -> true
        Running, Running -> true
        Falling, Falling -> true
        _, _             -> false

instance showActionState :: Show ActionState where
    show = case _ of
        Idle    -> "Idle"
        Running -> "Running"
        Jumping -> "Jumping"
        Falling -> "Falling"

data AttackState
    = Yes Number--Duration (1.0 and decrease ???)
    | No

instance showAttackState :: Show AttackState where
    show = case _ of
        Yes duration -> "Attacking " <> show duration
        No           -> "NotAttacking"
