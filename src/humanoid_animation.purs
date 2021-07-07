module HumanoidAnimation (animation_state, transition) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Animation as Animation
import Animation (AnimationClip, Transition)
import HumanoidSkeleton as Skeleton
import HumanoidIdleClip as IdleClip
import HumanoidRunClip as RunClip
import HumanoidJumpClip as JumpClip
import HumanoidFallingClip as FallingClip
import HumanoidTypes (HumanoidState, ActionState(..))


--TODO: make this a lot less error prone
to_clip :: ActionState -> AnimationClip
to_clip = case _ of
    Idle    -> IdleClip.clip 4
    Running -> RunClip.clip  0
    Jumping -> JumpClip.clip 5
    Falling -> FallingClip.clip 7

animation_state :: { static :: Animation.StaticState, dynamic :: Animation.DynamicState }
animation_state = let
    joints = Skeleton.joints

    poses = Array.concat [ RunClip.poses, IdleClip.poses, JumpClip.poses, FallingClip.poses ]--TODO: map with enum or something for safety

    static = { joints, poses }
    dynamic =
        { clip: to_clip
        , transition: Nothing
        , position: 0.0 }
    in
    { static, dynamic }

{--
do_transition :: HumanoidState -> HumanoidState -> Animation.DynamicState -> Maybe Transition
do_transition a a' d = case a.action, a'.action of
    Running, Idle -> Just { from: d.state, duration: 0.4, frozen_position: d.position }
    _      , _    -> Nothing
--}

--all transitions take 0.2s and start at 0.0
transition :: HumanoidState -> HumanoidState -> Animation.DynamicState -> Maybe Transition
transition a a' d = if a.action == a'.action then Nothing else Just { from: a.action, duration: 0.2, frozen_position: d.position }
