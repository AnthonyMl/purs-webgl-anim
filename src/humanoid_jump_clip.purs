module HumanoidJumpClip (clip, poses) where

import Prelude
import Math (pi)
import Data.Foldable (sum)
import Data.Functor (mapFlipped)
import Data.Vector2 (get2Y)
import Animation (PoseSequence, AnimationClip, EndTransition(..), AnimationPose)
import HumanoidSkeleton (BoneName(..), all_bones)
import HumanoidTypes (HumanoidState)
import Types (toMilliseconds)
import Constants (jump_impulse, gravity, max_jump_time)


data JumpPose
    = Anticipation
    | Jump

anticipation :: BoneName -> Number
anticipation = let
    thighs   = -pi / 2.0
    shins    =  pi / 2.0
    arms     =  pi / 4.0
    forearms = -pi / 4.0
    in case _ of
    Thigh_L   -> thighs
    Thigh_R   -> thighs
    Shin_L    -> shins
    Shin_R    -> shins
    Arm_L     -> -arms
    Arm_R     -> arms
    Forearm_L -> forearms
    Forearm_R -> forearms
    _ -> 0.0

jump :: BoneName -> Number
jump = let
    arms     =  pi / 20.0
    forearms = -pi / 20.0
    thighs   = -pi / 20.0
    shins    =  pi / 20.0
    in case _ of
    Arm_L     -> arms
    Arm_R     -> -arms
    Forearm_L -> forearms
    Forearm_R -> forearms
    Thigh_L   -> thighs * 1.3
    Thigh_R   -> thighs
    Shin_L    -> shins
    Shin_R    -> shins * 1.3
    _         -> 0.0

durations :: Int -> Number
durations 0 = 0.15 * max_jump_time
durations 1 = 0.3 * max_jump_time
durations _ = 9999.9

jump_sequence :: Int -> PoseSequence
jump_sequence base  =
    [ { pose_index: base,     duration: toMilliseconds $ durations 0 }
    , { pose_index: base + 1, duration: toMilliseconds $ durations 1 } ]

clip :: Int -> AnimationClip
clip base = let
    sequence = jump_sequence base
    in
    { sequence
    , duration: sum $ map _.duration sequence
    , end_transition: Freeze
    , hips_translation: hips_offset }

hips_offset :: HumanoidState -> Number -> Number
hips_offset state t = let
    impulse = get2Y jump_impulse
    bp0 = durations 0
    bp1 = bp0 * 2.0
    t' = (t - bp0) / (bp1 - bp0)
    velocity = impulse + t * gravity
    position = t * velocity + 0.5 * t * t * gravity
    in
    if t < bp0 then -position else
    if t < bp1 then -position * (1.0 - t')
    else 0.0

poses :: Array AnimationPose
poses = map (mapFlipped all_bones) [ anticipation, jump ]


{--
keep dynamic list of trees/tries/fsm for possible combos
if a leaf node is reached then start that action
    how do we cancel actions/start new ones
need an indicator of how long something was held down (timestamp/frame_id)
a combination of actions supercedes the individual presses (priority of trees)
climb the trees in reverse
    the action button and then the required prerequisites backward in time
--}




