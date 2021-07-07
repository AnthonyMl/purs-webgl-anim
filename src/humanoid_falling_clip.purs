module HumanoidFallingClip (poses, clip) where

import Prelude
import Math (pi)
import Data.Foldable (sum)
import Data.Functor (mapFlipped)
import Animation (PoseSequence, AnimationClip, EndTransition(..), AnimationPose)
import HumanoidSkeleton (BoneName(..), all_bones)
import Types (toMilliseconds)


falling :: BoneName -> Number
falling = let
    thighs   = -pi / 8.0
    shins    =  pi / 12.0
    arms     =  pi / 8.0
    forearms = -pi / 12.0
    in case _ of
--	Hips      -> -pi / 16.0
    Thigh_L   -> thighs
    Thigh_R   -> thighs * 1.3
    Shin_L    -> shins * 1.3
    Shin_R    -> shins
    Arm_L     -> arms
    Arm_R     -> -arms
    Forearm_L -> forearms
    Forearm_R -> forearms
    _         -> 0.0

falling_sequence :: Int -> PoseSequence
falling_sequence base = [ { pose_index: base, duration: toMilliseconds 100.0 } ]

--TODO can we shorten this
poses :: Array AnimationPose
poses = map (mapFlipped all_bones) [ falling ]

clip :: Int -> AnimationClip
clip base = let
    sequence = falling_sequence base
    in
    { sequence
    , duration: sum $ map _.duration sequence
    , end_transition: Freeze
    , hips_translation: (\_ _->0.0) }--TODO: use a maybe for the hips_translation
