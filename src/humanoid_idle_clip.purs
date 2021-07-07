module HumanoidIdleClip (clip, poses) where

import Prelude
import Data.Foldable (sum)
import Data.Functor (mapFlipped)
import Animation (AnimationClip, AnimationPose, EndTransition(..), PoseSequence)
import HumanoidSkeleton (BoneName, all_bones)
import Types (toMilliseconds)


idle :: BoneName -> Number
idle _ = 0.0

idle_sequence :: Int -> PoseSequence
idle_sequence base = [ { pose_index: base, duration: toMilliseconds 1.0 } ]--TODO: give the literal a name

clip :: Int -> AnimationClip
clip base =
    { sequence: idle_sequence base
    , duration: sum $ map _.duration $ idle_sequence base
    , end_transition: Freeze
    , hips_translation: (\_ _->0.0) }

poses :: Array AnimationPose
poses = map (mapFlipped all_bones) [ idle ]
