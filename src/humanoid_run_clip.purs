module HumanoidRunClip (clip, is_pass, poses) where

import Prelude
import Data.Enum (class Enum, class BoundedEnum, defaultCardinality, defaultFromEnum, defaultToEnum, fromEnum, defaultSucc, defaultPred)
import Data.Maybe (Maybe(..))
import Animation (AnimationClip, AnimationPose, EndTransition(..), PoseSequence)
import Constants (max_horizontal_speed)
import Data.Foldable (sum)
import Data.Functor (mapFlipped)
import HumanoidSkeleton (BoneName(..), mirror, all_bones)
import HumanoidTypes (HumanoidState)
import Math (pi, cos)
import Types (toMilliseconds, fromMilliseconds)


data RunPose
    = Pass_A
    | Pass_B
    | Reach_A
    | Reach_B

is_pass :: Int -> Boolean
is_pass x = x == fromEnum Pass_A || x == fromEnum Pass_B

derive instance eqRunPose :: Eq RunPose
derive instance ordRunPose :: Ord RunPose

instance enumRunPose :: Enum RunPose where
    succ = defaultSucc toEnumRunPose fromEnumRunPose
    pred = defaultPred toEnumRunPose fromEnumRunPose

toEnumRunPose :: Int -> Maybe RunPose
toEnumRunPose = case _ of
    0  -> Just Pass_A
    1  -> Just Pass_B
    2  -> Just Reach_A
    3  -> Just Reach_B
    _  -> Nothing

fromEnumRunPose :: RunPose -> Int
fromEnumRunPose = case _ of
    Pass_A  -> 0
    Pass_B  -> 1
    Reach_A -> 2
    Reach_B -> 3

instance boundedRunPose :: Bounded RunPose where
    bottom = Pass_A
    top = Reach_B

instance boundedEnumRunPose :: BoundedEnum RunPose where
    cardinality = defaultCardinality
    toEnum = defaultToEnum
    fromEnum = defaultFromEnum

reach_a :: BoneName -> Number
reach_a = let
    reach_arm_angle = pi / 3.5
    reach_forward_thigh_angle = -pi / 2.4
    reach_backward_thigh_angle = pi / 5.0
    reach_backward_shin_angle = pi / 2.5
    reach_forward_shin_angle = pi / 3.5
    in case _ of
    Arm_L     -> -reach_arm_angle
    Arm_R     ->  reach_arm_angle
    Forearm_L -> -pi / 2.0
    Forearm_R -> -pi / 6.0
    Thigh_L   ->  reach_backward_thigh_angle
    Thigh_R   ->  reach_forward_thigh_angle
    Shin_L    ->  reach_backward_shin_angle
    Shin_R    ->  reach_forward_shin_angle
    _         ->  0.0

reach_b :: BoneName -> Number
reach_b = mirror reach_a

pass_a :: BoneName -> Number
pass_a = case _ of
    Shin_R -> pi / 2.0
    _ -> 0.0

pass_b :: BoneName -> Number
pass_b = mirror pass_a

run_sequence :: Int -> PoseSequence
run_sequence base = let
    stride_distance = 6.0
    half_duration = 0.5 * stride_distance / max_horizontal_speed
    in
    [ { pose_index: base + fromEnum Pass_A , duration: toMilliseconds half_duration }
    , { pose_index: base + fromEnum Reach_A, duration: toMilliseconds half_duration }
    , { pose_index: base + fromEnum Pass_B , duration: toMilliseconds half_duration }
    , { pose_index: base + fromEnum Reach_B, duration: toMilliseconds half_duration } ]

clip :: Int -> AnimationClip
clip base =
    { sequence: run_sequence base
    , duration: sum $ map _.duration (run_sequence base)
    , end_transition: Loop
    , hips_translation: bounce (fromMilliseconds $ sum $ map _.duration (run_sequence base)) }

--TODO: make 0.15 based on velocity / a named constant
bounce :: Number -> HumanoidState -> Number -> Number
bounce duration _ position = 0.18 * (1.0 - cos (4.0 * pi * position / duration))

poses :: Array AnimationPose
poses = map (mapFlipped all_bones) [ pass_a, pass_b, reach_a, reach_b ]
