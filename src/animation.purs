module Animation (skinning_matrices, StaticState, DynamicState, Transition, AnimationClip(..), EndTransition(..), AnimationPose, PoseSequence, SequenceElement, animate, animation_pose) where

import Prelude
import Data.Array as Array
import Data.Array (unsafeIndex)
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
import Data.List as List
import Data.List (List(..))
import Data.Matrix4 (Mat4, identity, makeRotate, makeTranslate, mulM)
import Partial.Unsafe (unsafePartial)
import Types (Joint, Milliseconds(..), fromMilliseconds, toMilliseconds)
import Math (abs)
import Utility (extend)
import Constants (global_look)
import HumanoidTypes (HumanoidState, ActionState)


type StaticState =
    { joints :: Array Joint
    , poses  :: Array AnimationPose }

type DynamicState =
    { clip       :: ActionState -> AnimationClip--TODO: this is static
    , transition :: Maybe Transition
    , position   :: Number }

type Transition =
    { from            :: ActionState
    , duration        :: Number
    , frozen_position :: Number }

type AnimationPose = Array Number

data EndTransition
    = Loop
    | Freeze

--duration could be a Maybe since not all sequences are looping
type AnimationClip =
    { sequence         :: PoseSequence
    , duration         :: Milliseconds
    , end_transition   :: EndTransition
    , hips_translation :: HumanoidState -> Number -> Number }

type PoseSequence = Array SequenceElement

type SequenceElement =
    { pose_index :: Int
    , duration   :: Milliseconds }

--animation_pose
--TODO: allow custom functions and not just lerp
    --TODO: use a ring buffer or something
    --TODO: do not assume a looping clip

--TODO: figure out a better way to flatten the hierarchy into an array
skinning_matrix :: Mat4 -> Array Number -> Array Joint -> Int -> List {i :: Int, s :: Mat4}
skinning_matrix parent_transform angles joints index = let
    angle = unsafePartial $ unsafeIndex angles index
    joint = unsafePartial $ unsafeIndex joints index

    rotation = makeRotate angle global_look
    translation = makeTranslate $ extend joint.translation

    parent_x_rotation = mulM parent_transform rotation
    world = mulM parent_x_rotation translation
    skinning = mulM parent_x_rotation joint.inverse_bind_transform
    children_data = List.concatMap (skinning_matrix world angles joints) (List.fromFoldable joint.child_indices)
    in
    Cons {i: index, s: skinning} children_data

skinning_matrices :: AnimationPose -> Array Joint -> Array Mat4
skinning_matrices pose joints = let
    list_form = skinning_matrix identity pose joints 0
    array_form = Array.fromFoldable $ List.sortBy (Ord.comparing _.i) list_form
    in
    map _.s array_form

pose_index :: AnimationClip -> Milliseconds -> Int
pose_index clip position = let
    loop p i = let
        e = unsafePartial $ unsafeIndex clip.sequence i
        in
        if p < e.duration then e.pose_index else loop (p - e.duration) (i + 1)
    in
    loop position 0

animate :: DynamicState -> HumanoidState -> Number -> DynamicState
animate state hs delta = let
    position = state.position + abs delta
    clip = state.clip hs.action
    i_p = toMilliseconds position

    i_p' = if i_p < clip.duration then i_p else
        case clip.end_transition of
        Freeze -> clip.duration - (Milliseconds 1)--TODO: do we need this -1 (does it even help enough if we are converting back to fp)
        Loop -> i_p - clip.duration

    position' = fromMilliseconds i_p'

    transition' = case state.transition of
        Nothing -> Nothing
        Just transition -> if position' > transition.duration then Nothing else Just transition
    in
    state { position = position', transition = transition' }

get_pose :: Array AnimationPose -> Number -> AnimationClip -> AnimationPose
get_pose poses position clip = let
    num_poses = Array.length clip.sequence

    {index, base} = base_index position clip.sequence 0 (Milliseconds 0)

    sequence_index_b = if index /= num_poses - 1 then index + 1 else
        case clip.end_transition of
            Freeze -> index
            Loop ->   0

    element_a = unsafePartial $ unsafeIndex clip.sequence index
    element_b = unsafePartial $ unsafeIndex clip.sequence sequence_index_b

    t = (position - fromMilliseconds base) / fromMilliseconds element_a.duration

    a = unsafePartial $ unsafeIndex poses element_a.pose_index
    b = unsafePartial $ unsafeIndex poses element_b.pose_index
    in
    Array.zipWith (+) (map ((*) (1.0 - t)) a) (map ((*) t) b)

base_index :: Number -> PoseSequence -> Int -> Milliseconds -> { index :: Int, base :: Milliseconds }
base_index position sequence index sum = let
    element = unsafePartial $ unsafeIndex sequence index
    upper = sum + element.duration
    in
    if position <= fromMilliseconds upper then { index, base: sum }
    else base_index position sequence (index + 1) upper

animation_pose :: Array AnimationPose -> DynamicState -> ActionState -> AnimationPose
animation_pose poses ds action_state = case ds.transition of
    Just transition -> let
        clip_a = ds.clip transition.from
        clip_b = ds.clip action_state
        pose_a = get_pose poses transition.frozen_position clip_a
        pose_b = get_pose poses ds.position clip_b
        t = ds.position / transition.duration
        in
        Array.zipWith (+) (map ((*) (1.0 - t)) pose_a) (map ((*) t) pose_b)
    Nothing ->
        get_pose poses ds.position $ ds.clip action_state
