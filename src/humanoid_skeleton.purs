module HumanoidSkeleton (BoneName(..), mirror, all_bones, joints, color_data, triangle_data, skin_data, skinning_mesh, bounding_box, bounding_box_mesh) where

import Prelude
import BoundingVolumes (Box)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Data.Ord as Ord
import Data.Enum (class Enum, class BoundedEnum, defaultCardinality, defaultFromEnum, defaultToEnum, fromEnum, enumFromTo, defaultSucc, defaultPred)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Unfoldable (class Unfoldable, replicate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Matrix4 (Mat4, identity, makeTranslate, mulM, transform, inverseOrthonormal)
import Data.Vector (Vec(..), vlength, normalize, toArray, scale)
import Data.Vector2 (Vec2)
import Data.Vector3 (cross)
import Mesh as Mesh
import Utility (extend, truncate)
import Constants (global_look)
import Types (Joint)


all_bones :: forall f a. Unfoldable f => Functor f => BoundedEnum a => f a
all_bones = enumFromTo bottom top

mirror :: (BoneName -> Number) -> BoneName -> Number
mirror f = case _ of
    Arm_L     -> negate $ f Arm_L
    Arm_R     -> negate $ f Arm_R
    Forearm_L -> f Forearm_R
    Forearm_R -> f Forearm_L
    Thigh_L   -> f Thigh_R
    Thigh_R   -> f Thigh_L
    Shin_L    -> f Shin_R
    Shin_R    -> f Shin_L
    x         -> f x

type Bone =
    { end      :: Vec2 Number
    , children :: Array Int }

data BoneName
    = Hips
    | Spine_0
    | Spine_1
    | Arm_L
    | Forearm_L
    | Thigh_L
    | Shin_L
    | Arm_R
    | Forearm_R
    | Thigh_R
    | Shin_R

derive instance eqBoneName :: Eq BoneName
derive instance ordBoneName :: Ord BoneName

instance enumBoneName :: Enum BoneName where
    succ = defaultSucc toEnumBoneName fromEnumBoneName
    pred = defaultPred toEnumBoneName fromEnumBoneName

toEnumBoneName :: Int -> Maybe BoneName
toEnumBoneName = case _ of
    0  -> Just Hips
    1  -> Just Spine_0
    2  -> Just Spine_1
    3  -> Just Arm_L
    4  -> Just Forearm_L
    5  -> Just Thigh_L
    6  -> Just Shin_L
    7  -> Just Arm_R
    8  -> Just Forearm_R
    9  -> Just Thigh_R
    10 -> Just Shin_R
    _  -> Nothing

fromEnumBoneName :: BoneName -> Int
fromEnumBoneName = case _ of
    Hips      ->  0
    Spine_0   ->  1
    Spine_1   ->  2
    Arm_L     ->  3
    Forearm_L ->  4
    Thigh_L   ->  5
    Shin_L    ->  6
    Arm_R     ->  7
    Forearm_R ->  8
    Thigh_R   ->  9
    Shin_R    -> 10

instance boundedBoneName :: Bounded BoneName where
    bottom = Hips
    top = Shin_R

instance boundedEnumBoneName :: BoundedEnum BoneName where
    cardinality = defaultCardinality
    toEnum = defaultToEnum
    fromEnum = defaultFromEnum

child_names :: BoneName -> Array BoneName
child_names = case _ of
    Hips    -> [Spine_0, Thigh_L, Thigh_R]
    Spine_0 -> [Spine_1]
    Spine_1 -> [Arm_L, Arm_R]
    Arm_L   -> [Forearm_L]
    Thigh_L -> [Shin_L]
    Arm_R   -> [Forearm_R]
    Thigh_R -> [Shin_R]
    _       -> []

end :: BoneName -> Vec2 Number
end = case _ of
    Hips      -> Vec [ 0.0,  0.0]
    Spine_0   -> Vec [ 0.0,  1.0]
    Spine_1   -> Vec [ 0.0,  1.0]
    Arm_L     -> Vec [ 0.0, -1.5]
    Arm_R     -> Vec [ 0.0, -1.5]
    Forearm_L -> Vec [ 0.0, -1.0]
    Forearm_R -> Vec [ 0.0, -1.0]
    Thigh_L   -> Vec [ 0.0, -1.5]
    Thigh_R   -> Vec [ 0.0, -1.5]
    Shin_L    -> Vec [ 0.0, -1.5]
    Shin_R    -> Vec [ 0.0, -1.5]

type StaticJointData =
    { joint            :: Joint
    , triangles        :: Array Number
    , skinning_indices :: Array Number }

to_joint :: Mat4 -> BoneName -> List {i :: Int, l :: StaticJointData}
to_joint parent_transform name = let
    bone_end = end name
    bone_children_names = child_names name
    local_transform = mulM parent_transform $ makeTranslate $ extend bone_end
    bone_length = vlength bone_end
    bone_end_3d = extend bone_end
    joint =
        { translation: bone_end
        , inverse_bind_transform: inverseOrthonormal parent_transform
        , child_indices: map fromEnum bone_children_names }
    triangles = if bone_length == 0.0 then [] else let
        width = 0.2
        tangent = normalize $ cross bone_end_3d global_look
        points = map (truncate <<< transform parent_transform) [scale width tangent, bone_end_3d, scale (-width) tangent]
        in
        Array.concatMap toArray points
    skinning_indices = if bone_length == 0.0 then [] else map (Int.toNumber <<< fromEnum) (Array.replicate 3 name)
    children = List.concatMap (to_joint local_transform) (List.fromFoldable bone_children_names)
    in
    Cons {i: fromEnum name, l: {joint, triangles, skinning_indices}} children

static_joint_data :: Array StaticJointData
static_joint_data = let
    paired_list = to_joint identity Hips
    sorted_array = Array.fromFoldable $ List.sortBy (Ord.comparing _.i) paired_list
    in
    map _.l sorted_array

joints :: Array Joint
joints = map _.joint static_joint_data

color_data :: Array Number
color_data = Array.concatMap Array.concat [
    Array.replicate  6 [0.3, 0.7, 0.3],
    Array.replicate 12 [0.3, 0.3, 0.7],
    Array.replicate 12 [0.7, 0.3, 0.3] ]

triangle_data :: Array Number
triangle_data = Array.concatMap _.triangles static_joint_data

skin_data :: Array Number
skin_data = Array.concatMap _.skinning_indices static_joint_data

skinning_mesh :: forall eff. Eff (webgl :: WebGl | eff) Mesh.SkinningMesh
skinning_mesh = Mesh.skinning_mesh triangle_data color_data skin_data

bounding_box :: Box
bounding_box =
    { bottom: -3.0
    , left:   -1.0
    , top:     2.0
    , right:   1.0 }

box :: Number -> Number -> Number -> Number -> Array Number
box b l t r = [
    r, b,
    r, t,
    l, b,
    l, b,
    r, t,
    l, t ]

bb_triangle_data :: Array Number
bb_triangle_data = let w = 0.06 in Array.concat
    [ box bounding_box.bottom bounding_box.left (bounding_box.bottom + w) bounding_box.right
    , box bounding_box.bottom bounding_box.left bounding_box.top (bounding_box.left + w)
    , box bounding_box.bottom (bounding_box.right - w) bounding_box.top bounding_box.right
    , box (bounding_box.top - w) bounding_box.left bounding_box.top bounding_box.right ]

bb_color_data :: Array Number
bb_color_data = Array.concat $ replicate (Array.length bb_triangle_data / 2) [0.7, 0.7, 0.7]

bounding_box_mesh :: forall eff. Eff (webgl :: WebGl | eff) Mesh.Mesh
bounding_box_mesh = Mesh.mesh bb_triangle_data bb_color_data
