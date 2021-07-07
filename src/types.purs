module Types (SkinningBindings, GridBindings, FlatBindings, GridState, Joint, Milliseconds(..), fromMilliseconds, toMilliseconds, Command(..), HorizontalDirection(..)) where

import Prelude
import Graphics.WebGL (Attribute, Uniform, Buffer)
import Graphics.WebGL as GL
import Graphics.WebGLRaw (WebGLProgram)
import Data.Maybe (Maybe)
import Data.ArrayBuffer.Types as T
import Data.Int (round, toNumber)
import Data.Matrix4 (Mat4)
import Data.Vector2 (Vec2)


type SkinningBindings =
    { program              :: WebGLProgram
    , a2position           :: Attribute GL.Vec3
    , a3color              :: Attribute GL.Vec3
    , a1skinning_index     :: Attribute GL.Float
    , u44skinning_matrices :: Uniform   GL.Mat4 }

type GridBindings =
    { program       :: WebGLProgram
    , a2position    :: Attribute GL.Vec2
    , u2resolution  :: Uniform   GL.Vec2
    , u2screen_size :: Uniform   GL.Vec2 }

type FlatBindings =
    { program    :: WebGLProgram
    , a2position :: Attribute GL.Vec2
    , a3color    :: Attribute GL.Vec3
    , u44mvp     :: Uniform   GL.Mat4 }

type GridState = { position :: Buffer T.Float32 }

data HorizontalDirection
    = Left
    | Right

--derive instance eqFacingDirection :: Eq FacingDirection
--derive instance ordFacingDirection :: Ord FacingDirection

type Joint =
    { child_indices          :: Array Int
    , inverse_bind_transform :: Mat4
    , translation            :: Vec2 Number }

data Command
    = MoveCmd (Maybe HorizontalDirection)
    | JumpCmd (Maybe Number)
    | AttackCmd Number--Duration
    | PassTimeCmd Number

------------Milliseconds
newtype Milliseconds = Milliseconds Int

derive instance eqMilliseconds :: Eq Milliseconds
derive instance ordMilliseconds :: Ord Milliseconds
instance semiringMilliseconds :: Semiring Milliseconds where
    add (Milliseconds a) (Milliseconds b) = Milliseconds (a + b)
    zero = Milliseconds 0
    mul (Milliseconds a) (Milliseconds b) = Milliseconds (a * b)
    one = Milliseconds 1
instance ringMilliseconds :: Ring Milliseconds where
    sub (Milliseconds a) (Milliseconds b) = Milliseconds (a - b)

toMilliseconds :: Number -> Milliseconds
toMilliseconds x = Milliseconds $ round $ 1000.0 * x

fromMilliseconds :: Milliseconds -> Number
fromMilliseconds (Milliseconds x) = 0.001 * toNumber x

--toFrames :: Number -> Frames
--toFrames x = Milliseconds $ round $ x * toNumber ticks_per_second
--fromFrames :: Frames -> Number
--fromFrames (Frames x) = toNumber x / toNumber ticks_per_second
