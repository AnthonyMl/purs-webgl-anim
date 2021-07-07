module Utility (extend, truncate) where

import Prelude
import Data.Array (snoc, dropEnd)
import Data.Vector (Vec(..))
import Data.Vector2 (Vec2)
import Data.Vector3 (Vec3)


extend :: Vec2 Number -> Vec3 Number
extend (Vec v) = Vec $ snoc v 0.0

truncate :: forall a. Vec3 a -> Vec2 a
truncate (Vec v) = Vec $ dropEnd 1 v
