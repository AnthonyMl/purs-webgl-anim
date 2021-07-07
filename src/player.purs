module Player (initial_state) where

import Data.Vector (Vec(..))
import HumanoidTypes (HumanoidState, ActionState(..), AttackState(..))
import Types (HorizontalDirection(..))


initial_state :: HumanoidState
initial_state = { action: Falling, attack: No, position: Vec [0.0, 6.0], velocity: Vec [0.0, 0.0], facing: Right }
