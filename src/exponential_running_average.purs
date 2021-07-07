module ExponentialRunningAverage where

import Prelude


type ExponentialRunningAverage =
    { alpha :: Number
    , sum   :: Number }

update :: ExponentialRunningAverage -> Number -> ExponentialRunningAverage
update era new = era { sum = era.alpha * new + (1.0 - era.alpha) * era.sum }
