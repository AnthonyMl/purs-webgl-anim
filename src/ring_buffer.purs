module RingBuffer (RingBuffer, make, push, get, count_while) where

import Prelude
import Data.Array as Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)


type RingBuffer a =
    { buffer   :: Array a
    , position :: Int }

make :: forall a. Int -> a -> RingBuffer a
make size initial_value = { buffer: Array.replicate size initial_value, position: 0 }

push :: forall a. RingBuffer a -> a -> RingBuffer a
push { buffer, position } x =
    let position' = mod (position + 1) (Array.length buffer) in
    { buffer: unsafePartial $ fromJust $ Array.updateAt position' x buffer
    , position: position' }

get :: forall a. RingBuffer a -> a
get { buffer, position } = unsafePartial $ Array.unsafeIndex buffer position

move_back :: forall a. RingBuffer a -> RingBuffer a
move_back r = let
    position = if r.position == 0 then Array.length r.buffer - 1 else r.position - 1
    in
    r { position = position }

count_while :: forall a. RingBuffer a -> (a -> Boolean) -> Int
count_while ring_buffer predicate = let
    lmao rb i = if rb.position /= ring_buffer.position && predicate (get rb) then lmao (move_back rb) (i+1) else i
    in
    if predicate (get ring_buffer) then lmao (move_back ring_buffer) 1 else 0

