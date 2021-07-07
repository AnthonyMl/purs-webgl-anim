module Input (InputState, Input, InputEvent, KeyPress, keyUp, keyLeft, keyRight, update_state, register_keyboard, interpret, InputFrame) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, modifyRef, newRef)
import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import Data.Array (snoc)
import Data.List as List
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Set (Set)
import Data.Int (toNumber)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventType(..))
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML.Types (windowToEventTarget, Window)
import Types (Command(..), HorizontalDirection(..))
import RingBuffer (RingBuffer)
import RingBuffer as RingBuffer
import Constants (ticks_per_second, dt)


data KeyPress = Down | Up

derive instance eqKeyPress :: Eq KeyPress

type InputEvent =
    { press_type :: KeyPress
    , character  :: String }

type Input = List InputEvent

type InputFrame =
    { down     :: Set String
    , up       :: Set String
    , frame_id :: Int }

type InputState =
    { history :: RingBuffer InputFrame
    , input_q :: Ref Input }

keyUp :: String
keyUp = "w"

keyLeft :: String
keyLeft = "a"

keyRight :: String
keyRight = "d"

keyAttack :: String
keyAttack = "k"

listener :: forall eff. KeyPress -> Ref Input -> Event -> Eff (ref :: REF | eff) Unit
listener press_type input_q event = for_ (runExcept $ eventToKeyboardEvent event) \keyboard_event -> let
    character = key keyboard_event
    in
    modifyRef input_q $ Cons { press_type, character }

register_keyboard :: forall eff. Window -> Eff (ref :: REF, dom :: DOM | eff) InputState
register_keyboard window = do
    input_q <- newRef Nil
    addEventListener
        (EventType "keydown")
        (eventListener $ listener Down input_q)
        false
        (windowToEventTarget window)
    addEventListener
        (EventType "keyup")
        (eventListener $ listener Up input_q)
        false
        (windowToEventTarget window)
    let initial_frame = { down: Set.empty, up: Set.empty, frame_id: -1 }
    let history = RingBuffer.make ticks_per_second initial_frame
    pure { history , input_q }

split :: Input -> {down_set :: Set String, up_set :: Set String}
split input =
    { down_set: Set.fromFoldable $ map _.character $ List.filter (\c->c.press_type == Down) input
    , up_set:   Set.fromFoldable $ map _.character $ List.filter (\c->c.press_type == Up)   input }

update_state :: forall eff. InputState -> Int -> Eff (ref :: REF | eff) InputState
update_state state frame_id = do
    input_q <- readRef state.input_q

    let {down_set, up_set} = split input_q

    let last_frame = RingBuffer.get state.history

    --have to do the difference because we sometimes get an extra down after an up
    let down = Set.difference (Set.union last_frame.down down_set) up_set
    let up   = Set.difference up_set down_set

    modifyRef state.input_q (\_->Nil)

    let frame = { down, up, frame_id }

    pure state { history = RingBuffer.push state.history frame }

interpret :: InputState -> Array Command
interpret input = let
    frame_input = RingBuffer.get input.history
    commands = []
    commands' = case Set.member keyLeft frame_input.down, Set.member keyRight frame_input.down of
        true,  false -> snoc commands $ MoveCmd $ Just Left
        false, true  -> snoc commands $ MoveCmd $ Just Right
        false, false -> if Set.member keyLeft frame_input.up || Set.member keyRight frame_input.up
            then snoc commands $ MoveCmd Nothing else commands
        _, _         -> commands

    commands'' = let
        frames_held = RingBuffer.count_while input.history (\f->Set.member keyUp f.down)
        in
        if frames_held == 0 then
            if Set.member keyUp frame_input.up
            then snoc commands' (JumpCmd Nothing)
            else commands'
        else snoc commands' (JumpCmd (Just (dt * toNumber frames_held)))

    commands''' = if Set.member keyAttack frame_input.down then snoc commands'' $ AttackCmd 0.6 else commands''
    in
    commands'''
