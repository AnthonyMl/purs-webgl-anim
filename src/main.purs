module Main where
--pulp -w build -t main.js
{--TODO:
1.make the flip animation an over time transition (its post so shouldn't be too hard)
2.use lenses for stuff
3.add attack animation
4.add jump animations
    -draw debug jump trajectory
    -legs only point forward when moving forward
    4.2.add compression on landing
        blend with crouch pose
5.add non-player humanoids
6.track player with camera
    -add camera visualisation
7.upgrade to 0.12
8.upload modified forks to github
9.use regular gl
11.make clip position ms instead of fp
--}
import Prelude
import Animation as Animation
import Animation (animate, skinning_matrices, animation_pose)
import Background as Background
import Constants (dt, frame_time, max_horizontal_speed, view_distance, screen_width, global_up, global_look)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, newRef, modifyRef, readRef)
import Control.Monad.Eff.WebGL (WebGl)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document, innerHeight, innerWidth)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.Node.Node (setTextContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (documentToNonElementParentNode, ElementId(..), elementToNode, Node)
import Data.Foldable (foldr)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Matrix (toArray)
import Data.Matrix4 (makeOrtho, mulM, makeTranslate, makeRotate, identity)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Vector (Vec(..), scale)
import Data.Vector2 (get2X)
import Graphics.Canvas (CANVAS, CanvasElement, setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL (withShaders, runWebGL, requestAnimationFrame)
import Graphics.WebGL as GL
import Partial.Unsafe (unsafePartial)
import Math (pi, abs)
import Input as Input
import Player as Player
import Renderer as Renderer
import System.Clock (CLOCK)
import System.Clock as Clock
import GridShader as GridShader
import ExponentialRunningAverage as Average
import Types (Command(..), HorizontalDirection(..), fromMilliseconds, Milliseconds(..))
import HumanoidSkeleton as Skeleton
import HumanoidState as HumanoidState
import HumanoidTypes (HumanoidState)
import HumanoidAnimation as HumanoidAnimation
import Utility (extend)
import SkinningShader as SkinningShader
import FlatShader as FlatShader


type MainState =
    { last_time        :: Number
    , frame_id         :: Int
    , avg_frame_time   :: Average.ExponentialRunningAverage
    , animation_states :: Array AnimationContext
    , input_state      :: Input.InputState
    , text_field       :: Node
    , render_static    :: Renderer.StaticState
    , render_dynamic   :: Renderer.DynamicState
    , humanoid_states  :: Array HumanoidState }

type AnimationContext =
    { static  :: Animation.StaticState
    , dynamic :: Animation.DynamicState }

setDimensions :: forall eff. Number -> Number -> Maybe CanvasElement -> Eff (canvas :: CANVAS | eff) Unit
setDimensions _ _ Nothing = pure unit
setDimensions w h (Just canvas) = do
    _ <- setCanvasWidth w canvas
    _ <- setCanvasHeight h canvas
    pure unit

main :: forall eff. Eff (console :: CONSOLE, ref :: REF, clock :: CLOCK, canvas :: CANVAS, dom :: DOM | eff) Unit
main = runWebGL "glcanvas" log \webgl_context -> do
    doc <- window >>= document <#> htmlDocumentToDocument >>> documentToNonElementParentNode
    text_field <- getElementById (ElementId "text_field") doc <#> (unsafePartial $ fromJust) >>> elementToNode

    w <- innerWidth =<< window
    h <- innerHeight =<< window

    let resolution = Vec $ map toNumber [w, h]

    maybe_canvas <- getCanvasElementById webgl_context.canvasName

    input_state <- Input.register_keyboard =<< window

    setDimensions (toNumber w) (toNumber h) maybe_canvas

    GL.disable GL.CULL_FACE

    GL.disable GL.DEPTH_TEST

    GL.viewport 0 0 w h

    GL.clearColor 0.25 0.5 0.75 1.0

    let aspect_ratio = toNumber w / toNumber h

    let left   = -0.5 * screen_width
    let right  =  0.5 * screen_width
    let top    =  0.5 * screen_width / aspect_ratio
    let bottom = -0.5 * screen_width / aspect_ratio
    let projection_matrix = makeOrtho left right bottom top 1.0 view_distance

    player_mesh <- Skeleton.skinning_mesh

    background_mesh <- Background.mesh

    bounding_box_mesh <- Skeleton.bounding_box_mesh

    let meshes = [ player_mesh ]
    let animation_states = [ HumanoidAnimation.animation_state ]

    start_time <- Clock.milliseconds

    let avg_frame_time = { alpha: dt, sum: 0.0 }

    let humanoid_states = [ Player.initial_state ]

    grid_state <- GridShader.state

    grid_bindings_ref <- newRef Nothing

    flat_bindings_ref <- newRef Nothing

    _ <- withShaders GridShader.shaders log \bindings ->
        modifyRef grid_bindings_ref \_->
            let (GL.WebGLProg program) = bindings.webGLProgram
            in Just
            { program
            , a2position    : bindings.a2position
            , u2resolution  : bindings.u2resolution
            , u2screen_size : bindings.u2screen_size }

    _ <- withShaders FlatShader.flat_shaders log \bindings ->
        modifyRef flat_bindings_ref \_->
            let (GL.WebGLProg program) = bindings.webGLProgram
            in Just
            { program
            , a2position : bindings.a2position
            , a3color    : bindings.a3color
            , u44mvp     : bindings.u44mvp }

    withShaders SkinningShader.shaders log \bindings -> do
        GL.disableVertexAttribArray bindings.a2position
        GL.disableVertexAttribArray bindings.a1skinning_index
        GL.disableVertexAttribArray bindings.a3color

        grid_bindings <- readRef grid_bindings_ref <#> (unsafePartial $ fromJust)
        flat_bindings <- readRef flat_bindings_ref <#> (unsafePartial $ fromJust)
        let (GL.WebGLProg program) = bindings.webGLProgram
        let skinning_bindings = {
              program
            , a2position          : bindings.a2position
            , a3color             : bindings.a3color
            , a1skinning_index    : bindings.a1skinning_index
            , u44skinning_matrices: bindings.u44skinning_matrices }
        let render_static = {
              skinning_bindings
            , grid_bindings
            , flat_bindings
            , grid_state
            , projection_matrix
            , resolution
            , meshes
            , background_mesh
            , bounding_box_mesh }
        let render_dynamic = { model_transforms: [], skinning_arrays: [] }
        tick
            { last_time: start_time
            , frame_id: 0
            , avg_frame_time
            , animation_states
            , input_state
            , text_field
            , render_static
            , render_dynamic
            , humanoid_states }

tick :: forall eff. MainState -> Eff (webgl :: WebGl, ref :: REF, clock :: CLOCK, dom :: DOM | eff) Unit
tick state = requestAnimationFrame do
    current_time <- Clock.milliseconds

    state' <- update state current_time
    state' <- pure state' {
        avg_frame_time = Average.update state.avg_frame_time (current_time - state.last_time)
    }

    let msg = let
            hs = unsafePartial $ Array.unsafeIndex state'.humanoid_states 0
            {static, dynamic} = unsafePartial $ Array.unsafeIndex state'.animation_states 0
            pose_index i sum = let
                e = unsafePartial $ Array.unsafeIndex (dynamic.clip hs.action).sequence i
                upper = sum + e.duration
                in
                if dynamic.position <= fromMilliseconds upper then i else pose_index (i + 1) upper
        in
            "avg_frame_time: " <> show state'.avg_frame_time.sum
            <> "\n" <>
            show hs.action
            <> "\n" <>
            "position: " <> show dynamic.position
            <> "\n" <>
            "index: " <> show (pose_index 0 (Milliseconds 0))

    setTextContent msg state.text_field

    Renderer.draw_scene state'.render_static state'.render_dynamic
    tick state'

update :: forall eff. MainState -> Number -> Eff (ref :: REF | eff) MainState
update state current_time = do
    let loop s = if current_time - s.last_time > frame_time then do_update s >>= loop else pure s
    loop state

do_update :: forall eff. MainState -> Eff (ref :: REF | eff) MainState
do_update state = do
    input_state <- Input.update_state state.input_state state.frame_id

    --INPUT
    --could pass the pass time command at a later point
    --or handle it differently (would have a second function updating the skeletonstate)
    let commands = Array.snoc (Input.interpret input_state) (PassTimeCmd dt)

    --COLLISION, INTEGRATION
    let humanoid_states = map ((#) commands) $ map (foldr HumanoidState.not_like_this) state.humanoid_states

    --TICK_ANIMATION
    let update_anim (Tuple (Tuple hs hs') {static, dynamic}) = let
            dynamic' = case HumanoidAnimation.transition hs hs' dynamic of
                Nothing -> dynamic
                Just transition -> dynamic { position = 0.0, transition = Just transition }

            playback_rate = 1.0

            dynamic'' = animate dynamic' hs' (dt * playback_rate)
        in
            { static, dynamic: dynamic'' }
    let animation_states = map update_anim $ Array.zip (Array.zip state.humanoid_states humanoid_states) state.animation_states

    --POSES
    let pose (Tuple {static, dynamic} hs) = animation_pose static.poses dynamic hs.action
    let poses = map pose $ Array.zip animation_states humanoid_states

    --COMBINE_TRANSFORMS
    let model_transform hs = let
            facing_rotation = case hs.facing of
                Left  -> identity
                Right -> makeRotate pi global_up
            model_translation = makeTranslate $ extend hs.position
        in
            mulM model_translation facing_rotation
    let model_transforms = map model_transform humanoid_states

    --POST: TILT, OFFSET
    let tilted_transform (Tuple (Tuple hs {static, dynamic}) transform) = let
            --TODO: this needs to be accessible for collision/etc
            --TODO: make this based on acceleration (which we shouldnt use to integrate???) instead
            r_z = (pi / 20.0) * (abs $ get2X hs.velocity) / max_horizontal_speed
            velocity_tilt = makeRotate r_z global_look
            clip = dynamic.clip hs.action
            t_y = clip.hips_translation hs dynamic.position
            vertical_translation = makeTranslate $ scale t_y global_up
        in
            mulM (mulM vertical_translation transform) velocity_tilt
    let tilted_transforms = map tilted_transform $ Array.zip (Array.zip humanoid_states animation_states) model_transforms

    let view_projection = mulM state.render_static.projection_matrix Renderer.view_matrix
    --SKIN, TRANSFORM_SKIN
    let skinning_array {pose, joints, transform} = let
            model_view_projection = mulM view_projection transform
            matrices = skinning_matrices pose joints
        in
            Array.concatMap (toArray <<< (mulM model_view_projection)) matrices
    --TODO: find a way to do this better
    let zf (Tuple t p) {static, dynamic} = {pose: p, joints: static.joints, transform: t}
    let skinning_arrays = map skinning_array $ Array.zipWith zf (Array.zip tilted_transforms poses) animation_states

    pure state
        { last_time = state.last_time + frame_time
        , animation_states = animation_states
        , input_state = input_state
        , render_dynamic = { model_transforms, skinning_arrays }
        , humanoid_states = humanoid_states }
