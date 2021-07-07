module Constants (ticks_per_second, frame_time, dt, gravity, max_horizontal_speed, jump_impulse, max_jump_time, field_of_view, view_distance, screen_width, toDegrees, global_up, global_look) where

import Prelude
import Data.Int (toNumber)
import Math (pi, tan)
import Data.Vector (Vec(..))
import Data.Vector2 (Vec2)
import Data.Vector3 (Vec3)


ticks_per_second :: Int
ticks_per_second = 60

dt :: Number
dt = 1.0 / toNumber ticks_per_second

frame_time :: Number
frame_time = 1000.0 / toNumber ticks_per_second

max_horizontal_speed :: Number
max_horizontal_speed = 11.0

max_jump_height :: Number
max_jump_height = 7.1

max_jump_distance :: Number
max_jump_distance = 10.0

max_jump_time :: Number
max_jump_time = max_jump_distance / max_horizontal_speed

gravity :: Number
gravity = -8.0 * max_jump_height * max_horizontal_speed * max_horizontal_speed / (max_jump_distance * max_jump_distance)

jump_impulse :: Vec2 Number
jump_impulse = Vec [0.0, 4.0 * max_jump_height * max_horizontal_speed / max_jump_distance]
{--
Kinematic Equations
    s = t * (u + v) / 2

    v = u + a * t

    s = u * t + a * t * t / 2


    s = v * t - a * t * t / 2

    v * v = u * u + 2 * a * s

    --------vertical
    h (max_jump_height)
    t_h (max_jump_time [to peak])

    at t=0         v0 =  impulse
    at t = t_h     v0 = 0
    at t = 2 * t_h v0 = -impulse

    v0 = 2 * h / t_h
    g  = - 2 * h / t_h^2

    ---------horizontal
    v_x, t_h, x_h (one must be a fn of the other two)
    need to find g and vx0 from that

    t_h = x_h / v_x
    v0 = 2 * h * v_x / x_h
    g = -2 * h * v_x^2 / x_h^2
--}
field_of_view :: Number
field_of_view = pi / 3.0

view_distance :: Number
view_distance = 60.0

screen_width :: Number
screen_width = 2.0 * view_distance * tan (0.5 * field_of_view)

global_up :: Vec3 Number
global_up = Vec [0.0, 1.0, 0.0]

global_look :: Vec3 Number
global_look = Vec [0.0, 0.0, 1.0]

toDegrees :: Number -> Number
toDegrees = ((*) (180.0 / pi))

{--
Kinematic Equations
    s = t * (u + v) / 2

    v = u + a * t

    s = u * t + a * t * t / 2

    s = v * t - a * t * t / 2

    v * v = u * u + 2 * a * s
--}

