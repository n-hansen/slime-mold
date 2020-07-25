type dependent_f32 [n] = {
    const: [n]f32,
    slope: [n]f32
  }

type model_params [n] = {
    -- environment parameters
    trail_decay: f32,
    nutrient_decay: f32,
    nutrient_attr: f32,

    -- agent parameters
    sensor_angle: dependent_f32[n],
    sensor_offset: dependent_f32[n],
    -- sensor_width: i32,
    rot_angle: dependent_f32[n],
    step_size: dependent_f32[n],
    deposit_amount: dependent_f32[n],
    max_density: i32
    -- angle_jitter: f32
  }

type agent [n] = {
    loc: (f32,f32),
    ang: f32,
    nutrient: [n]f32
  }

type env [grid_h][grid_w][n_agents][nutrient_channels] = {
    model_params: model_params[nutrient_channels],
    trail_map: [grid_h][grid_w]f32,
    density_map: [grid_h][grid_w]i32,
    nutrient_map: [grid_h][grid_w][nutrient_channels]f32,
    agent_list: [n_agents]agent[nutrient_channels]
  }

let const_param [n]
                (x: f32)
                : dependent_f32[n] =
  {const=concat_to n [x] (replicate (n-1) 0), slope=replicate n 0}

let realize_param [n]
                  (nutrient: [n]f32)
                  (param: dependent_f32[n])
                  : f32 =
  f32.sum (
    map3 f32.mad nutrient param.slope param.const
  )

let gaussian_blur_2d [h][w]
                     (ker_size: i32)
                     (arr: [h][w]f32)
                     : [h][w]f32 =
  let l = ker_size/2
  let s = f32.sqrt <| r32 l
  let s2 = s*s
  let c = 1 / (s * f32.sqrt f32.pi)
  let ker = map
            (\x -> c * f32.exp (-(r32 <| x-l)**2 / s2))
            (iota ker_size)
  let blur_x = map (\row ->
                      map (\col ->
                             map (\neighbor ->
                                    let ncol = col + neighbor - l
                                    let ncol_wrapped = if ncol < 0 then ncol + w
                                                       else if ncol >= w then ncol - w
                                                       else ncol
                                    in ker[neighbor] * arr[row,ncol_wrapped]
                                 ) (iota ker_size)
                             |> f32.sum
                          ) (iota w)
                   ) (iota h)
  let blur_y = map (\row ->
                      map (\col ->
                             map (\neighbor ->
                                    let nrow = row + neighbor - l
                                    let nrow_wrapped = if nrow < 0 then nrow + h
                                                       else if nrow >= h then nrow - h
                                                       else nrow
                                    in ker[neighbor] * blur_x[nrow_wrapped,col]
                                 ) (iota ker_size)
                             |> f32.sum
                          ) (iota w)
                   ) (iota h)
  in blur_y


let update_nutrient [h][w][a][n]
                    (e: env[h][w][a][n])
                    ({loc, ang, nutrient}: agent[n])
                    : agent[n] =
  { loc
  , ang
  , nutrient=map2 (\m n -> f32.max 0 <| f32.max n (m + n / 2) - e.model_params.nutrient_decay)
                  e.nutrient_map[t32 loc.1, t32 loc.0]
                  nutrient
  }

let bounded (max: f32)
            (x: f32)
            : f32 =
  (x + max) f32.% max

let loc2grid (grid_size: i32)
             (real_loc: f32)
             : i32 =
  let gs_f = r32 grid_size
  in if real_loc >= 0 && real_loc < gs_f
     then t32 real_loc
     else t32 (bounded gs_f real_loc)

let read_sensor [h][w][a][n]
                (e: env[h][w][a][n])
                ({loc=(x,y), ang, nutrient}: agent[n])
                (ang_offset: f32)
                : f32 =
  let sx = loc2grid w (f32.cos (ang + ang_offset) * realize_param nutrient e.model_params.sensor_offset + x)
  let sy = loc2grid h (f32.sin (ang + ang_offset) * realize_param nutrient e.model_params.sensor_offset + y)
  in if sx < 5
        || sx >= w-5
        || sy < 5
        || sy >= h-5
     then 0 else
  let nut = f32.sum(
              map (\i -> f32.max 0 <| e.nutrient_map[sy,sx,i] - nutrient[i])
                  (iota n)
            )
  in e.trail_map[sy,sx] + e.model_params.nutrient_attr * nut

let move_step [n]
              (h: i32) (w: i32)
              (p: model_params[n])
              ({loc=(x, y), ang, nutrient} : agent[n])
              : agent[n] =
  let x_ = bounded (r32 w) <| x + realize_param nutrient p.step_size * f32.cos ang
  let y_ = bounded (r32 h) <| y + realize_param nutrient p.step_size * f32.sin ang
  in {loc=(x_, y_), ang, nutrient}

let check_density [n]
                  (p: model_params[n])
                  (density_map: [][]i32)
                  (x,y)
                  : bool =
  density_map[t32 y, t32 x] < p.max_density

let step_agent [h][w][a][n]
               (e: env[h][w][a][n])
               (a: agent[n])
               : (agent[n], (i32, i32, f32)) =
  let sl = read_sensor e a <| realize_param a.nutrient e.model_params.sensor_angle
  let sf = read_sensor e a 0
  let sr = read_sensor e a (f32.negate <| realize_param a.nutrient e.model_params.sensor_angle)
  let stepped = if sf >= sr && sf >= sl
                then move_step h w e.model_params a
                else move_step h w e.model_params {
                                 loc=a.loc,
                                 ang=a.ang + (f32.sgn <| sl - sr) * realize_param a.nutrient e.model_params.rot_angle,
                                 nutrient=a.nutrient
                               }
  in if check_density e.model_params e.density_map stepped.loc
     then ( update_nutrient e stepped
          , (t32 stepped.loc.0
            , t32 stepped.loc.1
            , realize_param stepped.nutrient e.model_params.deposit_amount
            )
          )
     else ( update_nutrient e {loc=a.loc,ang=stepped.ang, nutrient=a.nutrient}
          , (t32 a.loc.0
            , t32 a.loc.1
            , 0)
          )

let step_agents [h][w][a][n]
                (e: env[h][w][a][n])
                : env[h][w][a][n] =
  let (stepped, deposits) = unzip (map (step_agent e) e.agent_list)
  let (flat_deposits_ix, flat_deposits_amnt) = map (\(x,y,amnt) -> (y*w+x, (amnt, 1))) deposits |> unzip
  let (deposited, counted) = reduce_by_index
                             (map (\x -> (x,0)) (flatten e.trail_map))
                             (\(x1,y1) (x2,y2) -> (x1+x2, y1+y2))
                             (0,0)
                             flat_deposits_ix
                             flat_deposits_amnt
                             |> unzip
  in { model_params=e.model_params
     , trail_map=unflatten h w deposited
     , density_map=unflatten h w counted
     , agent_list=stepped
     , nutrient_map=e.nutrient_map}

let disperse_trail [h][w][a][n]
                   ({model_params, trail_map, density_map, nutrient_map, agent_list}: env[h][w][a][n])
                   : env[h][w][a][n] =
  { model_params
  , agent_list
  , density_map
  , nutrient_map
  , trail_map= trail_map
               |> map (map (\x -> model_params.trail_decay * x))
               |> gaussian_blur_2d 3
  }

-- Library API


entry simulation_step [h][w][a][n]
                    (e: env[h][w][a][n])
                    : env[h][w][a][n] =
  e |> step_agents |> disperse_trail

entry run_simulation [h][w][a][n]
                     (times: i32)
                     (e0: env[h][w][a][n])
                     : env[h][w][a][n] =
  loop e = e0 for _i < times do simulation_step e

entry init [h][w][a][n]
           (trail_decay: f32)
           (nutrient_decay: f32)
           (nutrient_attr: f32)
           (sensor_angle: f32)
           (sensor_offset: f32)
           (rot_angle: f32)
           (step_size: f32)
           (deposit_amount: f32)
           (max_density: i32)
           (trail_map: [h][w]f32)
           (nutrient_map: [h][w][n]f32)
           (agent_x: [a]f32)
           (agent_y: [a]f32)
           (agent_ang: [a]f32)
           (agent_nut: [a][n]f32)
           : env[h][w][a][n] =
  { model_params = { trail_decay
                   , nutrient_decay
                   , nutrient_attr
                   , sensor_angle=const_param sensor_angle
                   , sensor_offset=const_param sensor_offset
                   , rot_angle=const_param rot_angle
                   , step_size=const_param step_size
                   , deposit_amount=const_param deposit_amount
                   , max_density
                   }
  , trail_map
  , density_map=reduce_by_index
                (replicate (w*h) 0)
                (+) 0
                (map2 (\x y -> t32 x + t32 y * w) agent_x agent_y)
                (replicate a 1)
                |> unflatten h w
  , nutrient_map
  , agent_list = map4 (\x y ang nutrient -> {loc=(x,y), ang, nutrient}) agent_x agent_y agent_ang agent_nut
  }

entry update_params [h][w][a][n]
                    (trail_decay: f32)
                    (nutrient_decay: f32)
                    (nutrient_attr: f32)
                    (sensor_angle: f32)
                    (sensor_offset: f32)
                    (rot_angle: f32)
                    (step_size: f32)
                    (deposit_amount: f32)
                    (max_density: i32)
                    (e: env[h][w][a][n])
                    : env[h][w][a][n] =
  { model_params = { trail_decay
                   , nutrient_decay
                   , nutrient_attr
                   , sensor_angle=const_param sensor_angle
                   , sensor_offset=const_param sensor_offset
                   , rot_angle=const_param rot_angle
                   , step_size=const_param step_size
                   , deposit_amount=const_param deposit_amount
                   , max_density
                   }
  , trail_map=e.trail_map
  , density_map=e.density_map
  , nutrient_map=e.nutrient_map
  , agent_list=e.agent_list
  }


entry render_trail [h][w][a][n]
                   (e: env[h][w][a][n])
                   : [h][w]f32 =
  map (
    map (\t -> f32.min 1 t)
  ) e.trail_map

entry render_density [h][w][a][n]
                   (e: env[h][w][a][n])
                   : [h][w]f32 =
  map (
    map (\d -> r32 d / r32 e.model_params.max_density)
  ) e.density_map
  |> gaussian_blur_2d 5

let render_nutrient [h][w][a][n]
                    (e: env[h][w][a][n])
                    (a: [n]f32) -- a*n+b
                    (b: [n]f32)
                    : [h][w]f32 =
  let (is, as) = map (
                   \{loc=(x,y), ang=_, nutrient} -> ( t32 x + t32 y * w
                                                    , f32.sum (
                                                        map3 f32.fma nutrient a b
                                                      )
                                                    )
                 ) e.agent_list
                 |> unzip
  in reduce_by_index
     (replicate (h*w) 0)
     (+) 0
     is as
     |> unflatten h w

entry render_frame [h][w][a]
                   (e: env[h][w][a][3])
      : [h][w]i32 =
  let rss = render_nutrient e [255,0,0] [0,0,0]
            |> gaussian_blur_2d 5
  let gss = render_nutrient e [0,255,0] [0,0,0]
            |> gaussian_blur_2d 5
  let bss = render_nutrient e [0,0,255] [0,0,0]
            |> gaussian_blur_2d 5
  in map3 (
       \rs gs bs ->
         map3 (
           \r g b -> (t32 (f32.min 255 <| f32.max 0 r) << 16) +
                     (t32 (f32.min 255 <| f32.max 0 g) << 8) +
                     (t32 (f32.min 255 <| f32.max 0 b)) + 0

         ) rs gs bs
     ) rss gss bss
