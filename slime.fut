type model_params = {
    -- environment parameters
    decay: f32,

    -- agent parameters
    sensor_angle: f32,
    sensor_offset: f32,
    -- sensor_width: i32,
    rot_angle: f32,
    step_size: f32,
    deposit_amount: f32,
    max_density: i32
    -- angle_jitter: f32
  }

type agent = {
    loc: (f32,f32),
    ang: f32
  }

type env [grid_h][grid_w][n_agents] = {
    model_params: model_params,
    trail_map: [grid_h][grid_w]f32,
    density_map: [grid_h][grid_w]i32,
    agent_list: [n_agents]agent
  }

-- let diffusion_ker: [9](i32,i32,f32) =
--   let flat = flatten_to 9 (tabulate_2d 3 3 (\x y -> (x-1, y-1, 1 / f32.cosh (r32 ((x-1)**2 + (y-1)**2)))))
--   let total = reduce (+) 0 (map (.2) flat)
--   in map (\(x,y,z) -> (x,y,z/total)) flat

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

let read_sensor [xn] [yn]
                (p: model_params)
                (trail_map: [yn][xn]f32)
                (x: f32, y: f32)
                (ang: f32)
                : f32 =
  let sx = loc2grid xn (f32.cos ang * p.sensor_offset + x)
  let sy = loc2grid yn (f32.sin ang * p.sensor_offset + y)
  in trail_map[sy,sx]

let move_step (h: i32) (w: i32)
              (p: model_params)
              ({loc=(x: f32, y: f32),
                ang: f32} : agent)
              : agent =
  let x_ = bounded (r32 w) <| x + p.step_size * f32.cos ang
  let y_ = bounded (r32 h) <| y + p.step_size * f32.sin ang
  in {loc=(x_, y_), ang}

let check_density (p: model_params)
                  (density_map: [][]i32)
                  (x,y)
                  : bool =
  density_map[t32 y, t32 x] < p.max_density

let step_agent [h][w]
               (p: model_params)
               (trail_map: [h][w]f32)
               (density_map: [h][w]i32)
               ({loc,ang}: agent)
               : (agent, (i32, i32, f32)) =
  let sl = read_sensor p trail_map loc (ang + p.sensor_angle)
  let sf = read_sensor p trail_map loc ang
  let sr = read_sensor p trail_map loc (ang - p.sensor_angle)
  let stepped = if sf >= sr && sf >= sl
                then move_step h w p {loc,ang}
                else (if sr >= sl
                      then move_step h w p {loc, ang=ang - p.rot_angle}
                      else move_step h w p {loc, ang=ang + p.rot_angle})
  in if check_density p density_map stepped.loc
     then (stepped, (t32 stepped.loc.0, t32 stepped.loc.1, p.deposit_amount))
     else ({loc,ang=stepped.ang, nutrient}, (t32 loc.0, t32 loc.1, 0))

let step_agents [h][w][a]
                ({model_params, trail_map, density_map, agent_list}: env[h][w][a])
                : env[h][w][a] =
  let (stepped, deposits) = unzip (map (step_agent model_params trail_map density_map) agent_list)
  let (flat_deposits_ix, flat_deposits_amnt) = map (\(x,y,amnt) -> (y*w+x, (amnt, 1))) deposits |> unzip
  let (deposited, counted) = reduce_by_index
                             (map (\x -> (x,0)) (flatten trail_map))
                             (\(x1,y1) (x2,y2) -> (x1+x2, y1+y2))
                             (0,0)
                             flat_deposits_ix
                             flat_deposits_amnt
                             |> unzip
  in { model_params
     , trail_map=unflatten h w deposited
     , density_map=unflatten h w counted
     , agent_list=stepped}

let disperse_cell [h][w]
                  (p: model_params)
                  (trail_map: [h][w]f32)
                  (y: i32) (x: i32)
                  : f32 =
  let bdd m x = if x < m && x >= 0 then x else (x+m) i32.% h
  let neighbors = map (\(dx,dy,wt) -> wt * trail_map[bdd h (y+dy),
                                                     bdd w (x+dx)]
                      ) [(-1i32, -1i32, 5.709514e-2f32),
                         (-1i32, 0i32, 0.13920408f32),
                         (-1i32, 1i32, 5.709514e-2f32),
                         (0i32, -1i32, 0.13920408f32),
                         (0i32, 0i32, 0.21480311f32),
                         (0i32, 1i32, 0.13920408f32),
                         (1i32, -1i32, 5.709514e-2f32),
                         (1i32, 0i32, 0.13920408f32),
                         (1i32, 1i32, 5.709514e-2f32)]

  let sum = reduce (+) 0 neighbors
  in p.decay * sum

let disperse_trail [h][w][a]
                   ({model_params, trail_map, density_map, agent_list}: env[h][w][a])
                   : env[h][w][a] =
  { model_params
  , agent_list
  , density_map
  , trail_map=tabulate_2d h w (disperse_cell model_params trail_map)}


-- Library API


entry simulation_step [h][w][a]
                    (e: env[h][w][a])
                    : env[h][w][a] =
  e |> step_agents |> disperse_trail

entry run_simulation [h][w][a]
                   (n: i32)
                   (e0: env[h][w][a])
                   : env[h][w][a] =
  loop e = e0 for _i < n do simulation_step e

entry init [h][w][a]
           (decay: f32)
           (sensor_angle: f32)
           (sensor_offset: f32)
           (rot_angle: f32)
           (step_size: f32)
           (deposit_amount: f32)
           (max_density: i32)
           (trail_map: [h][w]f32)
           (agent_x: [a]f32)
           (agent_y: [a]f32)
           (agent_ang: [a]f32)
           : env[h][w][a] =
  { model_params = { decay
                   , sensor_angle
                   , sensor_offset
                   , rot_angle
                   , step_size
                   , deposit_amount
                   , max_density
                   }
  , trail_map
  , density_map=replicate (w*h) 0 |> unflatten h w
  , agent_list = map3 (\x y ang -> {loc=(x,y), ang}) agent_x agent_y agent_ang
  }

entry update_params [h][w][a]
                    (decay: f32)
                    (sensor_angle: f32)
                    (sensor_offset: f32)
                    (rot_angle: f32)
                    (step_size: f32)
                    (deposit_amount: f32)
                    (max_density: i32)
                    (e: env[h][w][a])
                    : env[h][w][a] =
  { model_params = { decay
                   , sensor_angle
                   , sensor_offset
                   , rot_angle
                   , step_size
                   , deposit_amount
                   , max_density
                   }
  , trail_map=e.trail_map
  , density_map=e.density_map
  , agent_list=e.agent_list
  }

entry render_frame [h][w][a]
                   (e: env[h][w][a])
                   : [h][w]i32 =
  map2 (
    map2 (
      \t_cell d_cell ->
        (t32 (f32.min t_cell 1 * 255) << 16)
        + i32.min 255 (d_cell * 255 / e.model_params.max_density)
    )
  ) e.trail_map e.density_map

