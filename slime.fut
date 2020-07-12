type model_params = {
    -- environment parameters
    --diffusion_ker_size: i32,
    decay: f32,

    -- agent parameters
    sensor_angle: f32,
    sensor_offset: f32,
    -- sensor_width: i32,
    rot_angle: f32,
    step_size: f32,
    deposit_amount: f32
    -- angle_jitter: f32
  }

type agent = {
    loc: (f32,f32),
    ang: f32
  }

type env [grid_h][grid_w][n_agents] = {
    model_params: model_params,
    trail_map: [grid_h][grid_w]f32,
    agent_list: [n_agents]agent
  }

let bounded (max: f32)
            (x: f32)
            : f32 =
  (x + max + max) f32.% max

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

let move_step (p: model_params)
              ({loc=(x: f32, y: f32),
                ang: f32} : agent)
              : agent =
  let x_ = x + p.step_size * f32.cos ang
  let y_ = y + p.step_size * f32.sin ang
  in {loc=(x_, y_), ang}

let step_agent (p: model_params)
               (trail_map: [][]f32)
               ({loc,ang}: agent)
               : (agent, (i32, i32)) =
  let sl = read_sensor p trail_map loc (ang + p.sensor_angle)
  let sf = read_sensor p trail_map loc ang
  let sr = read_sensor p trail_map loc (ang - p.sensor_angle)
  let stepped = if sf >= sr && sf >= sl
                then move_step p {loc,ang}
                else (if sr >= sl
                      then move_step p {loc, ang=ang - p.rot_angle}
                      else move_step p {loc, ang=ang + p.rot_angle})
  in (stepped, (t32 loc.0, t32 loc.1))

let step_agents [h][w][a]
                ({model_params, trail_map, agent_list}: env[h][w][a])
                : env[h][w][a] =
  let (stepped, deposits) = unzip (map (step_agent model_params trail_map) agent_list)
  let flat_deposits = map (\(x,y) -> y*w+x) deposits
  let deposited = reduce_by_index (copy (flatten trail_map)) (+) 0 flat_deposits (replicate a model_params.deposit_amount)
  in {model_params, trail_map=unflatten h w deposited, agent_list=stepped}

let disperse_cell [h][w]
                  (p: model_params)
                  (trail_map: [h][w]f32)
                  (x: i32) (y: i32)
                  : f32 =
  let neighbors = map (\(dx,dy) -> trail_map[(y+dy+h) i32.% h,
                                             (x+dx+w) i32.% w]
                      ) [(-1, 1), ( 0, 1), ( 1, 1),
                         (-1, 0),          ( 1, 0),
                         (-1,-1), ( 0,-1), ( 1,-1)]
  let sum = trail_map[x,y] + reduce (+) 0 neighbors
  in p.decay * sum / 9

let disperse_trail [h][w][a]
                   ({model_params, trail_map, agent_list}: env[h][w][a])
                   : env[h][w][a] =
  {model_params, agent_list,
   trail_map=tabulate_2d h w (disperse_cell model_params trail_map)}


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
                   }
  , trail_map
  , agent_list = map3 (\x y ang -> {loc=(x,y), ang}) agent_x agent_y agent_ang
  }

entry update_params [h][w][a]
                    (decay: f32)
                    (sensor_angle: f32)
                    (sensor_offset: f32)
                    (rot_angle: f32)
                    (step_size: f32)
                    (deposit_amount: f32)
                    (e: env[h][w][a])
                    : env[h][w][a] =
  { model_params = { decay
                   , sensor_angle
                   , sensor_offset
                   , rot_angle
                   , step_size
                   , deposit_amount
                   }
  , trail_map=e.trail_map
  , agent_list=e.agent_list
  }

entry get_trail_map [h][w][a]
                    (e: env[h][w][a])
                    : [h][w]f32 =
  e.trail_map
