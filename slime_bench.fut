import "slime"


let bench_env [h][w][a]
              (trail_map: [h][w]f32)
              (agent_x: [a]f32)
              (agent_y: [a]f32)
              (agent_a: [a]f32)
              : env[h][w][a] =
  { model_params =
      { pct_pop = 0
      , decay = 0.9
      , sensor_angle = f32.pi/4
      , sensor_offset = 3
      , rot_angle = f32.pi/8
      , step_size = 1
      , deposit_amount = 1
      }
  , trail_map
  , agent_list = map3 (\x y ang -> {loc=(x,y), ang})
                      agent_x
                      agent_y
                      agent_a
  }

let main (n: i32)
         (trail_map: [][]f32)
         (ax: []f32)
         (ay: []f32)
         (aa: []f32)
         : [][]f32 =
  let e = bench_env trail_map ax ay aa
  in run_simulation n e |> (.trail_map)
