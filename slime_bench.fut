import "slime"


let bench_env [h][w][a]
              (trail_map: [h][w]f32)
              (agent_x: [a]f32)
              (agent_y: [a]f32)
              (agent_a: [a]f32)
              : env[h][w][a] =
  init
  0.9
  (f32.pi/4)
  3
  (f32.pi/8)
  1
  1
  3
  trail_map
  (replicate h (replicate w 0))
  agent_x
  agent_y
  agent_a
  (replicate a 0)

-- Run simulation
-- ==
-- compiled input @ bench_input
-- output {0f32}

let main (n: i32)
         (trail_map: [][]f32)
         (ax: []f32)
         (ay: []f32)
         (aa: []f32)
         : f32 =
  let e = bench_env trail_map ax ay aa
  let result = run_simulation n e
  in result.agent_list[0].nutrient / result.agent_list[0].loc.0
