import "slime"

let to_deg (rad: f32): i32 = 180 * rad / f32.pi |> f32.round |> t32
let to_rad (deg: i32): f32 = r32 deg * f32.pi / 180

let build_test_env [h][w][a]
                   (trail_map: [h][w]f32)
                   (nutrient_map: [h][w]f32)
                   (agent_xs: [a]f32)
                   (agent_ys: [a]f32)
                   (agent_angs: [a]i32)
                   : env[h][w][a] =
  init
  0.5 -- trail_decay
  0.8 -- nutrient_decay
  (to_rad 45) -- sensor_angle
  2 -- sensor_offset
  (to_rad 45) -- rot_angle
  1 -- step_size
  9 -- deposit_amount
  2 -- max_density
  trail_map
  agent_xs
  agent_ys
  (map to_rad agent_angs)


-- Single Step Agent Tests
-- ==
-- entry: test_single_step_agent
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         1f32 1f32 0 }
-- output { [2f32,1f32,0f32] }
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         1f32 1f32 45 }
-- output { [1.707107f32,1.707107f32, 45f32] }
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,1f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         1f32 1f32 0 }
-- output { [1.707107f32,1.707107f32, 45f32] }
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,1f32,0f32,0f32]]
--         1.001f32 1.001f32 45 }
-- output { [1.1f32, 2.1f32, 90f32] }

entry test_single_step_agent [h][w]
                             (trail_map: [h][w]f32)
                             (x: f32)
                             (y: f32)
                             (ang: i32)
                             : [3]f32 =
  let blank_map = replicate h (replicate w 0)
  let agent_list = build_test_env trail_map blank_map [x] [y] [ang] |> simulation_step |> (.agent_list)
  in [agent_list[0].loc.0, agent_list[0].loc.1, agent_list[0].ang |> to_deg |> r32]

-- Single Step Trail Tests
-- ==
-- entry: test_single_step_trail
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         1f32 1f32 0 }
-- output {[[0.0f32, 0.25692815f32, 0.62641835f32, 0.25692815f32],
--          [0.0f32, 0.62641835f32, 0.966614f32, 0.62641835f32],
--          [0.0f32, 0.25692815f32, 0.62641835f32, 0.25692815f32],
--          [0.0f32, 0.0f32, 0.0f32, 0.0f32]]}
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         2f32 1f32 0 }
-- output {[[0.25692815f32, 0.0f32, 0.25692815f32, 0.62641835f32],
--          [0.62641835f32, 0.0f32, 0.62641835f32, 0.966614f32  ],
--          [0.25692815f32, 0.0f32, 0.25692815f32, 0.62641835f32],
--          [0.0f32, 0.0f32, 0.0f32, 0.0f32]]}
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,18f32]]
--         1f32 1f32 0 }
-- output { [[0.513856f32, 0.256928f32, 1.140275f32, 1.509765f32],
--           [0.0f32, 0.626418f32, 0.966614f32, 0.626418f32],
--           [0.513856f32, 0.256928f32, 1.140275f32, 1.509765f32],
--           [1.252837f32, 0.0f32, 1.252837f32, 1.933228f32]] }

entry test_single_step_trail [h][w]
                             (trail_map: [h][w]f32)
                             (x: f32)
                             (y: f32)
                             (ang: i32)
                             : [h][w]f32 =
  let blank_map = replicate h (replicate w 0)
  let e = simulation_step (build_test_env trail_map blank_map [x] [y] [ang])
  in e.trail_map

-- Single Step Density Tests
-- ==
-- entry: test_single_step_density
-- input {3 4 [0f32] [0f32] [0i32]}
-- output {[[0,1,0,0],
--          [0,0,0,0],
--          [0,0,0,0]]}
-- input {3 4 [1f32, 1f32, 1f32] [0f32, 0f32, 0f32] [0i32, 0i32, 0i32]}
-- output {[[0,0,3,0],
--          [0,0,0,0],
--          [0,0,0,0]]}
-- input {3 4 [0f32, 1f32, 1f32] [0f32, 0f32, 0f32] [0i32, 0i32, 0i32]}
-- output {[[1,0,2,0],
--          [0,0,0,0],
--          [0,0,0,0]]}


entry test_single_step_density [a]
                               (h: i32) (w: i32)
                               (xs: [a]f32)
                               (ys: [a]f32)
                               (angs: [a]i32)
      : [h][w]i32 =
  let blank_map = replicate h (replicate w 0)
  let e = simulation_step (build_test_env blank_map blank_map xs ys (angs))
  in e.density_map
