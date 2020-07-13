import "slime"

let to_deg (rad: f32): i32 = 180 * rad / f32.pi |> f32.round |> t32
let to_rad (deg: i32): f32 = r32 deg * f32.pi / 180

let build_test_env [h][w][a]
                   (trail_map: [h][w]f32)
                   (agent_xs: [a]f32)
                   (agent_ys: [a]f32)
                   (agent_angs: [a]i32)
                   : env[h][w][a] =
  let model_params = { decay=0.5
                     , sensor_angle=to_rad 45
                     , sensor_offset=2
                     , rot_angle=to_rad 45
                     , step_size=1
                     , deposit_amount=9
                     , max_density=2
                     }
  let agent_list = map3 (\x y ang -> {loc=(x,y), ang=to_rad ang}) agent_xs agent_ys agent_angs
  let density_map = replicate h (replicate w 1)
  in {model_params, agent_list, density_map, trail_map}


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
  let agent_list = build_test_env trail_map [x] [y] [ang] |> simulation_step |> (.agent_list)
  in [agent_list[0].loc.0, agent_list[0].loc.1, agent_list[0].ang |> to_deg |> r32]

-- Single Step Trail Tests
-- ==
-- entry: test_single_step_trail
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         1f32 1f32 0 }
-- output {[[0.25692815f32, 0.62641835f32, 0.25692815f32, 0.0f32],
--          [0.62641835f32, 0.966614f32, 0.62641835f32, 0.0f32],
--          [0.25692815f32, 0.62641835f32, 0.25692815f32, 0.0f32],
--          [0.0f32, 0.0f32, 0.0f32, 0.0f32]]}
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32]]
--         2f32 1f32 0 }
-- output {[[0.0f32, 0.25692815f32, 0.62641835f32, 0.25692815f32],
--          [0.0f32, 0.62641835f32, 0.966614f32, 0.62641835f32],
--          [0.0f32, 0.25692815f32, 0.62641835f32, 0.25692815f32],
--          [0.0f32, 0.0f32, 0.0f32, 0.0f32]]}
-- input { [[0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,0f32],
--          [0f32,0f32,0f32,18f32]]
--         1f32 1f32 0 }
-- output {[[0.77078444f32, 0.62641835f32, 0.77078444f32, 1.2528367f32],
--          [0.62641835f32, 0.966614f32, 0.62641835f32, 0.0f32],
--          [0.77078444f32, 0.62641835f32, 0.77078444f32, 1.2528367f32],
--          [1.2528367f32, 0.0f32, 1.2528367f32, 1.933228f32]] }

entry test_single_step_trail [h][w]
                             (trail_map: [h][w]f32)
                             (x: f32)
                             (y: f32)
                             (ang: i32)
                             : [h][w]f32 =
  let e = simulation_step (build_test_env trail_map [x] [y] [ang])
  in e.trail_map
