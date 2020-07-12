#!/usr/bin/fish

argparse 'n/iterations=' 'z/size=' 'a/agents=' 's/seed=?' 'o/out=' -- $argv

set -q _flag_seed; or set -l _flag_seed 42

futhark dataset -b -s $_flag_seed \
    --i32-bounds=$_flag_n:$_flag_n -g i32 \
    -g [$_flag_z][$_flag_z]f32 \
    --f32-bounds=0:(math "$_flag_z - 1") \
    -g [$_flag_a]f32 \
    -g [$_flag_a]f32 \
    --f32-bounds=0:6.283 \
    -g [$_flag_a]f32 \
    > $_flag_o
