ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.ONESHELL:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: test bench

test:
> futhark test `fd '.*_test.fut'`

bench_input: bench_input.fish
> bench_input.fish -n 1000 -z 300 -a 12000 -o $@

build/slime_bench-c: slime.fut slime_bench.fut
> mkdir -p $(@D)
> cd $(@D)
> futhark c ../slime_bench.fut -o slime_bench-c

build/slime_bench-opencl: slime.fut slime_bench.fut
> mkdir -p $(@D)
> cd $(@D)
> futhark opencl ../slime_bench.fut -o slime_bench-opencl

bench: bench_input build/slime_bench-c build/slime_bench-opencl
> echo 'Benchmarking C...'
> cat bench_input | build/slime_bench-c -t /dev/stderr  > /dev/null
> echo 'Benchmarking OpenCL...'
> cat bench_input | build/slime_bench-opencl -t /dev/stderr  > /dev/null
