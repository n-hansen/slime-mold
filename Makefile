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

.PHONY: test bench venv build run

test:
> futhark test `fd '.*_test.fut'`

bench_input: bench_input.fish
> bench_input.fish -n 1000 -z 300 -a 12000 -o $@

bench: bench_input slime_bench.fut
#> echo 'Benchmarking C...'
#> cat bench_input | build/slime_bench-c -t /dev/stderr  > /dev/null
> echo 'Benchmarking OpenCL...'
> futhark bench slime_bench.fut --backend=opencl

requirements.txt: requirements.in
> . venv/bin/activate
> pip-compile requirements.in > requirements.txt

venv/bin/activate: requirements.txt
> . venv/bin/activate
> pip-sync requirements.txt
> touch venv/bin/activate

venv: venv/bin/activate

slime.py: slime.fut
> futhark pyopencl --library slime.fut

build: slime.py

run: build venv
> . venv/bin/activate
> python slime_gui.py
