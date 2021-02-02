#!/bin/bash
mkdir -p $2
for f in $1/*.jld
do
    sbatch -c 1 --mem-per-cpu 20G --time 3-0:00:00 --wrap "julia parse_results.jl --in-file $f --out-file $2/${f##*/}.json"
done
