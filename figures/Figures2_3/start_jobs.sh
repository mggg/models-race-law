#!/bin/bash
mkdir -p $2
for f in $1/*.json
do
    echo $f
    if [[ $f == *"ei"* ]]; then
        use_ei=1
    fi
    if [[ $f == *"votes"* ]]; then
        use_election=1
    fi

    for level in "CD" "HD" "SD"
    do
        echo "level: $level"
        if [ "$level" == "SD" ]; then
            pop_tol=0.05
            n_steps=2000000
        elif [ "$level" == "HD" ]; then
            pop_tol=0.05
            n_steps=1000000
        else
            pop_tol=0.02
            n_steps=1000000
        fi

        if [[ $f == *$level* ]]; then
            echo "    $level $n_steps $pop_tol"
            sbatch -c 1 --mem-per-cpu 20G --time 3-0:00:00 --wrap "julia cluster_runs.jl --graph-json $f --pop-tolerance $pop_tol --assignment-col SPLITS_SEED_${level} --n-steps $n_steps --out-file $2/${f##*/}__${level}.jld --pop --vap ${use_ei:+--ei} ${use_election:+--election}"
        fi
    done
    unset use_ei
    unset use_election
    unset pop_tol
    unset n_steps
done
${foo##*:}
