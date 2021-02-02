using ArgParse
using GerryChain
using Serialization
using DataStructures

EI_COLS = ["chen_black_repub", "chen_black_dem",
           "chen_hispanic_repub", "chen_hispanic_dem",
           "chen_other_repub", "chen_other_dem"]
ELECTION_COLS = ["Obama", "Romney"]
VAP_COLS = ["VAP", "BVAP", "HVAP"]
TOTPOP_COL = "TOTPOP"
POP_COLS = ["TOTPOP", "NH_BLACK", "NH_WHITE", "HISP"]
ARGS_TO_COLS = Dict(
    "ei" => EI_COLS,
    "election" => ELECTION_COLS,
    "vap" => VAP_COLS,
    "pop" => POP_COLS
)

function num_county_splits(graph::BaseGraph, partition::Partition)::Int
    county_districts = DefaultDict{String, BitSet}(() -> BitSet())
    for (node, assignment) in enumerate(partition.assignments)
        county = graph.attributes[node]["GEOID10"][1:5]
        push!(county_districts[county], assignment)
    end
    n_splits = 0
    for districts in values(county_districts)
        n_splits += length(districts) - 1
    end
    return n_splits
end

function no_more_county_splits(graph::BaseGraph, threshold::Int)::Function
    function accept_fn(partition::Partition)::Float64
        if num_county_splits(graph, partition) > threshold
            return 0
        end
        return 1
    end
    return accept_fn
end

function parse_cli()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--graph-json"
            required = true
        "--pop-tolerance"
            required = true
            arg_type = Float64
        "--assignment-col"
            required = true
        "--n-steps"
            required = true
            arg_type = Int
        "--out-file"
            required = true
        "--ei"
            action = :store_true
        "--election"
            action = :store_true
        "--vap"
            action = :store_true
        "--pop"
            action = :store_true
        "--county-bound"
            action = :store_true
    end

    return parse_args(s)
end

function main()
    args = parse_cli()
    graph = BaseGraph(
        args["graph-json"],
        TOTPOP_COL,
        args["assignment-col"]
    )
    partition = Partition(graph, args["assignment-col"])
    pop_constraint = PopulationConstraint(
        graph,
        TOTPOP_COL,
        args["pop-tolerance"]
    )

    scores = Array{AbstractScore}(undef, 0)
    for (arg, cols) in ARGS_TO_COLS
        if args[arg]
            for col in cols
                push!(scores, DistrictAggregate(col))
            end
        end
    end
    
    if args["county-bound"]
        county_threshold = num_county_splits(graph, partition)
        println("county split threshold: ", county_threshold)
        accept_fn = no_more_county_splits(graph, county_threshold)
    else
        accept_fn = always_accept
    end
    
    recom_data = recom_chain(
        graph,
        partition,
        pop_constraint,
        args["n-steps"],
        scores,
        acceptance_fn=accept_fn
    )
    serialize(args["out-file"], recom_data)
end

main()
