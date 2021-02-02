using JSON
using ArgParse
using GerryChain
using StatsBase
using Statistics
using Serialization

EI_COLS = ["chen_black_repub", "chen_black_dem",
           "chen_hispanic_repub", "chen_hispanic_dem",
           "chen_other_repub", "chen_other_dem"]
ELECTION_COLS = ["Obama", "Romney"]
VAP_COLS = ["VAP", "BVAP", "HVAP"]
TOTPOP_COL = "TOTPOP"
POP_COLS = ["TOTPOP", "NH_BLACK", "NH_WHITE", "HISP"]
STAT_GROUPS = Dict(
    "ei" => EI_COLS,
    "election" => ELECTION_COLS,
    "vap" => VAP_COLS
)
MAJMIN_LEVELS = [25, 30, 35, 40, 45, 50]

function parse_cli()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--in-file"
            required = true
        "--out-file"
            required = true
    end

    return parse_args(s)
end


function summarize(stat)
    return Dict(
        "min" => minimum(stat),
        "max" => maximum(stat),
        "median" => median(stat),
        "mean" => mean(stat),
        #"p1" => percentile(stat, 1),
        #"p5" => percentile(stat, 5),
        #"p95" => percentile(stat, 95),
        #"p99" => percentile(stat, 99)
    )
end


function main()
    args = parse_cli()
    stats = deserialize(args["in-file"])
    print("loaded stats from ", args["in-file"], "...")
    cols_present = keys(stats.step_values[1])
    present = Dict(group => true for group in keys(STAT_GROUPS))
    for (group, cols) in STAT_GROUPS
        for col in cols
            if !(col in cols_present)
                present[group] = false
                break
            end
        end
    end
    println("present:", present)

    summary = Dict()
    if present["vap"]
        vaps = Dict(col => get_score_values(stats, col) for col in VAP_COLS)
        for level in MAJMIN_LEVELS
            bvap_share = vaps["BVAP"] ./ vaps["VAP"]
            hvap_share = vaps["HVAP"] ./ vaps["VAP"]
            combined_share = bvap_share .+ hvap_share
            summary["bvap_over_$level"] = summarize(sum(x->x > level / 100,
                                                        bvap_share, dims=2))
            summary["hvap_over_$level"] = summarize(sum(x->x > level / 100,
                                                        hvap_share, dims=2))
            summary["b+hvap_over_$level"] = summarize(sum(x->x > level / 100,
                                                          combined_share, dims=2))
        end
    end
    if present["election"] && present["ei"]
        dem = get_score_values(stats, "Obama")
        rep = get_score_values(stats, "Romney")
        black_dem = get_score_values(stats, "chen_black_dem")
        black_rep = get_score_values(stats, "chen_black_repub")
        hisp_dem = get_score_values(stats, "chen_hispanic_dem")
        hisp_rep = get_score_values(stats, "chen_hispanic_repub")
        other_dem = get_score_values(stats, "chen_other_dem")
        # Armed with this data, we define an opportunity district as one where
        #   (1) the minority-preferred candidate wins the general election
        #   (2) minority voters who support the minority-preferred candidate
        #       outnumber white voters backing that candidate, provided that 
        #   (3) minority voters of different racial groups are aggregated only
        #       if each group favors the same candidate.
        # ~ "Race-Blind Future", p. 29
        # TODO: does this match exactly?
        #println(dem .> rep)
        #println(black_dem .> black_rep)
        #println((dem .> rep) .& (black_dem .> black_rep))
        ood_agree = ((dem .> rep) .&
                     (black_dem .> black_rep) .&
                     (hisp_dem .> hisp_rep) .&
                     ((black_dem .+ hisp_dem) .> other_dem))
        disagreement = ((dem .> rep) .&
                        (black_dem .> black_rep) .&
                        (hisp_rep .> hisp_dem))
        ood_disagree = disagreement .& (black_dem .> other_dem)
        ood = ood_agree .| ood_disagree
        summary["ood"] = summarize(sum(ood, dims=2))
    end

    open(args["out-file"], "w") do f
        write(f, json(summary))
    end
end

main()
