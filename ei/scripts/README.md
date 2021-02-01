# Instructions for running EI

There are 5 EI scripts in this directory. Each is very similar to the rest, so reading through one of them should give you an understanding of the script as a whole. The scripts are:
 * `one_phase_byCounty_ei.R`
 * `one_phase_statewide_ei.R`
 * `one_phase_statewide_ei_all_elecs.R`
 * `two_phase_byCounty_ei.R`
 * `two_phase_statewide_ei.R`

Except for `one_phase_statewide_ei_all_elecs.R`, which was used to generate the EI data for Figure 4 in the Response, all the other EI scripts are set up to run only on the 2012 General Presidential Election in Texas, drawing data from `../resources/TX_data_for_Chen_EI.csv`. For these four 2012-only EI scripts, one can run them from the command line in this directory using this command:

`Rscript [script_name] [handlePPsMethod] [removePPs] [population] [outfile]`

where `handlePPsMethod` is one of `buffer`, `scaleVotes`, or `scalePop`, `removePPs` is either `PPs` or `noPPs`, `population` is either `VAP` or `CVAP`, and outfile is the name of the output file, which will be stored in `../outputs/outfile.csv`.

`one_phase_statewide_ei_all_elecs.R` is run by:

`Rscript one_phase_statewide_ei_all_elecs.R [election] [handlePPsMethod] [removePPs] [population] [outfile]`

where `election` is one of the elections in our dataset, for example: `14R_US_Sen`. The full list can be found in the EI script itself. This file uses an expanded dataset containing vote counts for a larger set of TX elections: `../resources/TX_cvap_for_EI.csv`. It puts results in `../outputs/fig4_outputs/outfile.csv`. 
