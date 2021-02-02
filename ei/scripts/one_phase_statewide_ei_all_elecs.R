library(tidyverse)
library(eiPack)
library(rgdal)
library(foreign)
library(readr)
library(optparse)

args <- commandArgs(trailingOnly=TRUE)

datafile <- "TX_cvap_for_EI"
election <- args[1]
bufferCol <- args[2] == "buffer"
scaleVotes <- args[2] == "scaleVotes"
scalePop <- args[2] == "scalePop"
removeProblemPrecs <- args[3] == "noPPs"
usingCVAP <- args[4] == "CVAP"
outfile <- args[5]

print(paste("EI on election: ", election, sep=""))
print(paste("Saving EI results to ", outfile, sep=""))
print(paste("handling PPs by ", args[2], sep=""))
print(paste("removeProblemPrecs is set to ", removeProblemPrecs, sep=""))
print(paste("pop. data is ", args[4], sep=""))

elec_dict <- vector(mode="list")
elec_dict[['12G_President']] <- c('RomneyR_12G_President', 'ObamaD_12G_President')
elec_dict[['14R_US_Sen']] <- c('AlameelD_14R_US_Sen', 'RogersD_14R_US_Sen')
elec_dict[['16P_President']] <- c('JuddD_16P_President', 'De_La_FuenteD_16P_President', 'ClintonD_16P_President', 'LockeD_16P_President', 'OMalleyD_16P_President', 'SandersD_16P_President', 'WIlsonD_16P_President', 'HawesD_16P_President')
elec_dict[['18G_Comptroller']] <- c('HegarR_18G_Comptroller', 'ChevalierD_18G_Comptroller')
elec_dict[['18G_Governor']] <- c('AbbottR_18G_Governor', 'ValdezD_18G_Governor')
elec_dict[['18G_Land_Comm']] <- c('BushR_18G_Land_Comm', 'SuazoD_18G_Land_Comm')
elec_dict[['18G_Lt_Governor']] <- c('PatrickR_18G_Lt_Governor', 'CollierD_18G_Lt_Governor')
elec_dict[['18P_Lt_Governor']] <- c('CooperD_18P_Lt_Governor', 'CollierD_18P_Lt_Governor')
elec_dict[['16G_RR_Comm_1']] <- c('YarbroughD_16G_RR_Comm_1', 'ChristianR_16G_RR_Comm_1')

candidates <- elec_dict[[election]]
if (usingCVAP) {
  year <- substr(election, 0, 2)
  old_popcols <- c(paste("CVAP_20",year,sep=""), paste("BCVAP_20",year,sep=""), paste("HCVAP_20",year,sep=""))
} else {
  old_popcols <- c("VAP", "BVAP", "HVAP")
}
popcols <- c("TOTPOP", "BPOP", "HPOP") # general — could refer to VAP or CVAP

ntunes_val <- 10
tunedraws <- 1000
thin_mcmc <- 100
burnin_mcmc <- 1000
sample_mcmc <- 1000

wd <- getwd()
outfilePath <- paste(wd, "/../outputs/fig4_outputs/", outfile, ".csv", sep="")
b <- as.data.frame(read_delim(paste(wd, "/../resources/", datafile, ".csv", sep=""), col_names=T, delim=','))

# change name of population columns to TOTPOP, BPOP, HPOP
for (i in seq_along(popcols)) {
  names(b)[names(b) == old_popcols[i]] <- popcols[i]
}

bs <- b[b$TOTPOP > 0,] # only blocks with total pop > 0 (not necessary in Texas) actually it is, I think...

# aggregate BLACK AND HISP pop and vote by VTD in each county — unnecessary using our data...
a <- aggregate(bs[,c('TOTPOP','BPOP','HPOP',candidates)],
               by = list(bs$CNTYVTD),
               FUN = sum);
# a <- bs

a$OPOP <- a$TOTPOP - a$BPOP - a$HPOP

# not sure if this is necessary
a$TOTPOP[is.na(a$TOTPOP)] <- 0
a$BPOP[is.na(a$BPOP)] <- 0
a$HPOP[is.na(a$HPOP)] <- 0
a$OPOP[is.na(a$OPOP)] <- 0
for (candidate in candidates) {
  a[[candidate]][is.na(a[[candidate]])] <- 0
  a[[candidate]] <- round(a[[candidate]])
}

a$totvotes <- rowSums(a[,candidates])

if (removeProblemPrecs) {
  a <- a[a$TOTPOP >= a$totvotes,] # remove the problem VTDs (so buffers should be only 0)
}

# handle POP < totvotes cases differently...
if (bufferCol) {
  # add a buffer POP column
  a$bufferPOP <- 0
  a$bufferPOP[a$TOTPOP < a$totvotes] <- a$totvotes[a$TOTPOP < a$totvotes] - a$TOTPOP[a$TOTPOP < a$totvotes]
  a$TOTPOP <- a$BPOP + a$HPOP + a$OPOP + a$bufferPOP

  if (removeProblemPrecs && sum(a$bufferPOP != 0) != 0) {
    print("ERROR: We haven't removed all problem precincts! (first phase)")
    break
  }
} else if (scaleVotes) {
  # or scale votes DOWN to match POP
  a$factor <- 1
  a$factor[a$TOTPOP < a$totvotes] <- a$TOTPOP[a$TOTPOP < a$totvotes] / a$totvotes[a$TOTPOP < a$totvotes]
  for (candidate in candidates) {
    a[[candidate]] <- floor(a[[candidate]]*a$factor)
  }
  a$totvotes <- rowSums(a[,candidates])

  if (removeProblemPrecs && sum(a$factor != 1) != 0) {
    print("ERROR: We haven't removed all problem precincts! (first phase)")
    break
  }
} else if (scalePop) {
  # or scale POP UP to match votes
  a$factor <- 1
  a$factor[a$TOTPOP < a$totvotes] <- a$totvotes[a$TOTPOP < a$totvotes] / a$TOTPOP[a$TOTPOP < a$totvotes]
  a$BPOP <- ceiling(a$BPOP * a$factor)
  a$HPOP <- ceiling(a$HPOP * a$factor)
  a$OPOP <- ceiling(a$OPOP * a$factor)
  a$TOTPOP <- a$BPOP + a$HPOP + a$OPOP

  if (removeProblemPrecs && sum(a$factor != 1) != 0) {
    print("ERROR: We haven't removed all problem precincts! (first phase)")
    break
  }
} else {
  print("ERROR! You need to specify 'buffer', 'scaleVotes', or 'scalePop'")
  break
}

a$notvotes <- a$TOTPOP - a$totvotes

# identify formula: estimate vote/no vote for black, hisp, and other
if (bufferCol) {
  ei_formula <- paste('cbind(',paste(candidates,collapse = ', '),', notvotes) ~ cbind(BPOP, HPOP, OPOP, bufferPOP)', sep = '')
} else {
  ei_formula <- paste('cbind(',paste(candidates,collapse = ', '),', notvotes) ~ cbind(BPOP, HPOP, OPOP)', sep = '')
}

tune.nocov <- tuneMD(ei_formula,
                     data = a,
                     ntunes = ntunes_val,
                     totaldraws = tunedraws)
md.out <- ei.MD.bayes(ei_formula,
                      data = a,
                      sample = sample_mcmc,
                      thin = thin_mcmc,
                      burnin=burnin_mcmc,
                      ret.mcmc=TRUE,
                      tune.list = tune.nocov)
qq <-  md.out$draws$Beta

# print("Made it past EI")

candidates <- c(candidates, "notvotes")
num_c <- length(candidates)
groups <- c("BPOP", "HPOP", "OPOP")
num_d <- 3
if (bufferCol) {
  groups <- c(groups, "bufferPOP")
  num_d <- 4
}

i <- 1
for (candidate in candidates) {
  for (group in groups) {
    new_row_proportions <- paste("ei", group, candidate, sep="_")
    new_row_votes <- paste(group, candidate, "votes", sep="_")
    product <- num_c * num_d
    a[[new_row_proportions]] <- apply(qq[,c(product*(1:dim(a)[1])-(product-i))],2,mean)
    bs[[new_row_votes]] <- a[[new_row_proportions]][match(bs$CNTYVTD,a$Group.1)] * a[[group]][match(bs$CNTYVTD,a$Group.1)]
    i <- i + 1
  }
}


write.table(bs, outfilePath, row.names = F, col.names = (!file.exists(outfilePath)), append = file.exists(outfilePath), sep=',')
