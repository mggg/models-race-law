library(tidyverse)
library(eiPack)
library(rgdal)
library(foreign)
library(readr)
library(optparse)

args <- commandArgs(trailingOnly=TRUE)

datafile <- "TX_data_for_Chen_EI"
bufferCol <- args[1] == "buffer"
scaleVotes <- args[1] == "scaleVotes"
scalePop <- args[1] == "scalePop"
removeProblemPrecs <- args[2] == "noPPs"
usingCVAP <- args[3] == "CVAP"
outfile <- args[4]

print(paste("Saving EI results to ", outfile, sep=""))
print(paste("handling PPs by ", args[1], sep=""))
print(paste("removeProblemPrecs is set to ", removeProblemPrecs, sep=""))
print(paste("usingCVAP is set to ", usingCVAP, sep=""))

if (usingCVAP) {
  old_popcols <- c("CVAP_2012", "BCVAP_2012", "HCVAP_2012")
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
outfilePath <- paste(wd, "/../outputs/", outfile, ".csv", sep="")
b <- as.data.frame(read_delim(paste(wd, "/../resources/", datafile, ".csv", sep=""), col_names=T, delim=','))
for (i in seq_along(popcols)) {
  names(b)[names(b) == old_popcols[i]] <- popcols[i]
}

ctys <- unique(b$CNTY_x)
for(cty in ctys){
  print(paste('cty:',cty))
  bs <- b[b$CNTY_x==cty,] # subset to single county
  bs <- bs[bs$TOTPOP > 0,] # only blocks with total pop > 0 (not necessary in Texas) actually it is, I think...

  # aggregate BLACK AND HISP pop and vote by VTD in each county — unnecessary using our data...
  a <- aggregate(bs[,c('TOTPOP','BPOP','HPOP','Obama','Romney')],
                 by = list(bs$CNTYVTD),
                 FUN = sum);

  a$OPOP <- a$TOTPOP - a$BPOP - a$HPOP

  # not sure if this is necessary
  a$TOTPOP[is.na(a$TOTPOP)] <- 0
  a$BPOP[is.na(a$BPOP)] <- 0
  a$HPOP[is.na(a$HPOP)] <- 0
  a$OPOP[is.na(a$OPOP)] <- 0
  a$Obama[is.na(a$Obama)] <- 0
  a$Romney[is.na(a$Romney)] <- 0

  a$Obama <- round(a$Obama)
  a$Romney <- round(a$Romney)
  a$totvotes <- a$Romney + a$Obama

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
    a$Obama <- floor(a$Obama*a$factor)
    a$Romney <- floor(a$Romney*a$factor);
    a$totvotes <- a$Romney + a$Obama

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
    ei_formula <- cbind(Romney, Obama, notvotes) ~ cbind(BPOP, HPOP, OPOP, bufferPOP)
  } else {
    ei_formula <- cbind(Romney, Obama, notvotes) ~ cbind(BPOP, HPOP, OPOP)
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

  if (bufferCol) {
    # estimate vote prefs
    if (dim(a)[1]>1) {
      a$ei.Brom <- apply(qq[,c(12*(1:dim(a)[1])-11)],2,mean) # every 12 starting at index 1
      a$ei.Hrom <- apply(qq[,c(12*(1:dim(a)[1])-10)],2,mean) # every 12 starting at index 2
      a$ei.Orom <- apply(qq[,c(12*(1:dim(a)[1])-9)],2,mean) # every 12 starting at index 3
      a$ei.Boba <- apply(qq[,c(12*(1:dim(a)[1])-7)],2,mean) # every 12 starting at index 5
      a$ei.Hoba <- apply(qq[,c(12*(1:dim(a)[1])-6)],2,mean) # every 12 starting at index 6
      a$ei.Ooba <- apply(qq[,c(12*(1:dim(a)[1])-5)],2,mean) # every 12 starting at index 3
    } else {
      a$ei.Brom <- mean(qq[,c(12*(1:dim(a)[1])-11)])
      a$ei.Hrom <- mean(qq[,c(12*(1:dim(a)[1])-10)])
      a$ei.Orom <- mean(qq[,c(12*(1:dim(a)[1])-9)])
      a$ei.Boba <- mean(qq[,c(12*(1:dim(a)[1])-7)])
      a$ei.Hoba <- mean(qq[,c(12*(1:dim(a)[1])-6)])
      a$ei.Ooba <- mean(qq[,c(12*(1:dim(a)[1])-5)])
    }
  } else {
    # estimate votes pref
    if (dim(a)[1]>1) {
      a$ei.Brom <- apply(qq[,c(9*(1:dim(a)[1])-8)],2,mean) # every 9 starting at index 1
      a$ei.Hrom <- apply(qq[,c(9*(1:dim(a)[1])-7)],2,mean) # every 9 starting at index 2
      a$ei.Orom <- apply(qq[,c(9*(1:dim(a)[1])-6)],2,mean) # every 9 starting at index 3
      a$ei.Boba <- apply(qq[,c(9*(1:dim(a)[1])-5)],2,mean) # every 9 starting at index 4
      a$ei.Hoba <- apply(qq[,c(9*(1:dim(a)[1])-4)],2,mean) # every 9 starting at index 5
      a$ei.Ooba <- apply(qq[,c(9*(1:dim(a)[1])-3)],2,mean) # every 9 starting at index 6
    } else {
      a$ei.Brom <- mean(qq[,c(9*(1:dim(a)[1])-8)])
      a$ei.Hrom <- mean(qq[,c(9*(1:dim(a)[1])-7)])
      a$ei.Orom <- mean(qq[,c(9*(1:dim(a)[1])-6)])
      a$ei.Boba <- mean(qq[,c(9*(1:dim(a)[1])-5)])
      a$ei.Hoba <- mean(qq[,c(9*(1:dim(a)[1])-4)])
      a$ei.Ooba <- mean(qq[,c(9*(1:dim(a)[1])-3)])
    }
  }

  # get votes
  bs$BRvotes <- a$ei.Brom[match(bs$CNTYVTD,a$Group.1)] * a$BPOP[match(bs$CNTYVTD,a$Group.1)]
  bs$HRvotes <- a$ei.Hrom[match(bs$CNTYVTD,a$Group.1)] * a$HPOP[match(bs$CNTYVTD,a$Group.1)]
  bs$ORvotes <- a$ei.Orom[match(bs$CNTYVTD,a$Group.1)] * a$OPOP[match(bs$CNTYVTD,a$Group.1)]
  bs$BDvotes <- a$ei.Boba[match(bs$CNTYVTD,a$Group.1)] * a$BPOP[match(bs$CNTYVTD,a$Group.1)]
  bs$HDvotes <- a$ei.Hoba[match(bs$CNTYVTD,a$Group.1)] * a$HPOP[match(bs$CNTYVTD,a$Group.1)]
  bs$ODvotes <- a$ei.Ooba[match(bs$CNTYVTD,a$Group.1)] * a$OPOP[match(bs$CNTYVTD,a$Group.1)]

  # get turnout
  bs$bvotes <- bs$BRvotes + bs$BDvotes
  bs$hvotes <- bs$HRvotes + bs$HDvotes
  bs$ovotes <- bs$ORvotes + bs$ODvotes

  write.table(bs, outfilePath, row.names = F, col.names = (!file.exists(outfilePath)), append = file.exists(outfilePath), sep=',')
}
