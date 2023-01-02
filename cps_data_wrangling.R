# Confirmatory Preclinical Studies 
##################################

#setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/re_size/CPS")
source("./load_packages.R")
cps<-read.csv(file = "cps.csv", sep = ";", header = TRUE)

sapply(3:ncol(cps), function(i) {
  cps[, i] <<- as.numeric(gsub(",", ".", cps[, i]))
})


cps$orig_ci_low_z<-numeric(length(nrow(cps)))
cps$orig_ci_high_z<-numeric(length(nrow(cps)))
cps$orig_se_z<-numeric(length(nrow(cps)))
cps$orig_p_2sided<-numeric(length = nrow(cps))
cps$project<-"CPS"

for (i in 1:nrow(cps)) {
  cps$orig_ci_high_z[i]<-FisherZ(d_to_r(cps$orig_ci_high[i]))
  cps$orig_ci_low_z[i]<-FisherZ(d_to_r(cps$orig_ci_low[i]))
  cps$orig_se_z[i]<-ci2se(lower = cps$orig_ci_low_z[i], upper  = cps$orig_ci_high_z[i])
  cps$orig_p_2sided[i]<-ci2p(lower = cps$orig_ci_low_z[i], 
                             upper = cps$orig_ci_high_z[i],
                             alternative = "two.sided")
}



