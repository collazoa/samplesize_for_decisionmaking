
setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./bri_data_wrangling.R")
setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./crp_data_wrangling.R")
setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./cps_data_wrangling.R")



sel<-colnames(crp) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
crp<-crp[,sel]


sel<-colnames(bri) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
bri<-bri[,sel]

sel<-colnames(cps) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
cps<-cps[,sel]

df<-rbind(cps, crp, bri)
