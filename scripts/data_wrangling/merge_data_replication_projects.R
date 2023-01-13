setwd("~/Desktop/samplesize_for_decisionmaking")

# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/bri_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/crp_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/cps_data_wrangling.R")


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

df_combined <- rbind(cps, crp, bri)

save(df_combined, file = "./data/df_combined.RData")
