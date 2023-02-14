# setwd("~/Desktop/samplesize_for_decisionmaking")

# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/bri_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/crp_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/cps_data_wrangling.R")


sel<-colnames(crp) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_z", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
crp<-crp[,sel]


sel<-colnames(bri) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_z", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
bri<-bri[,sel]

sel<-colnames(cps) %in% c("orig_ss", "orig_d", "orig_ci_low", 
                          "orig_ci_high", "orig_z", "orig_ci_low_z",
                          "orig_ci_high_z", "orig_se_z", 
                          "orig_p_2sided", "project")
cps<-cps[,sel]

df_combined <- rbind(cps, crp, bri)

# check for negative effect sizes and CIs
# for negative effect sizes and respective CIs change sign
# to work with positive effect sizes only in the simulation
#sum(df_combined$orig_ci_low < 0)
#sum(df_combined$orig_ci_high < 0)

#neg_es <- 
#  df_combined %>% 
#  filter(df_combined$orig_d < 0)

#sum(neg_es$orig_ci_low < 0)
#sum(neg_es$orig_ci_high < 0)

#neg_d <- which(df_combined$orig_d < 0)

#df_combined$orig_d <- abs(df_combined$orig_d)

#df_combined$orig_ci_low[neg_d] <- abs(df_combined$orig_ci_low[neg_d])

#df_combined$orig_ci_high[neg_d] <- abs(df_combined$orig_ci_high[neg_d])

# min(df_combined$orig_d)

# check for lower CIs that are negative but have positive effect sizes
# these are skipped in the simulation with safeguard approach (approach C)
# neg_ci <- 
#   df_combined %>% 
#   filter(orig_ci_low < 0)

# are all results significant?
# sum(df_combined$orig_p_2sided <= 0.05)
# which(df_combined$orig_p_2sided > 0.05)


save(df_combined, file = "./data/df_combined.RData")

