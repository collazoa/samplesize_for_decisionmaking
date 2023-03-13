# preparing data set df_combined 

load("./data/df_combined.RData")

df_combined$zo <- df_combined$orig_z/df_combined$orig_se_z

# calculate sample size for replication

# Approach D:
# Replication study powered for reverse Bayesian approach (skeptical p-value)
# with an effect size shrinkage estimate of 25%  

rep_sample_size_d <- NULL
max_sample_size_total <- 280

for (i in 1:nrow(df_combined)) {
  rep_sample_size_d[i] <- ceiling(sample_size_d(data = df_combined[i,])) 
}

rep_sample_size_d[rep_sample_size_d == "NaN"] <- NA
#rep_sample_size_d[rep_sample_size_d > max_sample_size_total] <- max_sample_size_total

df_combined$rep_sample_size_d <- rep_sample_size_d



# We calculated sample sizes for pSceptical. This approach let's us decide which studies will 
# be replicated. Some are already sorted out at the stage of sample size calculation, either bc 
# a replication doesnt seem necessary given the original evidence, or an replication seems unfeasible bc 
# either sample sizes in the replication study would be far to high or because it would not be possible 
# to declare replication success for some of the studies 

df_combined$conducted <- vector(length = nrow(df_combined))

vec_not_nec <- ifelse(df_combined$rep_sample_size_d < 4 & is.na(df_combined$rep_sample_size_d) == FALSE, TRUE, FALSE)
vec_unfeasible <- ifelse(df_combined$rep_sample_size_d > 280 | is.na(df_combined$rep_sample_size_d) == TRUE, TRUE, FALSE)
vec_go <- ifelse(vec_unfeasible == FALSE & vec_not_nec == FALSE, TRUE, FALSE)


vec_conducted <- vector(length = nrow(df_combined))
vec_conducted[which(vec_go == TRUE)] <- "yes"
vec_conducted[which(vec_unfeasible == TRUE)] <- "unfeasible"
vec_conducted[which(vec_not_nec == TRUE)] <- "not_necessary"

df_combined$conducted <- vec_conducted

decision1 <- df_combined%>%group_by(conducted)%>%summarize(n = n())%>%mutate(approach = "pSceptical")
decision1