# source additional functions
# source("functions_for_simulation.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./data/df_combined.RData")

# calculate sample size for replication

# Approach B:
# Replication study powered at 50% for the smallest effect size of interest (SESOI)
# SESOI 1 = 0.5 (for orig. effect size 0.5-1.5)
# SESOI 2 = 1.5 (for orig. effect size 1.5-2.5)
# SESOI 3 = 2.5 (for orig. effect size > 2.5)

# adjust SESOI based on orig_d
df_combined$SESOI <- 
  ifelse (abs(df_combined$orig_d) >= 0.5 & abs(df_combined$orig_d) <= 1.5, 0.5,
          ifelse(abs(df_combined$orig_d) >= 2.5, 2.5, 1.5))

rep_sample_size_b <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_b[i] <-
    ceiling(sample_size_b(data = df_combined[i, ],
                          SESOI = df_combined$SESOI[i],
                          power = 0.5))
  
}

df_combined$rep_sample_size_b <- rep_sample_size_b * 2

##################################
### Simulate replication study ###
### SCENARIO 1: ##################
##################################

# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:86) %>% 
  select(study_id, everything())

# set seed to reproduce results
set.seed(84335)

# number of experiments we run for each true underlying effect size
n_exp <- 5 

study_id_vector <- c(1:86)

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_b[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] / 2)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "t_value",
               "p_value", "effect", 
               "ci_low", "ci_high")

res_summary_rep_b <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_b <- 
  res_summary_rep_b %>% 
  group_by(study_id) %>% 
  summarize(mean_p = mean(p_value),
            mean_t = mean(t_value),
            mean_p = mean(p_value),
            mean_effect = mean(effect),
            mean_ci_low = mean(ci_low),
            mean_ci_high = mean(ci_high)) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         sesoi = df_combined$SESOI,
         rep_sample_size = df_combined$rep_sample_size_b,
         es_true = df_combined$orig_d / 2,
         sample_size_approach = "b",
         project = df_combined$project)

save(res_summary_b, file = "./data/res_summary_b.RData")
