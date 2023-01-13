# source additional functions
# source("functions_for_simulation.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./data/df_combined.RData")

# calculate sample size for replication

# Approach C:
# Replication study powered at 80% for the lower 80% confidence bound 
# obtained from the original study

rep_sample_size_c <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_c[i] <-
    ceiling(sample_size_c(data = df_combined[i, ],
                          power = 0.8))
  
}

df_combined$rep_sample_size_c <- rep_sample_size_c * 2

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
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
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

rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "t_value",
               "p_value", "effect", 
               "ci_low", "ci_high")

res_summary_rep_c <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <- 
  res_summary_rep_c %>% 
  group_by(study_id) %>% 
  summarize(mean_p = mean(p_value),
            mean_t = mean(t_value),
            mean_p = mean(p_value),
            mean_effect = mean(effect),
            mean_ci_low = mean(ci_low),
            mean_ci_high = mean(ci_high)) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         sesoi = df_combined$SESOI,
         rep_sample_size = df_combined$rep_sample_size_c,
         es_true = df_combined$orig_d / 2,
         sample_size_approach = "c",
         project = df_combined$project)


save(res_summary_c, file = "./data/res_summary_c.RData")

