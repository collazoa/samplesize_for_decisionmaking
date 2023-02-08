# source additional functions
# source("functions_for_simulation.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./data/df_combined.RData")

# calculate sample size for replication

# Approach C:
# Replication study powered at 80% for the lower 80% confidence bound 
# obtained from the original study
# if lower CI bound < 0, the rep_sample_size_c column will display NA

rep_sample_size_c <- NULL

for (i in 1:nrow(df_combined)) {
  
  if (df_combined$orig_ci_low[i] > 0) {
    
    rep_sample_size_c[i] <-
      ceiling(sample_size_c(data = df_combined[i, ],
                            power = 0.8))
    
  } else {
    
    rep_sample_size_c[i] <- NA
    
  }
  
}

df_combined$rep_sample_size_c <- rep_sample_size_c * 2

sum(is.na(df_combined$rep_sample_size_c))

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
n_exp <- 10

study_id_vector <- c(1:86)

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        
        # insert a function to meta-analyse the two effect sizes
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] / 2)
      
    }
    
}

# row_names <- NULL
# col_names <- c("study_id", "t_value",
#                "p_value", "effect", 
#                "ci_low", "ci_high")
# 
# res_summary_rep_c <- 
#   as_tibble(matrix(unlist(rep_data_summary),
#                    nrow = n_exp * length(study_id_vector), byrow = TRUE,
#                    dimnames = list(c(row_names),
#                                    c(col_names))))
# 
# res_summary_c <- 
#   res_summary_rep_c %>% 
#   group_by(study_id) %>% 
#   summarize(n_success = ,
#             N = n(),
#             pct_success = n_success/N * 100) %>% 
#   mutate(orig_ss = df_combined$orig_ss,
#          rep_sample_size = df_combined$rep_sample_size_c,
#          es_true = df_combined$orig_d / 2,
#          sample_size_approach = "c",
#          project = df_combined$project,
#          scenario = "m_error")


# save(res_summary_c_m_err, file = "./data/res_summary_c_m_err.RData")


##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

# row_names <- NULL
# col_names <- c("study_id", "t_value",
#                "p_value", "effect", 
#                "ci_low", "ci_high")
# 
# res_summary_rep_c <- 
#   as_tibble(matrix(unlist(rep_data_summary),
#                    nrow = n_exp * length(study_id_vector), byrow = TRUE,
#                    dimnames = list(c(row_names),
#                                    c(col_names))))
# 
# res_summary_c <- 
#   res_summary_rep_c %>% 
#   group_by(study_id) %>% 
#   summarize(n_success = ,
#             N = n(),
#             pct_success = n_success/N * 100) %>% 
#   mutate(orig_ss = df_combined$orig_ss,
#          rep_sample_size = df_combined$rep_sample_size_c,
#          es_true = 0,
#          sample_size_approach = "c",
#          project = df_combined$project,
#          scenario = "null_effect")

# save(res_summary_c_null, file = "./data/res_summary_c_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################

# row_names <- NULL
# col_names <- c("study_id", "t_value",
#                "p_value", "effect", 
#                "ci_low", "ci_high")
# 
# res_summary_rep_c <- 
#   as_tibble(matrix(unlist(rep_data_summary),
#                    nrow = n_exp * length(study_id_vector), byrow = TRUE,
#                    dimnames = list(c(row_names),
#                                    c(col_names))))
# 
# res_summary_c <- 
#   res_summary_rep_c %>% 
#   group_by(study_id) %>% 
#   summarize(n_success = ,
#             N = n(),
#             pct_success = n_success/N * 100) %>% 
#   mutate(orig_ss = df_combined$orig_ss,
#          rep_sample_size = df_combined$rep_sample_size_c,
#          es_true = df_combined$orig_d - (1.25 * df_combined$orig_d),
#          sample_size_approach = "c",
#          project = df_combined$project,
#          scenario = "s_error")

# save(res_summary_c_s_err, file = "./data/res_summary_c_s_err.RData")


res_summary_c <- 
  bind_rows(res_summary_c_m_err, 
            res_summary_c_null, 
            res_summary_c_s_err)

save(res_summary_c, file = "./data/res_summary_c.RData")
