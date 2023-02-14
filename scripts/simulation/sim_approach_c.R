setwd("~/Desktop/samplesize_for_decisionmaking")

# source packages and additional functions
source("./scripts/simulation/functions_for_simulation.R")
source("./scripts/data_wrangling/load_packages.R")


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


# test <- 
#   df_combined %>% 
#   filter(is.na(rep_sample_size_c))

helper <- which(df_combined$rep_sample_size_c != is.na(df_combined$rep_sample_size_c))

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
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        
        rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] / 2,
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
for (i in 1:length(study_id_vector[helper])) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_c[helper],
         es_true = df_combined$orig_d[helper] / 2,
         sample_size_approach = "c",
         project = df_combined$project[helper],
         scenario = "m_error")


# save(res_summary_c_m_err, file = "./data/res_summary_c_m_err.RData")


##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

set.seed(84335)

list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        
        rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = 0)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector[helper])) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = ,
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_c[helper],
         es_true = 0,
         sample_size_approach = "c",
         project = df_combined$project[helper],
         scenario = "null_effect")

# save(res_summary_c_null, file = "./data/res_summary_c_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################

set.seed(84335)

list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        
        rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]))
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector[helper])) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = ,
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_c[helper],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "c",
         project = df_combined$project[helper],
         scenario = "null_effect")

# save(res_summary_c_s_err, file = "./data/res_summary_c_s_err.RData")


res_summary_c <- 
  bind_rows(res_summary_c_m_err, 
            res_summary_c_null, 
            res_summary_c_s_err)

# save(res_summary_c, file = "./data/res_summary_c.RData")
