# source additional functions
# source("functions_for_simulation.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./data/df_combined.RData")

# calculate sample size for replication

# Approach B:
# Replication study powered at 50% for the smallest effect size of interest (SESOI)
# SESOI 1 = 0.5
# SESOI 2 = 1.0

# set SESOI to 0.5
df_combined$SESOI <- 0.5

rep_sample_size_b_0.5 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_b_0.5[i] <-
    ceiling(sample_size_b(data = df_combined[i, ],
                          SESOI = df_combined$SESOI[i],
                          power = 0.5))
  
}

df_combined$rep_sample_size_b_0.5 <- rep_sample_size_b_0.5 * 2

# set SESOI to 1.0
df_combined$SESOI <- 1.0

rep_sample_size_b_1.0 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_b_1.0[i] <-
    ceiling(sample_size_b(data = df_combined[i, ],
                          SESOI = df_combined$SESOI[i],
                          power = 0.5))
  
}

df_combined$rep_sample_size_b_1.0 <- rep_sample_size_b_1.0 * 2

df_combined <-
  df_combined %>% 
  select(-SESOI)

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

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 0.5),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = df_combined$orig_d / 2,
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "m_error")




# now use replication sample size with SESOI = 1.0
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_b_1.0[study_id])
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_1.0 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_1.0 <- 
  res_summary_rep_b_1.0 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 1.0),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_1.0,
         es_true = df_combined$orig_d / 2,
         sample_size_approach = "b_1.0",
         project = df_combined$project,
         scenario = "m_error")

res_summary_b_m_err <-
  bind_rows(res_summary_rep_b_0.5, res_summary_rep_b_1.0)

# save(res_summary_b_m_err, file = "./data/res_summary_b_m_err.RData")

##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

set.seed(84335)

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = 0)
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 0.5),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = 0,
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "null_effect")


# now use replication sample size with SESOI = 1.0
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_b_1.0[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = 0)
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_1.0 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_1.0 <- 
  res_summary_rep_b_1.0 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 1.0),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_1.0,
         es_true = 0,
         sample_size_approach = "b_1.0",
         project = df_combined$project,
         scenario = "null_effect")

res_summary_b_null <-
  bind_rows(res_summary_rep_b_0.5, res_summary_rep_b_1.0)

# save(res_summary_b_null, file = "./data/res_summary_b_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################

set.seed(84335)

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]))
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 0.5),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = df_combined$orig_d - (1.25 * df_combined$orig_d),
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "s_error")


# now use replication sample size with SESOI = 1.0
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_b_1.0[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]))
      
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
col_names <- c("study_id", "t_value", "p_value", 
               "ci_low", "ci_high", "effect")

res_summary_rep_b_1.0 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_b_1.0 <- 
  res_summary_rep_b_1.0 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 1.0),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_1.0,
         es_true = df_combined$orig_d - (1.25 * df_combined$orig_d),
         sample_size_approach = "b_1.0",
         project = df_combined$project,
         scenario = "s_error")

res_summary_b_s_err <-
  bind_rows(res_summary_rep_b_0.5, res_summary_rep_b_1.0)

# save(res_summary_b_s_err, file = "./data/res_summary_b_s_err.RData")


res_summary_b <- 
  bind_rows(res_summary_b_m_err, 
            res_summary_b_null, 
            res_summary_b_s_err)

save(res_summary_b, file = "./data/res_summary_b.RData")
