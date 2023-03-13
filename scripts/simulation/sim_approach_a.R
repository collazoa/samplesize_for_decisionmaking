# source additional functions
# source("functions_for_simulation.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./data/df_combined.RData")

# calculate sample size for replication

# Approach A:
# Replication study powered for the effect size obtained 
# in the original study at 80% and 95% respectively
# take only significant studies

# how many of the original studies yielded a significant result?
sum(df_combined$orig_p_2sided <= 0.05)

# for simulation only take significant results
helper <- which(df_combined$orig_p_2sided <= 0.05)

# power level = 80%
rep_sample_size_a_80 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_a_80[i] <-
    ceiling(sample_size_a(data = df_combined[i, ],
                          power = 0.8))
  
}

df_combined$rep_sample_size_a_80 <- rep_sample_size_a_80 * 2

# power level = 95%
rep_sample_size_a_95 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_a_95[i] <-
    ceiling(sample_size_a(data = df_combined[i, ],
                          power = 0.95))
  
}

df_combined$rep_sample_size_a_95 <- rep_sample_size_a_95 * 2

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
n_exp <- 2

study_id_vector <- c(1:86)

# first use replication sample size with 80% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_a_80[study_id])
      
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
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_80 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_80 <- 
  res_summary_rep_a_80 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_80[helper],
         es_true = df_combined$orig_d[helper] / 2,
         sample_size_approach = "a_80pct",
         project = df_combined$project[helper],
         scenario = "m_error")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_80 <-
  bind_rows(helper_dat, res_summary_rep_a_80) %>% 
  arrange(study_id) %>% 
  mutate(es_true = df_combined$orig_d[study_id] / 2,
         sample_size_approach = "a_80pct",
         scenario = "m_error")
  

# now use replication sample size with 95% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
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
col_names <- c("study_id", "p_value", "effect")

res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_95[helper],
         es_true = df_combined$orig_d[helper] / 2,
         sample_size_approach = "a_95pct",
         project = df_combined$project[helper],
         scenario = "m_error")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_95 <-
  bind_rows(helper_dat, res_summary_rep_a_95) %>% 
  arrange(study_id) %>% 
  mutate(es_true = df_combined$orig_d[study_id] / 2,
         sample_size_approach = "a_95pct",
         scenario = "m_error")


res_summary_a_m_err <-
  bind_rows(res_summary_rep_a_80, res_summary_rep_a_95)

# save(res_summary_a_m_err, file = "./data/res_summary_a_m_err.RData")

##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

set.seed(84335)

# first use replication sample size with 80% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_a_80[study_id])
      
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
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_80 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_80 <- 
  res_summary_rep_a_80 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_80[helper],
         es_true = 0,
         sample_size_approach = "a_80pct",
         project = df_combined$project[helper],
         scenario = "null_effect")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_80 <-
  bind_rows(helper_dat, res_summary_rep_a_80) %>% 
  arrange(study_id) %>% 
  mutate(es_true = 0,
         sample_size_approach = "a_80pct",
         scenario = "null_effect")


# now use replication sample size with 95% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
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

rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_95[helper],
         es_true = 0,
         sample_size_approach = "a_95pct",
         project = df_combined$project[helper],
         scenario = "null_effect")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_95 <-
  bind_rows(helper_dat, res_summary_rep_a_95) %>% 
  arrange(study_id) %>% 
  mutate(es_true = 0,
         sample_size_approach = "a_95pct",
         scenario = "null_effect")

res_summary_a_null <-
  bind_rows(res_summary_rep_a_80, res_summary_rep_a_95)

# save(res_summary_a_null, file = "./data/res_summary_a_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################

set.seed(84335)

# first use replication sample size with 80% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_a_80[study_id])
      
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

rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_80 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_80 <- 
  res_summary_rep_a_80 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_80[helper],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "a_80pct",
         project = df_combined$project[helper],
         scenario = "s_error")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_80 <-
  bind_rows(helper_dat, res_summary_rep_a_80) %>% 
  arrange(study_id) %>% 
  mutate(es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "a_80pct",
         scenario = "s_error")


# now use replication sample size with 95% power
list_rep_data <- 
  
  foreach(study_id = study_id_vector[helper]) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
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
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector[helper]), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[helper],
         rep_sample_size = df_combined$rep_sample_size_a_95[helper],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "a_95pct",
         project = df_combined$project[helper],
         scenario = "s_error")

helper_dat <-
  df_combined %>% 
  filter(orig_p_2sided > 0.05) %>% 
  select(study_id, orig_ss, project)

res_summary_rep_a_95 <-
  bind_rows(helper_dat, res_summary_rep_a_95) %>% 
  arrange(study_id) %>% 
  mutate(es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "a_95pct",
         scenario = "s_error")


res_summary_a_s_err <-
  bind_rows(res_summary_rep_a_80, res_summary_rep_a_95)

# save(res_summary_a_s_err, file = "./data/res_summary_a_s_err.RData")

res_summary_a <- 
  bind_rows(res_summary_a_m_err, 
            res_summary_a_null, 
            res_summary_a_s_err)

res_summary_a$conducted <- 
  ifelse(is.na(res_summary_a$rep_sample_size) | res_summary_a$rep_sample_size < 4, "not_necessary",
         ifelse(res_summary_a$rep_sample_size >= 280, "unfeasible", "yes"))


# save(res_summary_a, file = "./data/res_summary_a.RData")
