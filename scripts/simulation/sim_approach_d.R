# source additional functions
source("./scripts/simulation/functions_for_simulation.R")
# source("./scripts/simulation/functions_for_simulation_d.R")
source("./scripts/data_wrangling/load_packages.R")


##################################
### Simulate replication study ###
### SCENARIO 1: ##################
##################################

# load combined data of all three replication projects
source("./scripts/simulation/prepare_df_combined.R")

# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:86, 
         es_true = df_combined$orig_d/2, 
         scenario = "m_error") %>% 
  select(study_id, everything())

# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 100

study_id_vector <- which(df_combined$conducted == "yes")

# now use replication sample size 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id]/2,
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id]/2, 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}

# extracting results from the list to form data frames with the results, averaged
# over the n_exp experiments 

row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", 
               "rep_z", "rep_se_z", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_d

res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = df_combined$orig_d[study_id_vector]/ 2 ,
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "m_error")



res_summary_d <- left_join(df_combined, res_summary_d) 

res_summary_d <- 
  res_summary_rep_d %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))

res_summary_d_m_error <- res_summary_d

save(res_summary_d_m_error, file = "./data/res_summary_d_m_error.RData")

##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

source("./scripts/simulation/prepare_df_combined.R")
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:86, 
         es_true = 0, 
         scenario = "null_effect") %>% 
  select(study_id, everything())


# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 100

study_id_vector <- which(df_combined$conducted == "yes")


# now use replication sample size to simulate new experiments 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = 0, 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}


row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", 
               "rep_z", "rep_se_z", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_d


res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = 0,
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "null_effect")

  
res_summary_d <- left_join(df_combined, res_summary_d) 

res_summary_d <- 
  res_summary_d %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))

res_summary_d_null <- res_summary_d

save(res_summary_d_null, file = "./data/res_summary_d_null.RData")

##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################
source("./scripts/simulation/prepare_df_combined.R")

df_combined <-
  df_combined %>% 
  mutate(study_id = 1:86, 
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]), 
         scenario = "s_error") %>% 
  select(study_id, everything())


# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 100 

study_id_vector <- which(df_combined$conducted == "yes")


# now use replication sample size to simulate new experiments 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]), 
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]), 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multicore)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}


row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", 
               "rep_z", "rep_se_z", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_rep_d
save(res_summary_rep_d, file = "./data/res_summary_rep_d.RData")

res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "s_error")


res_summary_d <- left_join(df_combined, res_summary_d) 

res_summary_d <- 
  res_summary_d %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))

res_summary_d_s_error <- res_summary_d


res_summary_d <- rbind(res_summary_d_m_error, res_summary_d_null, res_summary_d_s_error)

res_summary_d$sample_size_approach <- "d"

# save(res_summary_d, file = "./data/res_summary_d.RData")
