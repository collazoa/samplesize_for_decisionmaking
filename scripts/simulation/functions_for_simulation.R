###############################################
### sample size calculation for replication ###
###############################################

# approach A: standard sample size calculation
# option to change power level

sample_size_a <- 
  
  function(data, sample_size, max_sample_size = 200,
           alpha = .05, power = .8) {
  
  aa <- data
  
  es_measured <- abs(aa$orig_d) # take initial study main effect
    # if(es_measured > 16) es_measured <- 16
    if(es_measured == 0) es_measured <- 0.0001
    
  
  bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                     type = "two.sample",
                     alternative = "one.sided")
  if (es_measured > 0) {
    if(bb$n > max_sample_size) 
      return(max_sample_size) 
    else return(bb$n)
    
   }
  
 }

# approach B: sample size calculation using SESOI and 50% power level
# option to change power level
# option to set SESOI

sample_size_b <- 
  
  function(data, sample_size, max_sample_size = 200,
           alpha = .05, SESOI, power = .8) {
    
    aa <- data
    
    es_measured <- SESOI
    
    bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                       type = "two.sample",
                       alternative = "one.sided")
    if (es_measured > 0) {
      if(bb$n > max_sample_size) 
        return(max_sample_size) 
      else return(bb$n)
      
    }
    
  }

# approach C: sample size calculation using lower 80% confidence bound
# option to change power level (we use .80)

# install.packages("remotes")
# remotes::install_github("GiulioCostantini/safeguardpower")
# install.packages("safeguardpower")

# require(pwr)
# require(MBESS)
# safeguard.d(d = .8, n.1 = 30, n.2 = 30, sig.level = .05, power = .8, conf = .80)    

sample_size_c <- 
  
  function(data, sample_size, max_sample_size = 200,
           alpha = .05, power = .8) {
    
    aa <- data
    
    es_measured <- aa$orig_ci_low # take lower 80% CI bound
    # if(es_measured > 16) es_measured <- 16
    if(es_measured == 0) es_measured <- 0.0001
    if(es_measured < 0) es_measured <- 0.0001
    
    
    bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                       type = "two.sample",
                       alternative = "one.sided")
    if (es_measured > 0) {
      if(bb$n > max_sample_size) 
        return(max_sample_size) 
      else return(bb$n)
      
    }
    
  }

####################################
### conducting replication study ###
####################################

# generate replication study data 

generate_study <- 
  function(ES_true = 1, sample_size = samp_size, l_bias = 0, pop_sd = 1) {
    
    ES_mod <- ES_true + l_bias
    sample_data <- data.frame(values = c(rnorm(sample_size/2, 0, pop_sd),
                                         rnorm(sample_size/2, ES_mod, pop_sd)),
                              treatment = rep(c("control", "atreat"),
                                              each = sample_size/2),
                              sample_size = sample_size)
    
    
    return(sample_data)
  }


get_summary_study_rep <- function(study_data) {
  
  t <- t.test(study_data$values ~ study_data$treatment,
              alternative = "greater",
              var.equal = FALSE,
              conf.level = .95)
  
  p_value <- t$p.value
  CI      <- t$conf.int
  effect  <- t$statistic/sqrt(nrow(study_data)) # one-sided t-test
  
  
  study_summary <-
    study_data %>%
    group_by(study_id, treatment) %>%
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>%
    mutate(t_value = round(t$statistic, 3),
           p_value = round(t$p.value, 3),
           ci_low = round(t$conf.int[1], 3),
           ci_high = round(t$conf.int[2], 3),
           effect = round(effect, 3))
  
  study_summary <-
    study_summary %>%
    group_by(study_id, t_value, p_value, ci_low, ci_high) %>%
    summarize(effect = mean(effect))
  
}




