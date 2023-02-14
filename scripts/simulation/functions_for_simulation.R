###############################################
### sample size calculation for replication ###
###############################################

# approach A: standard sample size calculation
# option to change power level

sample_size_a <- 
  
  function(data, sample_size, max_sample_size = 140,
           alpha = .05, power = .8) {
  
  aa <- data
  
  es_measured <- abs(aa$orig_d) # take initial study main effect
  
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
  
  function(data, sample_size, max_sample_size = 140,
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
  
  function(data, sample_size, max_sample_size = 140,
           alpha = .05, power = .8) {
    
    aa <- data
    
    es_measured <- aa$orig_ci_low # take lower 80% CI bound
    
    bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                       type = "two.sample",
                       alternative = "one.sided")
    if (es_measured > 0) {
      if(bb$n > max_sample_size) 
        return(max_sample_size) 
      else return(bb$n)
      
    }

  }

# approach D, calculating sample size for Replication Success according to the pSceptical (Held 2020)
# allowing for a 25% shrinkage of the effect size 

sample_size_d <- 
  
  function(data, max_sample_size = 140, power = .8, shrinkage = .25) {
    
    aa <- data 
    
    zo <- data$orig_z/data$orig_se_z
    
    bb <- sampleSizeReplicationSuccess(zo = zo, power = power, level = 0.025, type = "golden", 
                                       alternative = "one.sided", designPrior = "conditional", 
                                       shrinkage = shrinkage)
    
    return(bb * data$orig_ss)
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
                              intervention = rep(c("control", "atreat"),
                                              each = sample_size/2),
                              sample_size = sample_size)
    
    
    return(sample_data)
  }




get_summary_study_rep <- function(study_data) {
  
  t <- t.test(study_data$values ~ study_data$intervention,
              alternative = "greater",
              var.equal = FALSE,
              conf.level = .95)
  
  
  study_summary <-
    study_data %>%
    group_by(study_id, intervention) %>%
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>%
    mutate(p_value = round(t$p.value, 3))
  
  effect <- 
    (study_summary$mean_group[1] - study_summary$mean_group[2]) /
    sqrt((study_summary$sd_group[1]^2 + study_summary$sd_group[2]^2)/2)
  
  study_summary <-
    study_summary %>%
    group_by(study_id, p_value) %>%
    summarize(effect = mean(effect))
  
}


get_summary_study_rep_pSceptical <- function(study_data) {
  
  re<-esc_mean_sd(
    grp2m = mean(study_data[study_data$treatment == "A",]$values),
    grp2sd = sd(study_data[study_data$treatment == "A",]$values), 
    grp2n = length(study_data[study_data$treatment == "A",]$values),
    grp1m = mean(study_data[study_data$treatment == "B",]$values),
    grp1sd = sd(study_data[study_data$treatment == "B",]$values),
    grp1n = length(study_data[study_data$treatment == "B",]$values),
    es.type = "d")
  
  study_summary <-
    study_data %>%
    group_by(study_id, treatment) %>%
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>%
    mutate(rep_d = re$es,
           ci_low = re$ci.lo,
           ci_high = re$ci.hi,
           rep_z =  FisherZ(d_to_r(rep_d)),
           rep_ci_low_z = FisherZ(rho = d_to_r(ci_low)), 
           rep_ci_high_z = FisherZ(rho = d_to_r(ci_high)),
           rep_se_z = ci2se(lower = rep_ci_low_z, upper = rep_ci_high_z), 
           rep_p_value = ci2p(lower = rep_ci_low_z, upper = rep_ci_high_z, alternative = "greater"))
  
}


