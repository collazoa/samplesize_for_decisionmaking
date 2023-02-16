sample_size_d <- 
  
  function(data, power = .8, shrinkage = .25) {
    
    aa <- data 
    
    
    bb <- sampleSizeReplicationSuccess(zo = data$zo, power = power, level = 0.025, type = "golden", 
                                       alternative = "one.sided", designPrior = "conditional", 
                                       shrinkage = shrinkage)
    
    return(bb * data$orig_ss)
  }


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




get_summary_study_rep_pSceptical <- function(study_data, level = .025, alpha_s = 0.062) {
  
  re<-esc_mean_sd(
    grp2m = mean(study_data[study_data$intervention == "control",]$values),
    grp2sd = sd(study_data[study_data$intervention == "control",]$values), 
    grp2n = length(study_data[study_data$intervention == "control",]$values),
    grp1m = mean(study_data[study_data$intervention == "atreat",]$values),
    grp1sd = sd(study_data[study_data$intervention == "atreat",]$values),
    grp1n = length(study_data[study_data$intervention == "atreat",]$values),
    es.type = "d")
  
  study_summary <-
    study_data %>%
    group_by(study_id, intervention) %>%
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>%
    mutate(rep_d = re$es,
           ci_low = re$ci.lo,
           ci_high = re$ci.hi,
           rep_z =  FisherZ(d_to_r(rep_d)),
           rep_ci_low_z = FisherZ(rho = d_to_r(ci_low)), 
           rep_ci_high_z = FisherZ(rho = d_to_r(ci_high)),
           rep_se_z = ci2se(lower = rep_ci_low_z, upper = rep_ci_high_z), 
           rep_p_value = ci2p(lower = rep_ci_low_z, upper = rep_ci_high_z, alternative = "greater"), 
           zr = rep_z/rep_se_z)
  
  study_summary <- 
    study_summary %>%
    select(study_id, effect = rep_d, 
           ci_low, ci_high, p_value = rep_p_value, 
           rep_z, rep_se_z, zr)%>%
    mutate(zo = study_data$zo[1], 
           orig_ss = study_data$orig_ss[1], 
           rep_ss = study_data$rep_ss[1])
  
  study_summary <- study_summary[1,]
  
  pScep <- pSceptical(zo = study_summary$zo, 
                      zr = study_summary$zr, 
                      c = study_summary$orig_ss/study_summary$rep_ss,
                      alternative = "one.sided", 
                      type = "golden")
  
  study_summary <- 
    study_summary %>%
    mutate(pScep = pScep,
           success = ifelse(pScep < alpha_s, TRUE, FALSE))
}

