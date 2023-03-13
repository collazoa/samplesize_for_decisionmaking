

power.t.test(delta = 0.77000000, sd = 1, sig.level = 0.05, power = 0.8,
             type = "two.sample",
             alternative = "one.sided")


df_combined$SESOI <- 
  ifelse (abs(df_combined$orig_d) >= 0.5 & abs(df_combined$orig_d) <= 1.5, 0.5,
          ifelse(abs(df_combined$orig_d) >= 2.5, 2.5, 1.5))

df_short <-
  df_combined %>% 
  select(orig_d, SESOI)


sum((abs(df_combined$orig_d) >= 0.5 & abs(df_combined$orig_d) <= 1.5))
sum(df_combined$SESOI == 0.5)

sum((abs(df_combined$orig_d) > 1.5 & abs(df_combined$orig_d) <= 2.5))
sum(df_combined$SESOI == 1.5)

sum((abs(df_combined$orig_d) > 2.5))
sum(df_combined$SESOI == 2.5)



study_data <- list_rep_data[[3]][[2]]





generate_study_test <- 
  function(ES_true = 1, sample_size = samp_size, l_bias = 0, pop_sd = 1) {
    
    ES_mod <- ES_true + l_bias
    sample_data <- data.frame(values = c(rnorm(sample_size/2, 0, pop_sd),
                                         rnorm(sample_size/2, ES_mod, pop_sd)),
                              treatment = rep(c("control", "atreat"),
                                              each = sample_size/2),
                              sample_size = sample_size)
    
    
    return(sample_data)
  }


library(compute.es)
# des() function to compute ci around es

test <- des(d = 2.0272000, 
            n.1 = 6, 
            n.2 = 6,
            level = 80)

str(test)

test$l.d





