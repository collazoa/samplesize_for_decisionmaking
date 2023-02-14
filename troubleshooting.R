

power.t.test(delta = 0.3, sd = 1, sig.level = 0.05, power = 0.8,
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



2.0272 - (1.25 * 2.0272)

.25 * 2.0272

sum(res_summary_rep_a_80$effect < res_summary_rep_a_80$ci_high)

sum(res_summary_rep_a_80$effect > res_summary_rep_a_80$ci_low)

check <- which(res_summary_rep_a_80$effect < res_summary_rep_a_80$ci_low)


sum(res_summary_rep_a_95$effect < res_summary_rep_a_95$ci_high)

sum(res_summary_rep_a_95$effect > res_summary_rep_a_95$ci_low)

check <- which(res_summary_rep_a_95$effect < res_summary_rep_a_95$ci_low)

check <-
  res_summary_rep_a_80 %>% 
  filter(effect < ci_low)


check <-
  res_summary_rep_a_95 %>% 
  filter(effect < ci_low)


sum(res_summary_rep_b_0.5$effect < res_summary_rep_b_0.5$ci_high)

sum(res_summary_rep_b_0.5$effect > res_summary_rep_b_0.5$ci_low)

check <-
  res_summary_rep_b_0.5 %>% 
  filter(effect < ci_low)


sum(res_summary_rep_b_1.0$effect < res_summary_rep_b_1.0$ci_high)

sum(res_summary_rep_b_1.0$effect > res_summary_rep_b_1.0$ci_low)

check <-
  res_summary_rep_b_1.0 %>% 
  filter(effect < ci_low)


sum(res_summary_rep_c$effect < res_summary_rep_c$ci_high)

sum(res_summary_rep_c$effect > res_summary_rep_c$ci_low)

check <-
  res_summary_rep_c %>% 
  filter(effect < ci_low)


test_data <- list_rep_data[[12]][[2]]

t <- t.test(test_data$values ~ test_data$intervention,
            alternative = "greater",
            var.equal = FALSE,
            conf.level = .95)

str(t)

ci <- t$conf.int

study_summary <-
  test_data %>%
  group_by(study_id, intervention) %>%
  summarize(mean_group = mean(values),
            sd_group = sd(values)) %>%
  mutate(t_value = round(t$statistic, 3),
         p_value = round(t$p.value, 3),
         ci_low = round(ci[1], 3),
         ci_high = round(ci[2], 3))
         # effect = round(t$statistic/sqrt(nrow(test_data)), 3))


effect <- 
  (study_summary$mean_group[1] - study_summary$mean_group[2]) /
  sqrt((study_summary$sd_group[1]^2 + study_summary$sd_group[2]^2)/2)


effect2 <- 2 * t$statistic/sqrt(4)

effect3 <- t$statistic * sqrt((2+2) / (2*2))



study_summary2 <-
  study_summary %>%
  group_by(study_id, t_value, p_value, ci_low, ci_high) %>%
  summarize(effect = mean(effect))


effect = round(t$statistic/sqrt(nrow(test_data)), 3)

ci_low = round(t$conf.int[1], 3)

ci_high = round(t$conf.int[2], 3)

ci <- t$conf.int

str(ci)

ci[1]
ci[2]
