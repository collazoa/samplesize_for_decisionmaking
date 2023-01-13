

power.t.test(delta = 0.07179143, sd = 1, sig.level = 0.05, power = 0.5,
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
           CI_lower = round(t$conf.int[1], 3),
           CI_upper = round(t$conf.int[2], 3),
           effect = round(effect, 3))
  
  study_summary <-
    study_summary %>%
    group_by(study_id, t_value, p_value, CI_lower, CI_upper) %>%
    summarize(effect = mean(effect))
  
}
