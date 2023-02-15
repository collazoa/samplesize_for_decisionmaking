# Analysis sample size approach D 

mean_success <- 
  res_summary_d %>%
  group_by(scenario)%>%
  summarize(mean_success_d = mean(pct_success, na.rm = T), 
            quan_25 = quantile(pct_success, prob = 0.25, na.rm = T), 
            quan_75 = quantile(pct_success, prob = 0.75, na.rm = T)) %>%
  mutate(mean_success_orig = round(sum(df_combined$orig_p_2sided < 0.05)/86 *100,2)) 

conducted <- 
  res_summary_d %>%
  group_by(conducted)%>%
  summarize(n = n()/3, 
            pct = n/86)


ratio_animals <- 
  res_summary_d %>%
  filter(conducted == "yes") %>%
  mutate(ss_ratio = rep_sample_size_d/orig_ss)%>%
  summarize(mean_ratio  = mean(ss_ratio)) 

abs_animals <-
  res_summary_d %>%
  summarize(sum_orig_ss = sum(orig_ss), 
            sum_rep_ss = sum(rep_sample_size_d, na.rm = T), 
            sum_total = sum_orig_ss + sum_rep_ss) 



abs_animals <- 
  res_summary_d %>%
  summarize(mean_orig_ss = mean(orig_ss), 
            )


p1 <- 
    ggplot(res_summary_d %>% filter(conducted == "yes")) + 
    geom_histogram(aes(x = rep_sample_size_d))+ 
    facet_wrap(~scenario)




