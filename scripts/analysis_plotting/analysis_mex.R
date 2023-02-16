ggplot(res_summary_d %>% filter(conducted == "yes")) +
  geom_histogram(aes(x = rep_sample_size_d))+
  facet_wrap(~scenario) 


# facet_wrap(~sample_size_approach)