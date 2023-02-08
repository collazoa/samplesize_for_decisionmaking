setwd("~/Desktop/samplesize_for_decisionmaking")

load("./data/res_summary_a.RData")
load("./data/res_summary_b.RData")

test <-
  bind_rows(res_summary_a,
            res_summary_b)


p.1 <- 
  ggplot(data = test,
         aes(x = sample_size_approach,
             y = pct_success)) +
  facet_wrap(~ scenario) +
  geom_boxplot() +
  theme_bw()

plot(p.1)

min(test$rep_sample_size)

test$conducted <-
  ifelse(test$rep_sample_size < 50, "yes", "no")

p.2 <- 
  ggplot(data = test,
         aes(fill = conducted,
             x = sample_size_approach)) +
  geom_bar(position = "stack") +
  coord_flip() +
  facet_wrap(~ scenario) +
  theme_bw()

plot(p.2)


