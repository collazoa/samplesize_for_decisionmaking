setwd("~/Desktop/samplesize_for_decisionmaking")

load("./data/res_summary_a.RData")
load("./data/res_summary_b.RData")
load("./data/res_summary_c.RData")
load("./data/res_summary_d.RData")

# if all simulation scripts are run again the outcommented code below should be included already 
# and does not need to be run again and can be deleted once checked

# res_summary_a$conducted <- 
#   ifelse(is.na(res_summary_a$rep_sample_size) | res_summary_a$rep_sample_size >= 280, "unfeasible", 
#          ifelse(res_summary_a$rep_sample_size < 4, "not_necessary", "yes"))
# 
# 
# res_summary_c$scenario[1:86] <- "m_error"
# res_summary_c$scenario[87:172] <- "null_effect"
# res_summary_c$scenario[173:258] <- "s_error"
# 
# names(res_summary_d)[6] <- "rep_sample_size"
# names(res_summary_d)[7] <- "es_true"
# 
# res_summary_c$sample_size_approach <- "c"
# res_summary_d$sample_size_approach <- "d"

dat <-
  bind_rows(res_summary_a,
            res_summary_b,
            res_summary_c,
            res_summary_d)

dat_sum <-
  dat %>% 
  filter(scenario == "m_error") %>% 
  group_by(sample_size_approach, conducted) %>% 
  summarize(N = n(),
            pct = N / 86 * 100)

p.1 <- 
  ggplot(data = dat_sum,
         aes(fill = conducted,
             x = sample_size_approach,
             y = pct)) +
  geom_bar(stat = "identity",
           position = "stack") +
  coord_flip() +
  labs(x = "Sample size approach",
       y = "Percentage",
       fill = "Replication conducted") +
  scale_x_discrete(limits = c("a_80pct",
                              "a_95pct",
                              "b_0.5",
                              "b_1.0",
                              "c",
                              "d"),
                   labels = c("Significance \n80 % power",
                              "Significance \n95 % power",
                              "SESOI = 0.5",
                              "SESOI = 1.0",
                              "Safeguard",
                              "Skeptical \np-value")) +
  scale_fill_manual(limits = c("yes",
                               "unfeasible",
                               "not_neccessary"), # script contained typo, now corrected, should be changed here, once simulation is run again
                    labels = c("yes",
                               "no - unfeasible",
                               "no - not necessary"),
                    values = c("#009E73", "#CC79A7", "#56B4E9")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 9, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
        
plot(p.1)

summary(dat$rep_sample_size)
vec <- c(4, 6, 14, 46, 250)

dat_p4 <-
  dat %>% 
  filter(scenario == "m_error") %>%
  filter(conducted == "yes") %>% 
  mutate(bins = findInterval(rep_sample_size, vec))

facet_names <- 
  c("a_80pct" = "Significance \n80 % power", 
    "a_95pct" = "Significance \n95 % power",
    "b_0.5" = "SESOI = 0.5",
    "b_1.0" = "SESOI = 1.0", 
    "c" = "Safeguard", 
    "d" = "Skeptical \np-value")

p.4 <-
  ggplot(data = dat_p4,
         aes(x = factor(bins),
             y = pct_success,
             fill = factor(bins))) +
  geom_boxplot(size = 0.3) +
  # geom_beeswarm(alpha = 0.5, size = 0.7) +
  facet_wrap(~ sample_size_approach,
             labeller = labeller(.cols = facet_names),
             ncol = 2) +
  # facet_grid(scenario ~ sample_size_approach) +
  labs(x = "Sample size",
       y = "Percentage success",
       fill = "Replication \nsample size") +
  scale_fill_manual(limits = c("1", "2", "3", "4", "5"),
                    labels = c("4-6", "7-14", "15-46",
                               "46-250", "> 250"),
                    values = c("#009E73", "#CC79A7", "#56B4E9",
                               "#E69F00", "#0072B2")) +
  # scale_x_discrete(limits = c("1", "2", "3", "4", "5"),
  #                  labels = c("4-6", "7-14", "15-46",
  #                             "46-250", "> 250")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11),
        # axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"),
        strip.text.x = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(size = 0.25),
        panel.grid.minor = element_line(size = 0.1),
        strip.background = element_rect(fill = "white", color = "black", size = 0.3))

plot(p.4)


dat_hist <-
  dat %>% 
  filter(conducted == "yes") %>% 
  filter(rep_sample_size < 150)

p.5 <-
  ggplot(data = dat_hist,
         aes(x = rep_sample_size)) +
  geom_histogram(bins = 50, size = 0.3,
                 color = "black", fill = "white") +
  # geom_abline() + 
  facet_wrap(~ sample_size_approach) +
  theme_bw()

plot(p.5)



