library(kableExtra)
library(tidyverse)
library(gridExtra)


# Analysis sample size approach D 


res_summary_c$scenario[173:258] <- "s_error"
res_summary_c$scenario[1:86] <- "m_error"
res_summary_c$scenario[87:172]<- "null_effect"
res_summary_c$sample_size_approach <- "c"
res_summary_d$sample_size_approach <- "d"
colnames(res_summary_d)[6] <- "rep_sample_size"
colnames(res_summary_d)[7] <- "es_true"
res_summary_a$conducted <- "yes"

res_summary <- 
  rbind(res_summary_a, res_summary_b, res_summary_c, res_summary_d)


##############################
# descriptive data 
# data check 
#############################

t1 <- 
  res_summary %>%
  group_by(sample_size_approach, scenario)%>%
  summarize( n = n())

kable(t1)%>%kable_classic()


t2 <- 
  res_summary%>%
  filter(scenario == "m_error")%>%
  filter(conducted == "yes") %>%
  group_by(sample_size_approach)%>%
  summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)), 
            mean_pct_success = round(mean(pct_success)), 
            sum_rep_ss = sum(rep_sample_size))



t2 <- 
  res_summary%>%
  filter(conducted == "yes") %>%
  group_by(sample_size_approach, scenario)%>%
  summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)), 
            mean_pct_success = round(mean(pct_success)), 
            sum_rep_ss = sum(rep_sample_size))%>%
  pivot_wider(names_from = scenario, values_from = mean_pct_success)

colnames(t2) <- c("sample_size_approach", 
                  "mean_rep_ss", 
                  "sum_rep_ss", 
                  "success_rate_m_error", 
                  "success_rate_null_effect", 
                  "success_rate_s_error")

t2


kable(t2) %>% 
  kable_classic()


###############
# Fig 4 
#
###############

t2a <- 
  res_summary%>%
  filter(conducted == "yes") %>%
  group_by(sample_size_approach, scenario)%>%
  summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)), 
            mean_pct_success = round(mean(pct_success)), 
            sum_rep_ss = sum(rep_sample_size))


p4<- 
  grid.arrange(

ggplot(t2a %>% filter(scenario == "m_error")) +
  geom_point(aes(x = sum_rep_ss, 
                 y = mean_pct_success, 
                 color = sample_size_approach), 
             size = 3)+ 
  scale_color_manual(name = " ", 
                    labels = c("standard significance, 80% power", 
                               "standard significance, 95% power", 
                               "smallest effect size of interest, d = 0.5", 
                               "smallest effect size of interest, d = 1", 
                               "safeguard power analysis, 60% lower CI bound", 
                               "p-skeptical"), 
                    values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  labs(y = "mean replication success rate", 
       x = " sum of replication sample sizes"
  )+
  theme(legend.position = "none"),

ggplot(t2a %>% filter(scenario == "null_effect")) +
  geom_point(aes(x = sum_rep_ss, 
                 y = mean_pct_success, 
                 color = sample_size_approach), 
             size = 3)+
  scale_color_manual(name = " ", 
                     labels = c("standard significance, 80% power", 
                                "standard significance, 95% power", 
                                "smallest effect size of interest, d = 0.5", 
                                "smallest effect size of interest, d = 1", 
                                "safeguard power analysis, 60% lower CI bound", 
                                "p-skeptical"), 
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  labs(y = "", 
       x = " sum of replication sample sizes")+
  theme(legend.position = "none"), 

ggplot(t2a %>% filter(scenario == "s_error")) +
  geom_point(aes(x = sum_rep_ss, 
                 y = mean_pct_success, 
                 color = sample_size_approach), 
             size = 3)+
  scale_color_manual(name = " ", 
                     labels = c("standard significance, 80% power", 
                                "standard significance, 95% power", 
                                "smallest effect size of interest, d = 0.5", 
                                "smallest effect size of interest, d = 1", 
                                "safeguard power analysis, 60% lower CI bound", 
                                "p-skeptical"), 
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  labs(y = "", 
       x = " sum of replication sample sizes")+
  theme(legend.position = "none"), 
ncol = 3
) 
# plot1 with legend
plot1_legend <- ggplot(t2a %>% filter(scenario == "null_effect")) +
  geom_point(aes(x = sum_rep_ss, 
                 y = mean_pct_success, 
                 color = sample_size_approach), 
             size = 3)+
  scale_color_manual(name = " ", 
                     labels = c("standard significance, 80% power", 
                                "standard significance, 95% power", 
                                "smallest effect size of interest, d = 0.5", 
                                "smallest effect size of interest, d = 1", 
                                "safeguard power analysis, 60% lower CI bound", 
                                "p-skeptical"), 
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  theme(legend.position = "bottom")

# function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# extract legend from plot1 using above function
legend <- get_only_legend(plot1_legend) 


p4a <- grid.arrange(p4, legend, heights = c(10, 1))
p4a

################################
# Figure 2 
# --> ratio = rep_ss/orig_ss <- 
# --> for each approach      <- 
###############################

#filtering the conducted replication studies 
d2 <- 
  res_summary %>%
  filter(conducted == "yes")
# 1383 / 1548 

# calculating the ratio of replication sample size vs original sample size 
d2$ratio_ss <-
  d2$rep_sample_size/d2$orig_ss

# data points 

p2a <- ggplot(data = d2, aes(x = sample_size_approach, y = ratio_ss, color = sample_size_approach)) + 
  geom_jitter(size = 2, width = 0.1) +
  coord_trans( y = "log10")+
  scale_y_continuous(breaks=c(0,0.5,1,2,5,10,25))+
  geom_abline(intercept = 1, slope = 0, color = "red", linetype = 2, alpha = 0.5)+
  labs(title = "Comparative methods for sample size calculation in confirmatory studies",
       subtitle = "Range of relative sample sizes for 86 confirmatory preclinical research projects",
       y = expression(paste ("relative sample size = N"[confirmatory],  "/ N"[exploratory])), 
       x = ""
  ) + 
  scale_color_manual(name = " ", 
                     labels = c("standard significance, 80% power", 
                                "standard significance, 95% power", 
                                "smallest effect size of interest, d = 0.5", 
                                "smallest effect size of interest, d = 1", 
                                "safeguard power analysis, 60% lower CI bound", 
                                "p-skeptical"), 
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  theme(axis.ticks.x = element_blank(), 
        legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

p2a

# violin plot 

p2b <- ggplot(data = d2, aes(x = sample_size_approach, y = ratio_ss, fill = sample_size_approach)) + 
  geom_violin() + 
  coord_trans( y = "log10")+
  scale_y_continuous(breaks=c(0,0.5,1,2,5,10,25))+
  geom_abline(intercept = 1, slope = 0, color = "red", linetype = 2, alpha = 0.5)+
  labs(title = "Comparative methods for sample size calculation in confirmatory studies",
       subtitle = "Range of relative sample sizes for 86 confirmatory preclinical research projects",
       y = expression(paste ("relative sample size = N"[confirmatory],  "/ N"[exploratory])), 
       x = ""
  ) + 
  scale_fill_manual(name = " ", 
                     labels = c("standard significance, 80% power", 
                                "standard significance, 95% power", 
                                "smallest effect size of interest, d = 0.5", 
                                "smallest effect size of interest, d = 1", 
                                "safeguard power analysis, 60% lower CI bound", 
                                "p-skeptical"), 
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  theme(axis.ticks.x = element_blank(), 
        legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

p2b


# boxplot 

p2c <- ggplot(data = d2, aes(x = sample_size_approach, y = ratio_ss, fill = sample_size_approach)) + 
  geom_boxplot() + 
  coord_trans( y = "log10")+
  scale_y_continuous(breaks=c(0,0.5,1,2,5,10,25))+
  geom_abline(intercept = 1, slope = 0, color = "red", linetype = 2, alpha = 0.5)+
  labs(title = "Comparative methods for sample size calculation in confirmatory studies",
       subtitle = "Range of relative sample sizes for 86 confirmatory preclinical research projects",
       y = expression(paste ("relative sample size = N"[confirmatory],  "/ N"[exploratory])), 
       x = ""
  ) + 
  scale_fill_manual(name = " ", 
                    labels = c("standard significance, 80% power", 
                               "standard significance, 95% power", 
                               "smallest effect size of interest, d = 0.5", 
                               "smallest effect size of interest, d = 1", 
                               "safeguard power analysis, 60% lower CI bound", 
                               "p-skeptical"), 
                    values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  theme(axis.ticks.x = element_blank(), 
        legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
p2c



##############################
# Figure 3 
# success rates in relation to 
# orig_d, bins for quartiles of 
# distribution of orig_d 
#
##############################

d3 <- 
  res_summary %>%
  mutate(orig_d = rep(df_combined$orig_d, 18))

# creating bins for orig_d 
vec_breaks <- c(min(df_combined$orig_d),
  as.numeric(quantile(df_combined$orig_d, prob = c(.25, .5, .75))), 
  round(max(df_combined$orig_d)))

orig_d_bin <- findInterval(df_combined$orig_d, vec_breaks)

d3 <- 
  d3 %>%
  mutate(orig_d_bin = rep(orig_d_bin, 18))

d3f <- 
  d3 %>%
  filter(conducted == "yes")


d3a <- 
  d3f %>%
  group_by(sample_size_approach, scenario, orig_d_bin) %>%
  summarize(mean_success = round(mean(pct_success),1))



p3a <- 
  ggplot(d3a)+ 
  geom_col(aes(x = orig_d_bin, y = mean_success, fill = factor(orig_d_bin))) + 
  facet_grid(rows = vars(scenario), 
             cols = vars(sample_size_approach))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 1.7", 
                               "1.7 - 2.8", 
                               "2.8 - 6.2", 
                               "6.2 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+ 
  labs(y = "mean replication success rate in synthetic replication studies", 
       x = ""
  ) +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank())

p3a

p3b <- 
  ggplot(d3a %>% filter(scenario == "m_error"))+ 
  geom_col(aes(x = orig_d_bin, y = mean_success, fill = factor(orig_d_bin))) + 
  facet_grid(cols = vars(sample_size_approach))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 1.7", 
                               "1.7 - 2.8", 
                               "2.8 - 6.2", 
                               "6.2 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+ 
  labs(y = "percentage of replication success", 
       x = ""
  ) +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank())

p3b
  
  
ggplot(d3f) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success))+
  facet_grid(rows = vars(scenario), 
             cols = vars(sample_size_approach))

facet_names <- c("a_80pct" = "Significance \n80 % power", 
                 "a_95pct" = "Significance \n95 % power",
                 "b_0.5" = "SESOI = 0.5",
                 "b_1.0" = "SESOI = 1.0", 
                 "c" = "Safeguard",
                 "d" = "Skeptical \np-value")



p3c <- 
  ggplot(d3f %>% filter(scenario == "m_error")) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 1.7", 
                               "1.7 - 2.8", 
                               "2.8 - 6.2", 
                               "6.2 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "Rate of successful replication experiments under M-error scenario", 
       x = "",
       y = "Percentage success")+
  theme(
      legend.position = "bottom", 
      panel.grid.major = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank())

p3c


p3d <- 
  ggplot(d3f %>% filter(scenario == "null_effect")) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 1.7", 
                               "1.7 - 2.8", 
                               "2.8 - 6.2", 
                               "6.2 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "Rate of successful replication experiments under null-effect scenario", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom", 
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank())

p3d

p3e <- 
  ggplot(d3f %>% filter(scenario == "s_error")) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 1.7", 
                               "1.7 - 2.8", 
                               "2.8 - 6.2", 
                               "6.2 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "Rate of successful replication experiments under s-error scenario", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom", 
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank())

p3e



##############################
# Figure 4 
# rep_sample_size ~ orig_d
#
##############################

d4 <- 
  res_summary %>%
  mutate(orig_d = rep(df_combined$orig_d, 18)) %>%
  filter(scenario == "m_error")

plot(d3$rep_sample_size ~ d3$orig_d)

ggplot(d4) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted))+
  facet_grid(rows = vars(sample_size_approach)) +
  scale_y_log10()+
  scale_x_log10()

d4a <- 
  d4 %>%
  filter(sample_size_approach == c("d", "c", "a_80pct", "b_0.5"))


ggplot(d4 %>% filter(sample_size_approach == "d")) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted), size = 3)+
  facet_grid(cols = vars(sample_size_approach))+
  scale_y_log10()+
  scale_x_log10()



ggplot(d4 %>% filter(sample_size_approach == "d") %>% filter(scenario == "m_error")) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = pct_success), size = 3)+
  facet_grid(cols = vars(sample_size_approach))+
  scale_color_continuous(type = "viridis")+ 
  coord_cartesian(xlim = c(0,10), ylim = c(0,100))


ggplot(d4 %>% filter(sample_size_approach == c("d", "c", "a_80pct", "b_0.5"))) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted))+
  facet_grid(cols = vars(sample_size_approach))+
  scale_y_log10()+
  scale_x_log10()


###################
# Fig 5
# Mex 
#####################

vec <- c(4,6,46,250)

d5 <- 
  res_summary %>%
  mutate(orig_d = rep(df_combined$orig_d, 18)) %>%
  filter(conducted == "yes") %>%
  filter(scenario == "m_error") %>%
  mutate(bins = findInterval(rep_sample_size, vec))

facet_names <- c("a_80pct" = "Significance \n80 % power", 
                 "a_95pct" = "Significance \n95 % power",
                 "b_0.5" = "SESOI = 0.5",
                 "b_1.0" = "SESOI = 1.0", 
                 "c" = "Safeguard",
                 "d" = "Skeptical \np-value")


p5 <- 
  ggplot(d5, 
         aes(x = factor(bins), 
             y = pct_success, 
             fill = factor(bins)))+ 
  geom_boxplot(size = 0.3)+
  facet_wrap(~sample_size_approach, 
             labeller = labeller(.cols = facet_names), 
             ncol = 2)+
  scale_fill_manual(limits = c("1", "2", "3", "4", "5"),
                  labels = c("4-6", "7-14", "15-46","46-250", "> 250"),
                  values = c("#009E73", "#CC79A7", "#56B4E9","#E69F00", "#0072B2"))
p5









res_summary <- rbind(res_summary_a, res_summary_b, res_summary_c, res_summary_d)
table(res_summary$sample_size_approach)

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




