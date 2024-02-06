result_policy <- read.csv(AM_strategy_varying_priors_file,
                   header = FALSE)

names(result_policy) <- c("belief_benef_low",
                   "belief_benef_high",
                   "policy",
                   "profile")

result_policy <- result_policy %>%
  mutate(policy = case_when(
    policy == "1-1" ~ "Never deploy",
    policy == "1-2" ~ "Deploy healthy",
    policy == "2-1" ~ "Deploy unhealthy",
    policy == "2-2" ~ "Always deploy"
  ))

AM_strategy_plot <- result_policy %>%
  mutate(policy = factor(policy, levels=c("Never deploy",
                                          "Deploy healthy",
                                          "Deploy unhealthy",
                                          "Always deploy")))%>%
  ggplot()+
  geom_raster( aes(x=belief_benef_low, y=belief_benef_high, fill=policy))+
  scale_fill_manual(values = c("red",
                               "darkblue",
                               "orange",
                               "darkgreen"))+
  coord_equal()+
  theme(
    panel.grid = element_blank()
  ) +
  theme_bw()+
  facet_wrap(~profile, nrow=1)+
  labs(x = "Initial belief technology deployment\nis beneficial when unhealthy",
       y = "Initial belief technology deployment\nis beneficial when healthy",
       fill = "Deployment strategy"
  )
print(AM_strategy_plot)
ggsave(AM_strategy_varying_priors_figure,
       plot = AM_strategy_plot, width = 10, height = 6, units = "in")
