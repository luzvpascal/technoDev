results <- read.csv(heatmap_maxTime_file_varying_priors, sep=",",
                    header=FALSE)
names(results) <- c("belief_benef_low",
                    "belief_benef_high",
                    "max_invest",
                    "coeffDev_index",
                    "coeffDeploy_index",
                    "profile")

results$profile <- gsub("-","\n",as.character(results$profile))
## map #####
breaks_countour <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(results,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  # geom_tile() +
  geom_raster(aes(fill=max_invest),interpolate = TRUE) +
  scale_fill_gradient(low = "mistyrose", high = "purple4") +
  labs(x = TeX("$\\beta^{res}_0$"),
       y = TeX("$\\beta^{prev}_0$"),
       fill = TeX("$T_{max}$")) +
  geom_contour(aes(z = max_invest),
               binwidth = 10,
               breaks = breaks_countour,
               colour = "black") +
  metR::geom_text_contour(aes(z = max_invest),
                          breaks = breaks_countour[-1],
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 6,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))+
  # theme_minimal() +
  coord_equal()+
  facet_wrap(~profile,nrow=1)+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text=element_text(size=12),
    axis.title = element_text(size=16),
    legend.text=element_text(size=13),
    legend.title=element_text(size=14),
    strip.text = element_text(size=14),
    panel.spacing = unit(2, "lines"),
    legend.position="bottom"
    )
print(max_time_heatMap)

# AM strategy figure ####
result_policy <- read.csv(AM_strategy_varying_priors_file,
                          header = FALSE)

names(result_policy) <- c("belief_benef_low",
                          "belief_benef_high",
                          "policy",
                          "profile")

result_policy <- result_policy %>%
  mutate(policy = case_when(
    policy == "1-1" ~ "Do not deploy",
    policy == "1-2" ~ "Deploy healthy",
    policy == "2-1" ~ "Deploy unhealthy",
    policy == "2-2" ~ "Deploy"
  ))

AM_strategy_plot <- result_policy %>%
  mutate(policy = factor(policy, levels=c("Do not deploy",
                                          "Deploy healthy",
                                          "Deploy unhealthy",
                                          "Deploy")))%>%
  ggplot()+
  geom_raster( aes(x=belief_benef_low, y=belief_benef_high, fill=policy))+
  scale_fill_manual(values = c("red",
                               "dodgerblue3",
                               "goldenrod1",
                               "darkgreen"))+
  coord_equal()+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text=element_text(size=12),
    axis.title = element_text(size=16),
    legend.text=element_text(size=12),
    legend.title=element_text(size=14),
    strip.text = element_text(size=14),
    panel.spacing = unit(2, "lines")
  ) +
  facet_wrap(~profile,nrow=1)+
  labs(x = TeX("$\\beta^{res}$"),
       y = TeX("$\\beta^{prev}$"),
       fill = "Adaptive deployment strategy"
  )
print(AM_strategy_plot)

## joining the 2 figures ####
fig <- ggarrange(max_time_heatMap,
                AM_strategy_plot,
                nrow=2,
                align = "hv",
                labels = c("(I)","(II)"))
fig
ggsave(heatmap_maxTime_figure_varying_priors,
       plot = fig, width = 15, height = 6, units = "in")


#################### overlap figure ####
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
  )) %>%
  mutate(policy = factor(policy, levels=c("Never deploy",
                                          "Deploy healthy",
                                          "Deploy unhealthy",
                                          "Always deploy")))

overlap <- max_time_heatMap +
  new_scale_fill()+
  geom_raster(data=result_policy,
              aes(x=belief_benef_low, y=belief_benef_high, fill=policy,
                  alpha=0.2))+
  scale_alpha(guide="none")+
  scale_fill_manual(values = c("red",
                               "darkblue",
                               "orange",
                               "darkgreen"))+
  labs(fill = "Deployment strategy"  )+
  geom_contour(aes(z = max_invest),
               binwidth = 10,
               breaks = breaks_countour,
               colour = "black") +
  metR::geom_text_contour(aes(z = max_invest),
                          breaks = breaks_countour[-1],
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 6,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))

print(overlap)
ggsave(heatmap_maxTime_overlap_AM_strategy_figure_varying_priors,
       plot = overlap, width = 10, height = 6, units = "in")
