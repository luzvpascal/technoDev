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
  scale_fill_gradient(low = "palegoldenrod", high = "darkgreen") +
  labs(x = "Initial belief technology deployment\nis beneficial when unhealthy",
       y = "Initial belief technology deployment\nis beneficial when healthy",
       fill = TeX("Time limit\nfor technology\ndevelopment $T_{max}$")) +
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
  facet_wrap(~profile)+
  theme(text = element_text(size = 12),
        axis.text = element_text(color="black"))
print(max_time_heatMap)

ggsave(heatmap_maxTime_figure_varying_priors,
       plot = max_time_heatMap, width = 10, height = 6, units = "in")
