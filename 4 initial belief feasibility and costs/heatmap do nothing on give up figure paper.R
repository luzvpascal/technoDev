results <- read.csv(heatmap_maxTime_file_varying_transitions, sep=" ",
                    header=FALSE)
names(results) <- c("tr_low_low",
                    "tr_high_low",
                    "max_invest",
                    "coeffDev_index",
                    "coeffDeploy_index")


## map #####
breaks_countour <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(results,
       aes(x = tr_low_low, y = tr_high_low)) +
  # geom_tile() +
  geom_raster(aes(fill=max_invest),interpolate = TRUE) +
  scale_fill_gradient(low = "palegoldenrod", high = "darkgreen") +
  labs(x = "Difficulty of recovery\n Pr(unhealthy| unhealthy, BAU)",
       y = "Degradation rate\n Pr(unhealthy| healthy, BAU)",
       fill = TeX("Time limit\nfor technology\ndevelopment$T_{max}$")) +
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
                          size = 9,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.75))+
  theme_minimal() +
  coord_equal()+
  theme(text = element_text(size = 16),
        plot.margin = unit(c(0, 0, 0.02, 0.02), "npc"))
print(max_time_heatMap)
ggsave(heatmap_maxTime_figure_varying_transitions,
       plot = max_time_heatMap, width = 10, height = 6, units = "in")

##modify map####
svglite(heatmap_maxTime_figure_varying_transitions)
max_time_heatMap
grid.polygon(x=c(0.2,  0.8, 0.8,   0.85,  0.8, 0.8, 0.2),
             y=c(0.05, 0.05,   0, 0.1, 0.2, 0.15, 0.15)/8,
             id=NULL, id.lengths=NULL,
             default.units="npc", name=NULL,
             gp=gpar(
               fill = linearGradient(
                 c("white", "red"),
                 stops = c(0, 1)
               ),
               col = NA
             ), draw=TRUE, vp=NULL)

grid.polygon(y=c(0.2,     0.9, 0.9, 0.95,  0.9, 0.9, 0.2),
             x=c(0.05, 0.05,   0, 0.1, 0.2, 0.15, 0.15)/8,
             id=NULL, id.lengths=NULL,
             default.units="npc", name=NULL,
             gp=gpar(
               fill = linearGradient(
                 c("white", "red"),
                 stops = c(0, 1)
               ),
               col = NA
             ), draw=TRUE, vp=NULL)
dev.off()
