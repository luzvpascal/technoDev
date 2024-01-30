results <- read.csv(heatmap_valueNoRD_valueRD_varying_transitions_file_analytical,
                    header = FALSE)
names(results) <- c("tr_low_low","tr_high_low","value_RD","value_noRD",
                    "coeffDev_index","coeffDeploy_index")
results <- mutate(results,
                  ratio = round(((value_RD-value_noRD)/value_noRD)*100, digits = 5))%>%
  arrange(tr_low_low, tr_high_low)

## tile heatmap plot ####
breaks_countour <- c(0.001,seq(2,16,2))
heatmap_plot <- ggplot(results, aes(tr_low_low, tr_high_low)) +
  geom_raster(aes(fill=ratio),interpolate = TRUE) +
  scale_fill_gradient(low = "mistyrose", high = "blue4") +  # Define the color scale
  geom_contour(aes(z = ratio),
               # binwidth = 2,
               breaks = breaks_countour,
               colour = "black") +
  metR::geom_text_contour(aes(z = ratio),
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 9,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.75))+
  labs(x = ("Difficulty of recovery\n Pr(unhealthy| unhealthy, BAU)"),
       y = ("Degradation rate\n Pr(unhealthy| healthy, BAU)"),
       fill = ("Expected\nrelative(%)\nbenefits\nof R&D")) +
  theme_minimal() +
  coord_equal()+
  theme(text = element_text(size = 16),
        plot.margin = unit(c(0, 0, 0.02, 0.02), "npc"))
print(heatmap_plot)
ggsave(heatmap_valueNoRD_valueRD_varying_transitions_figure_analytical,
       plot = heatmap_plot, width = 12, height = 6, units = "in")##modify map####
svglite(heatmap_valueNoRD_valueRD_varying_transitions_figure_analytical)
heatmap_plot
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


# ## plot data contours ####
# data_contours <- read.csv(contours_file,
#                            header = FALSE)
# names(data_contours) <- c("tr_low_low","tr_high_low","cost")
#
# data_contours <- rbind(data_contours,
#                         data.frame(tr_low_low=c(0,1,1),
#                                    tr_high_low=c(1,1,0),
#                                    cost=0.1))
#
# data_contours$cost <- factor(data_contours$cost,
#                              levels=c('1e-04', '0.001', '0.01', '0.1'))
# library(ggplot2)
#
# contours_plot <-
#   ggplot(data_contours,
#        aes(x=tr_low_low,
#            y=tr_high_low,
#            group=cost))+
#   geom_line(aes(color=cost), size=2)+
#   scale_color_manual(values=c('dodgerblue1',
#                               'dodgerblue3',
#                               "dodgerblue4",
#                               "darkblue"))+
#   labs(x = ("Difficulty of recovery\n Pr(unhealthy| unhealthy, BAU)"),
#        y = ("Degradation rate\n Pr(unhealthy| healthy, BAU)"),
#        col = ("Relative cost\nof development"))+
#   coord_equal()+
#   theme_minimal()+
#   theme(text = element_text(size = 16),
#         plot.margin = unit(c(0, 0, 0.02, 0.02), "npc"))
# contours_plot
# ggsave(contours_figure,
#        plot = contours_plot, width = 12, height = 6, units = "in")
# ##modify map####
# svglite(contours_figure)
# contours_plot
# grid.polygon(x=c(0.2,  0.8, 0.8,   0.85,  0.8, 0.8, 0.2),
#              y=c(0.05, 0.05,   0, 0.1, 0.2, 0.15, 0.15)/8,
#              id=NULL, id.lengths=NULL,
#              default.units="npc", name=NULL,
#              gp=gpar(
#                fill = linearGradient(
#                  c("white", "red"),
#                  stops = c(0, 1)
#                ),
#                col = NA
#              ), draw=TRUE, vp=NULL)
#
# grid.polygon(y=c(0.2,     0.9, 0.9, 0.95,  0.9, 0.9, 0.2),
#              x=c(0.05, 0.05,   0, 0.1, 0.2, 0.15, 0.15)/8,
#              id=NULL, id.lengths=NULL,
#              default.units="npc", name=NULL,
#              gp=gpar(
#                fill = linearGradient(
#                  c("white", "red"),
#                  stops = c(0, 1)
#                ),
#                col = NA
#              ), draw=TRUE, vp=NULL)
# dev.off()
