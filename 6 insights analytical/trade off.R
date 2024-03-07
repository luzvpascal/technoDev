res <- data.frame()

for (value_noRD_index in seq(0,1,length.out=101)){
  for (value_AM_index in seq(0,1,length.out=101)){
    belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1,
    # belief_IS <- belief_invest_to_surrender(cost_dev =0.01,
                                            p_idle = prob_ready,
                                            Rnord = value_noRD_index,
                                            Wready = value_AM_index/(1-gamma),
                                            disc = gamma)

    if (belief_IS >0){
      Tmax_years <- max_years(initial_belief_state_P1[1],
                              prob_ready,
                              belief_IS)

      res <- rbind(res,
                   data.frame(value_AM=value_AM_index,
                              value_noRD=value_noRD_index,
                              belief_IS=belief_IS,
                              Tmax_years=Tmax_years))
    } else {
      res <- rbind(res,
                   data.frame(value_AM=value_AM_index,
                              value_noRD=value_noRD_index,
                              belief_IS=belief_IS,
                              Tmax_years=0))
    }

  }
}

breaks_countour <- c(0.001, 20, 30, 50, 55, 58)

res %>%
  ggplot(aes(x=value_noRD, y=value_AM))+
  geom_raster(aes(fill=Tmax_years), interpolate = TRUE)+
  scale_fill_gradient(low = "white", high = "blue")+
  geom_contour(aes(z = Tmax_years),
               binwidth = 0.05,
               breaks = breaks_countour,
               colour = "black") +
  # metR::geom_text_contour(aes(z = Tmax_years),
  #                         breaks = breaks_countour[-1],
  #                         min.size = 0,
  #                         skip=0,
  #                         stroke = 0.1,
  #                         rotate = FALSE,
  #                         size = 6,
  #                         fontface = "bold",
  #                         label.placer = metR::label_placer_fraction(frac = 0.6))+
  coord_equal()+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text=element_text(size=13),
    axis.title = element_text(size=16),
    legend.text=element_text(size=13),
    legend.title=element_text(size=14),
    legend.position="right"
  ) +
  geom_abline(intercept = 0, slope = 1, col="red")

res %>%
  # filter(value_AM-value_noRD>=0)%>%
  ggplot()+
  geom_point(aes(x=(costDev_P1-value_noRD+value_AM), y=Tmax_years))+
  labs(x=TeX("$R_{AM}-(R_{BAU}-C_{dev})"),
       y=TeX("$T_{max}$"))+
  theme_bw()
