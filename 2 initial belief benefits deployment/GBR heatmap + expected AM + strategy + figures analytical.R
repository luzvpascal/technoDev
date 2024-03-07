## TIME LIMIT TECH DEV ####
res <- data.frame()
#expected value if do nothing
statesNames <- c("unhealthy", "healthy")
markovB <- new("markovchain", states = statesNames,
               transitionMatrix=tr_nothing,
               name = "A markovchain Object"
)
steady_states <- steadyStates(markovB)[1,]

## value No RD ####
value_noRD <- c(steady_states%*%c(V_min, V_max))
value_noRD_infty <- value_noRD/(1-gamma)

## solve adaptive management deployment#### #already solved?
tr_low_low <- tr_nothing[1,1]
tr_high_low <- tr_nothing[2,1]
## solve adaptive management deployment####
write_alpha_vectors_analytical(tr_low_low,tr_high_low,costImp_P1)
alphas_AM <- read_policyx2(file_outpolicy_AM_analytical(costImp_P1, tr_low_low,tr_high_low))

## influence of initial belief ####
for (belief_benef_low in coeffs_initial_belief_benefits_low){
  for (belief_benef_high in coeffs_initial_belief_benefits_high){

    ## Estimating reward value AM
    value_AM_infty <- c(analytical_Wready(tr_low_low,tr_high_low,
                                          costImp_P1,
                                          belief_benef_low,
                                          belief_benef_high)[[1]])
    ####################
    ##belief switch ####
    ####################
    belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1,
                                            p_idle = prob_ready,
                                            Rnord = value_noRD,
                                            Wready = value_AM_infty,
                                            disc = gamma)

    Tmax_years <- max_years(initial_belief_state_P1[1],
                            prob_ready,
                            belief_IS)

    res <- rbind(res,
                 data.frame(
                   belief_benef_low=belief_benef_low,
                   belief_benef_high=belief_benef_high,
                   value_AM_infty=value_AM_infty,
                   max_invest=Tmax_years
                 ))
  }
}

write.csv(res, results_varying_priors_example_file_analytical)

## FIGURES ####

## map TMAX#####
res <- read.csv(results_varying_priors_example_file_analytical)

breaks_countour_Tmax <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  geom_raster(aes(fill=max_invest),interpolate = TRUE) +
  scale_fill_gradient(low = "mistyrose", high = "purple4",
                      breaks = c(0,20,40),
                      labels = c(0,20,40)) +
  labs(x = TeX("$\\beta^{res}_0$"),
       y = TeX("$\\beta^{prev}_0$"),
       fill = TeX("$T_{max}$"),
       title="A") +
  geom_contour(aes(z = max_invest),
               binwidth = 10,
               breaks = breaks_countour_Tmax,
               colour = "black") +
  metR::geom_text_contour(aes(z = max_invest),
                          breaks = breaks_countour_Tmax[-1],
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 15,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))+
  coord_equal()+
  theme_bw()+
  theme(
    title=element_text(size=45),
    panel.grid = element_blank(),
    axis.text=element_text(size=45),
    axis.title = element_text(size=45),
    legend.text=element_text(size=45),
    legend.title=element_text(size=45),
    legend.position=c(0.8,0.25),
    legend.key.height= unit(1, 'cm'),
    legend.key.width= unit(2, 'cm'),
    legend.margin=margin(t = 0.5, r = 0.5, b = 0.7, l = 0.5, unit = "cm")
  ) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))
## map EXPECTED VALUE #####
breaks_countour <- c(0.01,1, 3,10,20)
value_AM_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  # geom_tile() +
  geom_raster(aes(fill=(value_AM_infty-value_noRD_infty)/value_noRD_infty*100),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "black",
                      breaks = c(0, 10,20),
                      labels = c(0,10,20)) +
  labs(x = TeX("$\\beta^{res}_0$"),
       y = TeX("$\\beta^{prev}_0$"),
       fill = TeX("$\\frac{R_{AM}-R_{BAU}}{R_{BAU}}$(%)"),
       title="B"
  ) +
  geom_contour(aes(z = (value_AM_infty-value_noRD_infty)/value_noRD_infty*100),
               binwidth = 0.05,
               breaks = breaks_countour,
               colour = "black") +
  metR::geom_text_contour(aes(z = (value_AM_infty-value_noRD_infty)/value_noRD_infty*100),
                          breaks = breaks_countour[-1],
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 15,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))+
  coord_equal()+
  theme_bw()+
  theme(
    title=element_text(size=45),
    panel.grid = element_blank(),
    axis.text=element_text(size=45),
    axis.title = element_text(size=45),
    legend.text=element_text(size=45),
    legend.title=element_text(size=45),
    legend.margin=margin(t = 0.5, r = 0.5, b = 0.7, l = 1, unit = "cm"),
    legend.key.height= unit(1, 'cm'),
    legend.key.width= unit(2, 'cm')
  ) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))

## AM strategy ####
## Adaptive management strategy ####


## sampling benef beliefs
#adapt sampled tests
data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy,
                alpha = alphas_AM)

combination_beliefs_index <- combination_beliefs
combination_beliefs_index$policy <- result

result_policy <- combination_beliefs_index %>%
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
    title=element_text(size=45),
    panel.grid = element_blank(),
    axis.text=element_text(size=45),
    axis.title = element_text(size=45),
    legend.text=element_text(size=45),
    legend.title=element_text(size=45),
    legend.position= "right",
    legend.spacing.y=unit(0.5, 'cm'),
    legend.margin=margin(t = 0.5, r = 0.5, b = 0.7, l = 1, unit = "cm"),
    # legend.box.background = element_rect(fill='transparent'),
    legend.background = element_rect(fill='transparent')
  ) +
  guides(fill = guide_legend(byrow = TRUE))+
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))+
  labs(x = TeX("$\\beta^{res}$"),
       y = TeX("$\\beta^{prev}$"),
       fill = "Adaptive\ndeployment",
       title = "C"
  )

## GENERAL FIGURE ####

FIG <- ggarrange(max_time_heatMap,
                 ggarrange(value_AM_heatMap,
                           AM_strategy_plot,
                           nrow=2,
                           align="hv"),
                 nrow=1
)

ggsave(results_varying_priors_example_figure_analytical,
       plot = FIG, width =25, height = 13, units = "in")
