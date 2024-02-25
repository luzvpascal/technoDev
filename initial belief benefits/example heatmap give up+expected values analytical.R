## TIME LIMIT TECH DEV ####
res <- data.frame()
#expected value if do nothing
tr_nothing_index <- transition_from_parameters(c(tr_low_low,
                                                 tr_high_low),
                                               N_actions=1)[,,1]
statesNames <- c("unhealthy", "healthy")
markovB <- new("markovchain", states = statesNames,
               transitionMatrix=tr_nothing_index,
               name = "A markovchain Object"
)
steady_states <- steadyStates(markovB)[1,]

## value No RD ####
value_noRD <- c(steady_states%*%c(V_min, V_max))
value_noRD_infty <- value_noRD/(1-gamma)

## solve adaptive management deployment#### #already solved?
data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
## influence of initial belief ####
for (belief_benef_low in coeffs_initial_belief_benefits_low){
  for (belief_benef_high in coeffs_initial_belief_benefits_high){

    ## Estimating reward value AM
    initial_belief_AM <- c((1-belief_benef_low)*(1-belief_benef_high), #BAU is better
                           (1-belief_benef_low)*(belief_benef_high), #BAU is better if low, deployment is better if high
                           belief_benef_low*(1-belief_benef_high), #deployment is better if low, BAU is better if high
                           belief_benef_low*belief_benef_high) #deployment is better

    initial_belief_AM <- initial_belief_AM[data_mean_parameters$opt]
    initial_belief_AM <- initial_belief_AM/sum(initial_belief_AM)

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

## map EXPECTED VALUE #####

breaks_countour <- c(0.001,seq(2,50,2))
value_AM_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  # geom_tile() +
  geom_raster(aes(fill=(value_AM_infty-value_noRD_infty)/value_noRD_infty*100),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "Initial belief technology deployment\nis beneficial when unhealthy",
       y = "Initial belief technology deployment\nis beneficial when healthy",
       fill = TeX("$\\frac{R_{AM}-R_{BAU}}{R_{BAU}}$")) +
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
                          size = 6,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))+
  coord_equal()+
  theme(text = element_text(size = 12),
        axis.text = element_text(color="black"))+
  theme_bw()
print(value_AM_heatMap)

ggsave(expected_value_varying_priors_example_figure_analytical,
       plot = value_AM_heatMap, width = 10, height = 6, units = "in")

## map TMAX#####

breaks_countour <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  # geom_tile() +
  geom_raster(aes(fill=max_invest),interpolate = TRUE) +
  scale_fill_gradient(low = "mistyrose", high = "purple4") +
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
  theme(text = element_text(size = 12),
        axis.text = element_text(color="black"))+
  theme_bw()
print(max_time_heatMap)

ggsave(heatmap_maxTime_varying_priors_example_figure_analytical,
       plot = max_time_heatMap, width = 10, height = 6, units = "in")

