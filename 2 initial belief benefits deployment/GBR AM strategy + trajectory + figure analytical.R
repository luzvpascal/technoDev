tr_low_low <- 0.8
tr_high_low <- 0.8
tr_nothing_index <- transition_from_parameters(c(tr_low_low,
                                                 tr_high_low),
                                               N_actions=1)[,,1]
## solving AM ####
write_alpha_vectors_analytical(tr_low_low,tr_high_low,costImp_P1)
alphas_AM <- read_policyx2(file_outpolicy_AM_analytical(costImp_P1, tr_low_low,tr_high_low))

## sampling benef beliefs
#adapt sampled tests
data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy, alpha = alphas_AM)

combination_beliefs_index <- combination_beliefs
combination_beliefs_index$policy <- result

result_policy <- combination_beliefs_index %>%
  mutate(policy = case_when(
    policy == "1-1" ~ "Do not\ndeploy",
    policy == "1-2" ~ "Deploy\nhealthy",
    policy == "2-1" ~ "Deploy\nunhealthy",
    policy == "2-2" ~ "Deploy"
  ))

## trajectory ####

models_list <- list()
for (mod_id in seq(nrow(data_mean_parameters))){
  params <- unlist(c(data_mean_parameters[mod_id,-1]))
  model <- transition_from_parameters(params, N_actions=2)
  models_list[[mod_id]] <- model
}
tr_mdp_real <- models_list[[1]]
tr_mdp_real <- transition_from_parameters(c(0.8,0.8,1,1), N_actions = 2)
#transition function momdp
transition_momdp <- transition_hmMDP(models_list)
#observation function momdp
observation_momdp <- obs_hmMDP(models_list)

tab_ <- trajectory(state_prior= 1,
                   Tmax = Tmax_SIM,
                   initial_belief_state = initial_belief_benef,
                   tr_mdp = tr_mdp_real,
                   rew_mdp= reward_reef,#dont care
                   tr_momdp = transition_momdp,
                   obs_momdp = observation_momdp,
                   alpha_momdp = alphas_AM,
                   disc = gamma,
                   optimal_policy = TRUE,
                   naive_policy = NA)

mod_probs <-as.data.frame(tab_$mod_probs)
names(mod_probs) <- c("A1A1","A1A2","A2A1","A2A2")

mod_probs <- mod_probs%>%
  mutate(belief_benef_low=1-A1A1-A1A2,
         belief_benef_high=1-A1A1-A2A1)

## FIGURES ####
AM_strategy_plot <- result_policy %>%
  mutate(policy = factor(policy, levels=c("Do not\ndeploy",
                                          "Deploy\nhealthy",
                                          "Deploy\nunhealthy",
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
    axis.text=element_text(size=13),
    axis.title = element_text(size=16),
    legend.text=element_text(size=12),
    legend.title=element_text(size=14),
    legend.spacing.y = unit(0.05,"cm"),
    legend.direction ="vertical",
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(ncol = 2))+
  labs(x = TeX("$\\beta^{res}$"),
       y = TeX("$\\beta^{prev}$"),
       fill = "Adaptive deployment\nstrategy"
  )+
  geom_line(data=mod_probs, aes(x=belief_benef_low, y=belief_benef_high))+
  geom_point(data=mod_probs, aes(x=belief_benef_low, y=belief_benef_high),
             shape = 15, size = 3)

## map EXPECTED VALUE #####
res <- read.csv(results_varying_priors_example_file_analytical)
breaks_countour <- c(0.01,1, 3,10,20)
value_AM_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  # geom_tile() +
  geom_raster(aes(fill=(value_AM_infty-value_noRD_infty)/value_noRD_infty*100),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = TeX("$\\beta^{res}_0$"),
       y = TeX("$\\beta^{prev}_0$"),
       fill = TeX("$\\frac{R_{AM}-R_{BAU}}{R_{BAU}}$(%)")
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
                          size = 6,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.6))+
  coord_equal()+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text=element_text(size=13),
    axis.title = element_text(size=16),
    legend.text=element_text(size=13),
    legend.title=element_text(size=14),
    legend.position="bottom"
  )

## map TMAX#####

breaks_countour <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
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
  coord_equal()+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.text=element_text(size=13),
    axis.title = element_text(size=16),
    legend.text=element_text(size=13),
    legend.title=element_text(size=14),
    legend.position="bottom"
  )


## GENERAL FIGURE ####

FIG <- ggarrange(max_time_heatMap,
                 value_AM_heatMap,
                 AM_strategy_plot,
                 nrow=1,
                 align="hv",
                 labels = c("A","B","C")
)
FIG

ggsave(AM_strategy_varying_priors_example_figure_analytical,
       plot = FIG, width = 10, height = 6, units = "in")
