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
    legend.position = "right"
  ) +
  guides(fill = guide_legend(ncol = 2))+
  labs(x = TeX("$\\beta^{res}$"),
       y = TeX("$\\beta^{prev}$"),
       fill = "Adaptive deployment\nstrategy"
  )+
  geom_line(data=mod_probs, aes(x=belief_benef_low, y=belief_benef_high))+
  geom_point(data=mod_probs, aes(x=belief_benef_low, y=belief_benef_high),
             shape = 15, size = 3)
