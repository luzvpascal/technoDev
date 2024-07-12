## TIME LIMIT TECH DEV ####
res <- data.frame()
res_analytical <- data.frame()
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
solving_AM(compute_mean_params=runMCUAMS,
           tr_nothing,
           reward_ready_file,
           output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
           output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
           solve_hmMDP = solve_hmMDP,
           file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low,
                                            initial_belief_benef_low,
                                            initial_belief_benef_high),
           file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,
                                                  initial_belief_benef_low,
                                                  initial_belief_benef_high),
           initial_belief_preset=initial_belief_benef
)

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
    solving_AM(compute_mean_params=FALSE,
               tr_nothing,
               reward_ready_file,
               output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
               solve_hmMDP = solve_hmMDP,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low,
                                                belief_benef_low,
                                                belief_benef_high),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,
                                                      belief_benef_low,
                                                      belief_benef_high),
               initial_belief_preset=initial_belief_AM
    )


    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,
                                                 belief_benef_low,
                                                 belief_benef_high))#alpha vectors

    value_unhealthy <- interp_policy2(initial_belief_AM,
                                      obs = 1,
                                      alpha = alphas_AM$vectors,
                                      alpha_action = alphas_AM$action,
                                      alpha_obs = alphas_AM$obs,
                                      alpha_index = alphas_AM$index)[[1]]
    action_unhealthy <- interp_policy2(initial_belief_AM,
                                      obs = 1,
                                      alpha = alphas_AM$vectors,
                                      alpha_action = alphas_AM$action,
                                      alpha_obs = alphas_AM$obs,
                                      alpha_index = alphas_AM$index)[[2]]

    value_healthy <- interp_policy2(initial_belief_AM,
                                    obs = 2,
                                    alpha = alphas_AM$vectors,
                                    alpha_action = alphas_AM$action,
                                    alpha_obs = alphas_AM$obs,
                                    alpha_index = alphas_AM$index)[[1]]

    action_healthy <- interp_policy2(initial_belief_AM,
                                       obs = 2,
                                       alpha = alphas_AM$vectors,
                                       alpha_action = alphas_AM$action,
                                       alpha_obs = alphas_AM$obs,
                                       alpha_index = alphas_AM$index)[[2]]

    value_AM_infty <- (c(steady_states%*%c(value_unhealthy,value_healthy)))
    value_AM <- (value_AM_infty)*(1-gamma)
    ## setting the value of reward_POMDP####
    reward_POMDP <- matrix(c(value_noRD, value_noRD, value_noRD-coeffDev, value_AM), ncol=2)

    ## Solving POMDP
    models <- solving_POMDP(prob_idle_idle,
                            initial_belief_state_P1,
                            reward_POMDP,
                            solve_hmMDP = solve_hmMDP,
                            file_pomdpx_index=file_pomdpx_techdev(costDev_P1, tr_low_low,tr_high_low,belief_benef_low,belief_benef_high),
                            file_outpolicy_index=file_outpolicy_techdev(costDev_P1, tr_low_low,tr_high_low,belief_benef_low,belief_benef_high))

    ## figure out Tmax ####
    models <-list(transition_function_P1(prob_idle_idle),
                  transition_function_P1(1))

    alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1, tr_low_low,tr_high_low,belief_benef_low,belief_benef_high)) #alpha vectors

    #transition function momdp
    transition_momdp <- transition_hmMDP(models)
    #observation function momdp
    observation_momdp <- obs_hmMDP(models)

    #real dynamics
    tr_mdp_real <-  transition_function_P1(1)

    tab_ <- trajectory(state_prior= 1,
                       Tmax = Tmax_SIM,
                       initial_belief_state = initial_belief_state_P1,
                       tr_mdp = tr_mdp_real,
                       rew_mdp= reward_reef,#dont care
                       tr_momdp = transition_momdp,
                       obs_momdp = observation_momdp,
                       alpha_momdp = alphas,
                       disc = gamma,
                       optimal_policy = TRUE,
                       naive_policy = NA)

    results <- tab_$data_output %>%
      mutate(invest=if_else(action >=2, 1,0)) %>%
      group_by(invest) %>%
      summarize(max_invest=max(max(time+1)*invest)) %>%
      select(-invest) %>%
      filter(max_invest==max(max_invest))

    res <- rbind(res, data.frame(belief_benef_low = belief_benef_low,
                                 belief_benef_high=belief_benef_high,
                                 max_invest=results$max_invest[1],
                                 value_AM_infty=value_AM_infty,
                                 policy = paste0(action_unhealthy,"-",action_healthy, sep="")
                                 ))

    ## analytical solution ####
    ## Estimating reward value AM
    value_AM_infty <- c(analytical_Wready(tr_low_low,tr_high_low,
                                          costImp_P1,
                                          belief_benef_low,
                                          belief_benef_high)[[1]])
    policy <- paste0(c(analytical_Wready(tr_low_low,tr_high_low,
                                         costImp_P1,
                                         belief_benef_low,
                                         belief_benef_high)[[2]]),
                     "-",
                     c(analytical_Wready(tr_low_low,tr_high_low,
                                         costImp_P1,
                                         belief_benef_low,
                                         belief_benef_high)[[3]]))
    ####################
    ##belief switch ####
    ####################
    belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1,
                                            p_idle = prob_idle_idle,
                                            Rnord = value_noRD,
                                            Wready = value_AM_infty,
                                            disc = gamma)

    Tmax_years <- max_years(initial_belief_state_P1[1],
                            prob_idle_idle,
                            belief_IS)

    res_analytical <- rbind(res_analytical,
                 data.frame(
                   belief_benef_low=belief_benef_low,
                   belief_benef_high=belief_benef_high,
                   value_AM_infty=value_AM_infty,
                   max_invest=Tmax_years,
                   policy=policy
                 ))
  }
}

write.csv(res,
          results_varying_priors_example_file, row.names = FALSE)

write.csv(res_analytical, results_varying_priors_example_file_analytical, row.names = FALSE)
## FIGURES ####
res <- read.csv(results_varying_priors_example_file)
## AM strategy ####
## trajectory ####
models_list <- list()
for (mod_id in seq(nrow(data_mean_parameters))){
  params <- unlist(c(data_mean_parameters[mod_id,-1]))
  model <- transition_from_parameters(params, N_actions=2)
  models_list[[mod_id]] <- model
}
# tr_mdp_real <- models_list[[1]]
tr_mdp_real <- transition_from_parameters(c(1,1,1,1), N_actions = 2)
#transition function momdp
transition_momdp <- transition_hmMDP(models_list)
#observation function momdp
observation_momdp <- obs_hmMDP(models_list)

alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,
                                             initial_belief_benef_low,
                                             initial_belief_benef_high))
tab_ <- trajectory(state_prior= 1,
                   Tmax = 100,
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

## AM PLOT####
result_policy <- res %>%
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
  scale_fill_manual(values = c("black",
                               "red",
                               "cyan2",
                               "palegreen"
  ))+
  coord_equal()+
  theme_bw()+
  guides(fill = guide_legend(byrow = TRUE,
                             nrow=4,
                             legend.title="none"))+
  theme(
    legend.position= "bottom",
    legend.box = "vertical",
    legend.key.width= unit(0.3, 'cm'),
    text=element_text(size=20),
    title=element_text(size=12),
    legend.title=element_text(size=18),
    axis.title=element_text(size=15, vjust=0.5)
  ) +
  scale_x_continuous(breaks = c(0,0.5,1), labels = c("0\nlow", 0.5, "1\nhigh")) +
  scale_y_continuous(breaks = c(0,0.5,1))+
  labs(x = TeX("Belief in restoration\ncapacity $\\beta^{r}_t$"),
       y = TeX("Belief in prevention\ncapacity $\\beta^{p}_t$"),
       fill = ""
       , title = "A. Adaptive management\nstrategy for deployment"
  )

## random trajectories
for (it in seq(2)){
  if (it==1){
    tr_mdp_real <- transition_from_parameters(c(0.8,0.8,0,0), N_actions = 2)
    state_prior <- 1
  } else if (it == 2){
    tr_mdp_real <- transition_from_parameters(c(0.8,0.8,0,1), N_actions = 2)
    state_prior <- 2
  } else if (it == 3){
    tr_mdp_real <- transition_from_parameters(c(0,0.8,1,0.1), N_actions = 2)
    state_prior <- 2
  }
  tab_rand <- trajectory(state_prior= state_prior,
                         Tmax = 10,
                         initial_belief_state = initial_belief_benef,
                         tr_mdp = tr_mdp_real,
                         rew_mdp= reward_reef,#dont care
                         tr_momdp = transition_momdp,
                         obs_momdp = observation_momdp,
                         alpha_momdp = alphas_AM,
                         disc = gamma,
                         optimal_policy = TRUE,
                         naive_policy = NA)

  mod_probs2 <-as.data.frame(tab_rand$mod_probs)
  names(mod_probs2) <- c("A1A1","A1A2","A2A1","A2A2")

  mod_probs2 <- mod_probs2%>%
    mutate(belief_benef_low=1-A1A1-A1A2,
           belief_benef_high=1-A1A1-A2A1)

  AM_strategy_plot <- AM_strategy_plot+
    geom_line(data=mod_probs2, aes(x=belief_benef_low, y=belief_benef_high),
              color=paste0("grey",it), alpha=0.3)
}

AM_strategy_plot <- AM_strategy_plot+
  geom_line(data=mod_probs, aes(x=belief_benef_low, y=belief_benef_high))+
  geom_point(data=mod_probs[-1,], aes(x=belief_benef_low, y=belief_benef_high),
             shape = 22, size = 3, col="white", fill="black")+
  geom_point(data=mod_probs[1,],
             aes(x=belief_benef_low, y=belief_benef_high),
             shape = 23, size = 3, col="black", fill="red")+
  annotate("text", x= mod_probs$belief_benef_low[seq(3)]+0.05,
           y=mod_probs$belief_benef_high[seq(3)]-0.1,
           label= TeX(paste("$\\beta_",seq(0,2),"$")),
           size = 5)+
  annotate("text", x= mod_probs$belief_benef_low[4],
           y=mod_probs$belief_benef_high[4]-0.1,
           label= "...",
           size = 5)

## map EXPECTED VALUE #####
res <- res %>%
  mutate(valueAMinst=value_AM_infty*(1-gamma))
break_point_up <- round(max(res$valueAMinst),digits=1)
break_point_low <-0.003 + value_noRD-costDev_P1

breaks_countour_AM <- c(break_point_low,0.65,0.7)

value_AM_heatMap <-  res %>%
  ggplot(aes(x = belief_benef_low, y = belief_benef_high)) +
  geom_raster(aes(fill=valueAMinst),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "darkred"
                      # ,
                      # breaks = c(0.6,0.75),
                      # labels = c(0.6,0.75)
  )+
  labs(x = TeX("$\\beta^{r}_0$ (restoration)"),
       y = TeX("$\\beta^{p}_0$ (prevention)"),
       fill = TeX("Deployment benefits $R_{AM}$")
       # ,
       # title="B. Yearly deployment\nbenefits"
  ) +
  geom_contour(aes(z = valueAMinst),
               binwidth = 0.1,
               breaks = breaks_countour_AM,
               colour = "black") +
  metR::geom_text_contour(aes(z = valueAMinst),
                          breaks = breaks_countour_AM,
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 5,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.3))+
  coord_equal()+
  theme_bw()+
  theme(
    legend.position="bottom",
    legend.title.position="top",
    legend.key.height= unit(0.75, 'cm'),
    legend.key.width= unit(1, 'cm'),
    text=element_text(size=20),
    title=element_text(size=12),
    legend.title=element_text(size=18),
    axis.title=element_text(size=18),
    legend.title.align=0.5
  ) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))+
  geom_point(data=mod_probs[1,],
             aes(x=belief_benef_low, y=belief_benef_high),
             shape = 23, size = 3, col="black", fill="red")+
  annotate("text", x=initial_belief_benef_low+0.05,
           y=initial_belief_benef_high-0.1,
           label= TeX("$\\beta_0$"),
           size = 5,
           colour = "white")
## map difference #####
res <- res %>%
  mutate(diff=valueAMinst-value_noRD+costDev_P1)
break_point_low <- 0.003#max(res$diff[which(res$max_invest==0)])

breaks_countour <- c(break_point_low,0.05,0.1)
break_point <- round(max(res$diff),digits=1)
value_diff_heatMap <-  res %>%
  ggplot(aes(x = belief_benef_low, y = belief_benef_high)) +
  geom_raster(aes(fill=diff),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "darkgreen"
                      # ,
                      # breaks = c(costDev_P1,break_point/2,break_point-costDev_P1),
                      # labels = c(0,break_point/2,break_point)
  ) +
  labs(x = TeX("$\\beta^{r}_0$ (restoration)"),
       y = TeX("$\\beta^{p}_0$ (prevention)"),
       fill = TeX("Net benefits $(\\Delta_R)$")
       # ,
       # title="C. Yearly deployment\nnet benefits"
  ) +
  geom_contour(aes(z = diff),
               binwidth = 0.05,
               breaks = breaks_countour,
               colour = "black") +
  metR::geom_text_contour(aes(z = diff),
                          breaks = breaks_countour,
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 5,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.3))+
  coord_equal()+
  theme_bw()+
  theme(
    legend.position="bottom",
    legend.title.position="top",
    legend.key.height= unit(0.75, 'cm'),
    legend.key.width= unit(1, 'cm'),
    text=element_text(size=20),
    title=element_text(size=12),
    legend.title=element_text(size=18),
    axis.title=element_text(size=18),
    legend.title.align=0.5
  ) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))+
  geom_point(data=mod_probs[1,],
             aes(x=belief_benef_low, y=belief_benef_high),
             shape = 23, size = 3, col="black", fill="red")+
  annotate("text", x=initial_belief_benef_low+0.05,
           y=initial_belief_benef_high-0.1,
           label= TeX("$\\beta_0$"),
           size = 5,
           colour = "white")

## map TMAX#####
breaks_countour_Tmax <- c(1,37,43)
max_time_heatMap <- ggplot(res,
                           aes(x = belief_benef_low, y = belief_benef_high)) +
  geom_raster(aes(fill=max_invest),interpolate = TRUE) +
  scale_fill_gradient(low = "white", high = "purple4"
                      # ,
                      # breaks = c(0,20,40),
                      # labels = c(0,20,40)
  ) +
  labs(x = TeX("$\\beta^{r}_0$ (restoration)"),
       y = TeX("$\\beta^{p}_0$ (prevention)"),
       fill = TeX("Development time limit $T_{max}$"))+
       # ,
       # title="D. Optimal stopping\ntime for development") +
  geom_contour(aes(z = max_invest),
               binwidth = 10,
               breaks = breaks_countour_Tmax,
               colour = "black") +
  metR::geom_text_contour(aes(z = max_invest),
                          breaks = breaks_countour_Tmax,
                          min.size = 0,
                          skip=0,
                          stroke = 0.1,
                          rotate = FALSE,
                          size = 5,
                          fontface = "bold",
                          label.placer = metR::label_placer_fraction(frac = 0.3))+
  coord_equal()+
  theme_bw()+
  theme(
    text=element_text(size=20),
    title=element_text(size=12),
    legend.position="bottom",
    legend.title.position="top",
    legend.title=element_text(size=18),
    axis.title=element_text(size=18),
    legend.key.height= unit(0.75, 'cm'),
    legend.key.width= unit(1, 'cm'),
    legend.title.align=0.5
  ) +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_y_continuous(breaks = c(0,0.5,1))+
  geom_point(data=mod_probs[1,],
             aes(x=belief_benef_low, y=belief_benef_high),
             shape = 23, size = 3, col="black", fill="red")+
  annotate("text", x=initial_belief_benef_low+0.05,
           y=initial_belief_benef_high-0.1,
           label= TeX("$\\beta_0$"),
           size = 5,
           colour = "white")



## GENERAL FIGURE ####

FIG <- ggarrange(
  AM_strategy_plot,
  value_AM_heatMap,
  value_diff_heatMap,
  max_time_heatMap,
  heights = 1.5,
  align="hv",
  ncol=4,nrow=1)

# Calculate x-coordinates for the dotted line
n_plots <- 4  # Number of plots
plot_width <- 0.25  # Width of each plot
x_coords <- seq(0.25, plot_width * (n_plots - 1), by = plot_width)

# Add dotted lines between plots
for (x_coord in x_coords[-n_plots]) {
  FIG <- FIG + geom_vline(xintercept = x_coord, linetype = "dotted",
                          color = "grey")
}

ggsave(results_varying_priors_example_figure,
       plot = FIG, height = 5, width = 13, units = "in")



