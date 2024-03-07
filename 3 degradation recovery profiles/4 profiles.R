# here we fix initial belief in project development
# we fix the transitions under BAU
# fix the costs of development and deployment
# we vary the initial belief that the project will be beneficial when low and high
#we determine the max number of time steps investing in project dev before we give up

## preset transition matrix if do nothing ####
profile_possibilities <- c(
                           "A fast degradation\nfast recovery",
                           "B fast degradation\nslow recovery",
                           "C slow degradation\nfast recovery",
                           "D slow degradation\nslow recovery"
                           )

for (profile in profile_possibilities){
    if (profile == "B fast degradation\nslow recovery"){
      tr_low_low <- 0.9
      tr_high_low <- 0.9
    } else if (profile=="A fast degradation\nfast recovery"){
      tr_low_low <- 0.1
      tr_high_low <- 0.9
    } else if (profile=="D slow degradation\nslow recovery"){
      tr_low_low <- 0.9
      tr_high_low <- 0.1
    } else if (profile=="C slow degradation\nfast recovery"){
      tr_low_low <- 0.1
      tr_high_low <- 0.1
    }

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

    ## solve adaptive management deployment####
    solving_AM(compute_mean_params=FALSE,
               tr_nothing_index,
               reward_ready_file,
               output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
               solve_hmMDP = FALSE,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low,
                                                initial_belief_benef_low,
                                                initial_belief_benef_high),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,
                                                      initial_belief_benef_low,
                                                      initial_belief_benef_high),
               initial_belief_preset=initial_belief_benef
    )

    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1,
                                                 tr_low_low,
                                                 tr_high_low,
                                                 initial_belief_benef_low,
                                                 initial_belief_benef_high)) #alpha vectors

    ## ADAPTIVE MANAGEMENT STRATEGY ####
    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
    sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
    result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy, alpha = alphas_AM)

    combination_beliefs_index <- combination_beliefs
    combination_beliefs_index$policy <- result
    combination_beliefs_index$profile <- profile
    combination_beliefs_index$max_invest <- 0
    ## Tmax calculation ####
    for (belief_benef_low in coeffs_initial_belief_benefits_low){
      print(belief_benef_low)
      for (belief_benef_high in coeffs_initial_belief_benefits_high){
        ## solving AM ####
        initial_belief_benef_index <- c( #initial belief tech beneficial
          (1-belief_benef_low)*(1-belief_benef_high), ##initial belief do nothing better than tech
          (1-belief_benef_low)*(belief_benef_high),##initial belief tech better than do nothing high
          (belief_benef_low)*(1-belief_benef_high), ##initial belief tech better than do nothing low
          (belief_benef_low)*(belief_benef_high)##initial belief tech better than do nothing
        )

        ## Estimating reward value AM
        initial_belief_AM <- initial_belief_benef_index[data_mean_parameters$opt]
        initial_belief_AM <- initial_belief_AM/sum(initial_belief_AM)

        value_unhealthy <- interp_policy2(initial_belief_AM,
                                          obs = 1,
                                          alpha = alphas_AM$vectors,
                                          alpha_action = alphas_AM$action,
                                          alpha_obs = alphas_AM$obs,
                                          alpha_index = alphas_AM$index)[[1]]
        value_healthy <- interp_policy2(initial_belief_AM,
                                        obs = 2,
                                        alpha = alphas_AM$vectors,
                                        alpha_action = alphas_AM$action,
                                        alpha_obs = alphas_AM$obs,
                                        alpha_index = alphas_AM$index)[[1]]

        value_AM_infty <- (c(steady_states%*%c(value_unhealthy,value_healthy)))
        value_AM <- (value_AM_infty)*(1-gamma)
        ## setting the value of reward_POMDP####
        reward_POMDP <- matrix(c(value_noRD, value_noRD, value_noRD-coeffDev, value_AM), ncol=2)

        ## Solving POMDP
        models <- solving_POMDP(prob_idle_idle,
                                initial_belief_state_P1,
                                reward_POMDP,
                                solve_hmMDP = FALSE,
                                file_pomdpx_index=file_pomdpx_techdev(costDev_P1,
                                                                      tr_low_low,
                                                                      tr_high_low,
                                                                      belief_benef_low,
                                                                      belief_benef_high),
                                file_outpolicy_index=file_outpolicy_techdev(costDev_P1,
                                                                            tr_low_low,
                                                                            tr_high_low,
                                                                            belief_benef_low,
                                                                            belief_benef_high))

        ## figure out Tmax ####
        models <-list(transition_function_P1(prob_idle_idle),
                      transition_function_P1(1))

        alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1,
                                                       tr_low_low,
                                                       tr_high_low,
                                                       belief_benef_low,
                                                       belief_benef_high)) #alpha vectors

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

        combination_beliefs_index[which(
          combination_beliefs_index$belief_benef_low == belief_benef_low &
            combination_beliefs_index$belief_benef_high == belief_benef_high),]$max_invest <- results$max_invest[1]
      }
    }

    write.table(combination_beliefs_index, results_4_profiles_file, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)
}

## FIGURE ####
results <- read.csv(results_4_profiles_file, sep=",",
                    header=FALSE)
names(results) <- c("belief_benef_low",
                    "belief_benef_high",
                    "policy",
                    "profile",
                    "max_invest")

results$profile <- gsub("-","\n",as.character(results$profile))
## map #####
breaks_countour <- c(0.01,seq(10,50,10))
max_time_heatMap <- ggplot(results,
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

# AM strategy figure ###
results <- results %>%
  mutate(policy = case_when(
    policy == "1-1" ~ "Do not deploy",
    policy == "1-2" ~ "Deploy healthy",
    policy == "2-1" ~ "Deploy unhealthy",
    policy == "2-2" ~ "Deploy"
  ))

AM_strategy_plot <- results %>%
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
       fill = "Adaptive\ndeployment strategy"
  )
print(AM_strategy_plot)

## joining the 2 figures ####
fig <- ggarrange(max_time_heatMap,
                 AM_strategy_plot,
                 nrow=2,
                 align = "hv")
fig
ggsave(results_4_profiles_figure,
       plot = fig, width = 15, height = 6, units = "in")
