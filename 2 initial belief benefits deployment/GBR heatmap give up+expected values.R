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

alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,0.5,0.5)) #alpha vectors
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
    models <- solving_POMDP(prob_ready,
                            initial_belief_state_P1,
                            reward_POMDP,
                            solve_hmMDP = FALSE,
                            file_pomdpx_index=file_pomdpx_techdev(costDev_P1, tr_low_low,tr_high_low,belief_benef_low,belief_benef_high),
                            file_outpolicy_index=file_outpolicy_techdev(costDev_P1, tr_low_low,tr_high_low,belief_benef_low,belief_benef_high))

    ## figure out Tmax ####
    models <-list(transition_function_P1(prob_ready),
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
                      value_AM_infty=value_AM_infty))
 }
}

write.csv(res,
          results_varying_priors_example_file)
