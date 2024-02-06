# here we fix initial belief in project development
# we fix initial belief in project benefits for ecosystem in unhealthy and healthy state
# fix the costs of development and deployment
# we vary the transition function for the action do nothing and
# we estimate value of RD and no RD

for (tr_low_low in coeffs_tr_low_low){
  print(tr_low_low)

  for (tr_high_low in coeffs_tr_high_low){
    ##steady states####
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

    ## solving AM ####
    solving_AM(compute_mean_params=TRUE,
               tr_nothing_index,
               reward_ready_file,
               output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
               solve_hmMDP = TRUE,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low, initial_belief_benef_low, initial_belief_benef_high),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low, initial_belief_benef_low, initial_belief_benef_high),
               initial_belief_preset=initial_belief_benef
    )

    ## Estimating reward value AM
    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high)) #alpha vectors

    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
    initial_belief_AM <- initial_belief_benef[data_mean_parameters$opt]
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
                            solve_hmMDP,
                            file_pomdpx_index=file_pomdpx_techdev(costDev_P1, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high),
                            file_outpolicy_index=file_outpolicy_techdev(costDev_P1, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high))

    ##reading and writing value RD vs value no RD####
    alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high)) #alpha vectors

    output <- interp_policy2(initial_belief_state_P1,
                             obs = 1,
                             alpha = alphas$vectors,
                             alpha_action = alphas$action,
                             alpha_obs = alphas$obs,
                             alpha_index = alphas$index)

    results <- data.frame(tr_low_low = tr_low_low,
                          tr_high_low = tr_high_low,
                          value_noRD = value_noRD_infty,
                          value_RD = output[[1]])
    write.table(results, heatmap_valueNoRD_valueRD_varying_transitions_file, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}
