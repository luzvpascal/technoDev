# here we vary the costs of development and deployment
# and we determine the max number of time steps investing in project dev before we give up
tr_low_low <- tr_nothing[1,1]
tr_high_low <- tr_nothing[2,1]
for (coeffDev_index in coeffs_costDev_heatmap){
  print(coeffDev_index)
  costDev_P1_index <- coeffDev_index*V_max #might be even smaller

  for (coeffDeploy_index in coeffs_costDep_heatmap){
    costImp_P1_index <- coeffDeploy_index*costDev_P1_index #getting massive boats, build labs


    ##########################################
    ##reward function POMDP with new costs####
    ##########################################
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

    ## solving AM ####
    reward_ready_file_index <- paste0("res/rewards/reward_",costDev_P1_index,".csv")
    write.table(reward_reef+matrix(c(0,0,-costImp_P1_index,-costImp_P1_index), ncol=2),
                reward_ready_file_index, col.names = FALSE,
                row.names = FALSE, sep = ",")
    solving_AM(compute_mean_params=TRUE,
               tr_nothing_index,
               reward_ready_file_index,
               output_meanPars_file = output_meanPars_file(costDev_P1_index, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1_index, tr_low_low,tr_high_low),
               solve_hmMDP = TRUE,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1_index, tr_low_low,tr_high_low, initial_belief_benef_low, initial_belief_benef_high),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1_index, tr_low_low,tr_high_low, initial_belief_benef_low, initial_belief_benef_high),
               initial_belief_preset=TRUE
    )

    ## Estimating reward value AM
    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1_index, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high)) #alpha vectors

    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1_index, tr_low_low,tr_high_low))
    initial_belief_AM <- initial_belief_benef[data_mean_parameters$opt]
    initial_belief_AM <- initial_belief_AM/sum(initial_belief_AM)
    initial_belief_AM <- matrix(initial_belief_AM, nrow=1)

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
    reward_POMDP <- matrix(c(value_noRD, value_noRD, value_noRD-costDev_P1_index, value_AM), ncol=2)

    ##################################
    ##solving POMDP with new costs####
    ##################################
    models <- solving_POMDP(prob_ready,
                            initial_belief_state_P1,
                            reward_POMDP,
                            solve_hmMDP,
                            file_pomdpx_index=file_pomdpx_techdev(costDev_P1_index, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high),
                            file_outpolicy_index=file_outpolicy_techdev(costDev_P1_index, tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high))

    alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1_index,tr_low_low,tr_high_low,initial_belief_benef_low, initial_belief_benef_high)) #alpha vectors
    #transition momdp
    transition_momdp <- transition_hmMDP(models$all_models)
    #observation function momdp
    observation_momdp <- obs_hmMDP(models$all_models)

    tr_mdp_real <- transition_function_P1(1)

    for (belief_i in belief_tests){

      ## solve hidden model MDP####
      initial_belief_index <- c(belief_i,1-belief_i)

      tab_ <- trajectory(state_prior= 1,
                         Tmax = Tmax_SIM,
                         initial_belief_state = initial_belief_index,
                         tr_mdp = tr_mdp_real,
                         rew_mdp= reward_reef,#does not matter here
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

      res <- data.frame(tr_low_low = tr_low_low,
                        tr_high_low=tr_high_low,
                        belief = belief_i,
                        max_invest=results$max_invest[1],
                        coeffDev_index=coeffDev_index,
                        coeffDeploy_index=coeffDeploy_index)

      write.table(res,
                  file = graph_maxTime_file,
                  col.names = FALSE,
                  row.names = FALSE,
                  append = TRUE ,
                  sep=",")
    }
  }
}

