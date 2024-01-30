# here we fix initial belief in project development
# fix the costs of development and deployment
# we vary the transition function for the action do nothing and
#we determine the max number of time steps investing in project dev before we give up
for (tr_low_low in coeffs_tr_low_low){
  for (tr_high_low in coeffs_tr_high_low){

    models <-list(transition_function_P1(prob_ready),
                  transition_function_P1(1))

    alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1,tr_low_low,tr_high_low,
                                                   initial_belief_benef_low,
                                                   initial_belief_benef_high)) #alpha vectors

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

    res <- data.frame(tr_low_low = tr_low_low,
                      tr_high_low=tr_high_low,
                      max_invest=results$max_invest[1],
                      coeffDev_index=coeffDev,
                      coeffDeploy_index=coeffDeploy)

    write.table(res,
                file = heatmap_maxTime_file_varying_transitions,
                col.names = FALSE,
                row.names = FALSE,
                append = TRUE )
  }
}
