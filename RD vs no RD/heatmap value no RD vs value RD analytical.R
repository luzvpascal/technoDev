# here we fix initial belief in project development
# fix the costs of development and deployment
# we vary the transition function for the action do nothing and
#we determine the max number of time steps investing in project dev before we give up
for (tr_low_low in coeffs_tr_low_low){

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

    ## value RD####
    value_AM_infty <- c(analytical_Wready(tr_low_low,tr_high_low, costImp_P1)[[1]])

    ####################
    ##belief switch ####
    ####################
    valueRD <- expected_value_analytical(belief_init = initial_belief_state_P1[1],
                                           cost_dev =costDev_P1,
                                            p_idle = prob_ready,
                                            Rnord = value_noRD,
                                            Wready = value_AM_infty,
                                            disc = gamma)

    res <- data.frame(tr_low_low = tr_low_low,
                      tr_high_low=tr_high_low,
                      valueRD=valueRD,
                      value_noRD=value_noRD/(1-gamma),
                      coeffDev_index=coeffDev,
                      coeffDeploy_index=coeffDeploy)

    write.table(res,
                file = heatmap_valueNoRD_valueRD_varying_transitions_file_analytical,
                sep = ",",
                col.names = FALSE,
                row.names = FALSE,
                append = TRUE )

  }
}
