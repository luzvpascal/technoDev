results <- data.frame()
tr_low_low <- tr_nothing[1,1]
tr_high_low <- tr_nothing[2,1]
# here we vary the costs of development and deployment
# and we determine the max number of time steps investing in project dev before we give up
for (coeffDev_index in c(0.01, 0.001, 0.0001)){
  print(coeffDev_index)
  costDev_P1_index <- coeffDev_index*V_max #might be even smaller

  for (coeffDeploy_index in coeffs_costDep_heatmap){
    costImp_P1_index <- coeffDeploy_index*costDev_P1_index #getting massive boats, build labs
    ##########################################
    ##reward function POMDP with new costs####
    ##########################################
    ##steady states####
    statesNames <- c("unhealthy", "healthy")
    markovB <- new("markovchain", states = statesNames, transitionMatrix=tr_nothing,
                   name = "A markovchain Object"
    )
    steady_states <- steadyStates(markovB)[1,]

    ## value No RD ####
    value_noRD <- c(steady_states%*%c(V_min, V_max))

    ## Estimating reward value AM
    value_AM_infty <- c(analytical_Wready(tr_low_low,tr_high_low, costImp_P1_index, initial_belief_benef_low, initial_belief_benef_high)[[1]])
    ####################
    ##belief switch ####
    ####################
    belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1_index,
                                     p_idle = prob_ready,
                                     Rnord = value_noRD,
                                     Wready = value_AM_infty,
                                     disc = gamma)

    for (belief_i in belief_tests){
      Tmax_years <- max_years(belief_i,
                              prob_ready,
                            belief_IS)

      results <- rbind(results,
                       data.frame(belief = belief_i,
                                  Tmax_years = Tmax_years,
                                  coeffDev_index=coeffDev_index,
                                  coeffDeploy_index=coeffDeploy_index))
    }
  }
}
results$Tmax_years <- pmax(results$Tmax_years,0)
write.csv(results, graph_maxTime_file_analytical, row.names = FALSE)
