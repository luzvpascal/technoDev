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

    ## Estimating reward value AM
    value_AM_infty <- c(analytical_Wready(tr_low_low,tr_high_low,
                                          costImp_P1,
                                          belief_benef_low,
                                          belief_benef_high)[[1]])
    ####################
    ##belief switch ####
    ####################
    belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1,
                                            p_idle = prob_ready,
                                            Rnord = value_noRD,
                                            Wready = value_AM_infty,
                                            disc = gamma)

    Tmax_years <- max_years(initial_belief_state_P1[1],
                              prob_ready,
                              belief_IS)

    res <- rbind(res,
                 data.frame(
                   belief_benef_low=belief_benef_low,
                   belief_benef_high=belief_benef_high,
                   value_AM_infty=value_AM_infty,
                   max_invest=Tmax_years
                 ))
  }
}

write.csv(res, results_varying_priors_example_file_analytical)
