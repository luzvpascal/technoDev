#here we aim to identify the frontiers of profiles of ecosystems:
#we aim to analyse the importance of the initial belief in benefits of technology deployment
#for the ecoystem

for (tr_low_low in seq(0.1,0.9,length.out=9)){

  for (tr_high_low in seq(0.1,0.9,length.out=9)){
    ##steady states####
    tr_nothing_index <- transition_from_parameters(c(tr_low_low,
                                                     tr_high_low),
                                                   N_actions=1)[,,1]
    ## solving AM ####
    solving_AM(compute_mean_params=FALSE,
               tr_nothing_index,
               reward_ready_file,
               output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
               solve_hmMDP = TRUE,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low, 0.5, 0.5),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low, 0.5, 0.5),
               initial_belief_preset=rep(0.25,4)
    )

    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,0.5,0.5))

    ## sampling benef beliefs
    #adapt sampled tests
    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
    sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
    result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy, alpha = alphas_AM)

    result <- as.data.frame(table(result))
    result <- result%>%
      mutate(tr_low_low=tr_low_low,
             tr_high_low=tr_high_low)

    write.table(result, profiles_definition_file, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}
