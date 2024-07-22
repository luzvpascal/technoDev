solving_AM <- function(compute_mean_params,
                        tr_nothing_index,
                        reward_ready_file_index,
                        output_meanPars_file_index,
                        output_priors_file_index,
                        solve_hmMDP,
                        file_pomdpx_index,
                        file_outpolicy_index,
                        initial_belief_preset=NA
){
  ## run MCUAMS ####
  if (compute_mean_params){
    system(paste(file_MCUAMS,
                 reward_ready_file_index,
                 output_meanPars_file_index,
                 output_priors_file_index,
                 as.character(tr_nothing_index[1,1]),
                 as.character(tr_nothing_index[2,1])))
  }
  ##Models reefs####
  data_mean_parameters <- read.csv(output_meanPars_file_index)

  Num_mod <- nrow(data_mean_parameters)

  models_list <- list()
  for (mod_id in seq(Num_mod)){
    params <- unlist(c(data_mean_parameters[mod_id,-1]))
    model <- transition_from_parameters(params, N_actions=2)
    models_list[[mod_id]] <- model
  }

  ##slve POMDP
  reward_intermediate <- as.matrix(unname(read.csv(reward_ready_file_index, header = FALSE))) %>%
    suppressWarnings()
  if (!is.na(initial_belief_preset[1])){
    initial_belief <- initial_belief_preset
    initial_belief <- initial_belief[data_mean_parameters$opt]
    initial_belief <- initial_belief/sum(initial_belief)
  } else {
    initial_belief <- read.csv(output_priors_file_index)$freq
  }
  write_hmMDP(TR_FUNCTION = models_list,
              B_FULL = c(1,0),
              B_PAR = initial_belief,
              REW = reward_intermediate,
              GAMMA= gamma,
              FILE=file_pomdpx_index)

  path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

  cmd <- paste(path_to_sarsop,
               "--precision", precision,
               "--timeout", timeout ,
               "--output", file_outpolicy_index,
               file_pomdpx_index,
               sep=" ")

  if (solve_hmMDP){system(cmd)}
}
