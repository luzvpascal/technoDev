solving_POMDP <- function(p_idle_idle,
                          initial_belief,
                          reward_POMDP,
                          solve_hmMDP,
                          file_pomdpx_index,
                          file_outpolicy_index){
  #INPUTS:
  # p_idle_idle: probability that a technology stays idle
  # initial_belief: initial belief state: [probability feasible, probability infeasible]
  # reward_POMDP: reward function POMDP
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  #solves the POMDP for technology development

  #OUTPUTS:
  #list object:
    # all_models: possible models of technology development
    # initial_belief: initial_belief: initial belief state: [probability feasible, probability infeasible]

  ## transitions P1 ####
  all_models <- list(transition_function_P1(p_idle_idle),
                                transition_function_P1(1))

  ## solve hidden model MDP####
  write_hmMDP(TR_FUNCTION = all_models,
              B_FULL = c(1,0),
              B_PAR = initial_belief,
              REW = reward_POMDP,
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

  return(list(all_models=all_models,
              initial_belief=initial_belief))
}
