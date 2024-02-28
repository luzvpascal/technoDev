##transition function for technologies ####
transition_function_P1 <- function(parameter, Num_P1=2, Num_a=2){
  #parameter is the parameter of the transition from idle to ready
  #when choosing to develop the technology P1
  #NumP1 is the number of states (idle, ready)
  #Num_a is the number of actions (do nothing, actP1, actP2, actP1P2)

  return(array(c(1,0,0,1, parameter,0,1-parameter,1), dim = c(Num_P1, Num_P1, Num_a)))
}

#transition function for reefs ####
transition_from_parameters <- function(params, N_actions, N_states=2){
  #input vector of parameters that define a transition matrix
  # N_states: number of states
  # N_actions: number of actions

  # return a transition function as a an array (N_states, N_states, N_actions)
  transition <- c()
  for (act_id in c(1:N_actions)){
    mat <- matrix(c(params[(act_id-1)*2+1], params[(act_id-1)*2+2],
                    1-params[(act_id-1)*2+1], 1-params[(act_id-1)*2+2]),
                  nrow = 2)
    transition <- c(transition, mat)
  }
  transition <- array(transition, dim = c(N_states, N_states, N_actions))
  return(transition)
}


transition_reefs_write_hmMDP <- function(mods_reefs){
  #input: mods_reefs: output of mcuams_reef function
  #output: list of transition function for the reefs as list
  #to be used as input of write_hmMDP function
  # list(
  #   #P1 idle
  #   list(possible models P1 idle),
  #   #P1 ready
  #   list(possible models P1 ready)
  # )

  compact_transition_reefs_hmMDP <- list()
  for (P1_id in seq(2)){

      data_mean_parameters <- mods_reefs[[P1_id]]

      data_mean_parameters <- data_mean_parameters[order(data_mean_parameters$opt),] #list of optimal policies

      Num_mod <- nrow(data_mean_parameters)

      models_list <- list()
      for (mod_id in seq(Num_mod)){
        params <- unlist(c(data_mean_parameters[mod_id,-1]))
        model <- transition_from_parameters(params, N_actions=2)
        models_list[[mod_id]] <- model
      }

      compact_transition_reefs_hmMDP[[P1_id]]<- models_list
    }
  return(compact_transition_reefs_hmMDP)
}

