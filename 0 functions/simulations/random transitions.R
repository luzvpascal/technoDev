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

random_transition <- function(N_actions, N_states=2, known_params=FALSE,
                                         vect_known_params=NA){
  #inputs
  # N_states: number of states
  # N_actions: number of actions
  # known_params is a bool indicating if there are parameters known
  # vect_known_params is a data.frame with 2 cols:
  #index: the indexes of the known parameters
  #value: values of the known parameters
  
  # return a transition function as a an array (N_states, N_states, N_actions)

  N_param <- N_states*(N_states-1)*N_actions
  
  if (known_params){
    params <- rep(0, N_param)
    params_index <- seq(N_param)
    
    #update number of parameters to sort
    N_param_interm <- N_param-nrow(vect_known_params)
    params_index <- params_index[-vect_known_params$index] #update indexes of new parameters
    
    #draw new number of parameters
    params_interm <- runif(N_param_interm)
    params[params_index] <- params_interm
    params[vect_known_params$index] <- vect_known_params$value
    params <- matrix(params, nrow=2)
  } else {
    params <- matrix(runif(N_param), nrow = 2)
  }
  
  return(transition_from_parameters(params, N_actions, N_states))
} 


random_transition_projects <- function(ProjectIndex=1, feasible=TRUE){
  # inputs:
  # ProjectIndex: index of the technology considered
  # feasible: boolean indicating if the project is feasible
  
  # return a transition function as a an array (2, 2, 4)
  N_actions <- 4 #4 actions considered
  
  known_parameters <- rep(c(1,0), N_actions)  
  if (feasible) {
    params_interm <- runif(1)
    if (ProjectIndex==1){
      known_parameters[3] <- params_interm
      known_parameters[7] <- params_interm
    } else {
      known_parameters[5] <- params_interm
      known_parameters[7] <- params_interm
    }
  }
  return(transition_from_parameters(known_parameters, N_actions))
}

transition_reefs <- function(tr_nothing, tr_reefsP1only_real,
                             tr_reefsP2only_real, tr_reefsP1P2_real,
                             feasible_P1, feasible_P2){
  
  if (feasible_P1){
    if (feasible_P2){
      return(list(
        #P1 idle
        list(array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
             array(c(tr_nothing, tr_nothing, tr_reefsP2only_real, tr_reefsP2only_real), dim=c(2,2,4))),#P2 ready
        #P1 ready
        list(
          array(c(tr_nothing, tr_reefsP1only_real, tr_nothing, tr_reefsP1only_real), dim=c(2,2,4)), #P2 idle
          array(c(tr_nothing, tr_reefsP1only_real, tr_reefsP2only_real, tr_reefsP1P2_real), dim=c(2,2,4)))#P2 ready
      ))
    } else {
      return(list(
        #P1 idle
        list(array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
             array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4))),#P2 ready
        #P1 ready
        list(
          array(c(tr_nothing, tr_reefsP1only_real, tr_nothing, tr_reefsP1only_real), dim=c(2,2,4)), #P2 idle
          array(c(tr_nothing, tr_reefsP1only_real, tr_nothing, tr_reefsP1only_real), dim=c(2,2,4)))#P2 ready
      ))
    }
  } else {
    if (feasible_P2){
      return(list(
        #P1 idle
        list(array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
             array(c(tr_nothing, tr_nothing, tr_reefsP2only_real, tr_reefsP2only_real), dim=c(2,2,4))),#P2 ready
        #P1 ready
        list(
          array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
          array(c(tr_nothing, tr_nothing, tr_reefsP2only_real, tr_reefsP2only_real), dim=c(2,2,4)))#P2 ready
      ))
    } else {
      return(list(
        #P1 idle
        list(array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
             array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4))),#P2 ready
        #P1 ready
        list(
          array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)), #P2 idle
          array(c(tr_nothing, tr_nothing, tr_nothing, tr_nothing), dim=c(2,2,4)))#P2 ready
      ))
    }
  }
  
  
}
