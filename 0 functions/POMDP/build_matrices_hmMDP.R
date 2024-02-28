transition_hmMDP <- function(transition_compact){
  #inputs transition function in compact manner as returned by build_hmMDP()
  #returns the transition matrix of the hmMDP

  Num_S <- dim(transition_compact[[1]])[1]#number of fully observable states
  Num_a <- dim(transition_compact[[1]])[3]#number of actions
  Num_mod <- length(transition_compact)

  all_values_tr <- c()
  for (action in seq(Num_a)){
    element_list <- lapply(transition_compact, function(matrix) matrix[,,action])
    element_list <- lapply(element_list, as.matrix)
    matrix_action <- as.matrix(bdiag(element_list))

    all_values_tr <- c(all_values_tr, c(matrix_action))
  }
  tr_momdp <- array(all_values_tr,
                    dim = c(Num_mod*Num_S, Num_mod*Num_S, Num_a))
  return(tr_momdp)
}

reward_hmMDP <- function(reward, transition_compact){
  #inputs
  # reward function as matrix SxA
  # transition_compact transition function in compact manner as returned by build_hmMDP()
  #returns the reward matrix of the hmMDP

  Num_S <- dim(transition_compact[[1]])[1]#number of fully observable states
  Num_a <- dim(transition_compact[[1]])[3]#number of actions
  Num_mod <- length(transition_compact) #number of models

  rew_momdp <- matrix(0, ncol = Num_a, nrow <-Num_mod*Num_S )
  for (act_id in seq(Num_a)){
    rew_momdp[,act_id] <- rep(reward[,act_id], Num_mod)
  }
  return(rew_momdp)
}

obs_hmMDP <- function(transition_compact){
  #inputs transition function in compact manner as returned by build_hmMDP()
  #returns the observation matrix of the hmMDP as array (Y, S, A)

  Num_S <- dim(transition_compact[[1]])[1]#number of fully observable states
  Num_a <- dim(transition_compact[[1]])[3]#number of actions
  Num_mod <- length(transition_compact) #number of models


  obs_momdp <- matrix(rep(c(diag(Num_S)),Num_mod), ncol = Num_S, byrow = T)
  obs_momdp <- array(rep(c(obs_momdp), Num_a),
                   dim = c(Num_mod*Num_S,Num_S,Num_a))
  return(obs_momdp)
}
