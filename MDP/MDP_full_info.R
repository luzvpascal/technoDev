obs_index <- function(real_reefs, real_P1){
  return((real_P1)+(real_reefs-1)*2)
}

transition_MDP_full_info <- function(tr_mdp_reefs_real, tr_P1_real){
  #inputs real MDP features
  # tr_mdp_reefs_real list (2)(2,2,2)
  # tr_P1_real array (2,2,2)

  #outputs array (4,4,2): transition function of entire system

  Num_reef <- 2
  Num_P1 <- 2
  Num_a <-2

  transition_list <- array(0, dim=c(Num_reef*Num_P1,
                                    Num_reef*Num_P1,
                                    Num_a))

  combinations <- expand.grid(seq(Num_a),
                              seq(Num_reef),seq(Num_reef),
                              seq(Num_P1), seq(Num_P1))

  names(combinations) <- c("action",
                           "from_reef",
                           "to_reef",
                           "from_P1",
                           "to_P1")
  for (row_id in seq(nrow(combinations))){
    row <- combinations[row_id,]

    transition_list[obs_index(row$from_reef, row$from_P1),
                    obs_index(row$to_reef, row$to_P1),
                    row$action] <- (tr_mdp_reefs_real[[row$from_P1]][row$from_reef, row$to_reef, row$action]*
                                      tr_P1_real[row$from_P1, row$to_P1, row$action])
  }
  return(transition_list)
}

reward_MDP_full_info <- function(reward_reef, rew_mdp_P1){
  #inputs real MDP features
  # reward_reef array (2,2)
  # rew_mdp_P1 array (2,2)

  #outputs array (4,2): reward function of entire system

  Num_reef <- 2
  Num_P1 <- 2
  Num_a <-2

  reward_function <- matrix(0, nrow = (Num_reef*Num_P1),
                            ncol= Num_a)

  combinations <- expand.grid(seq(Num_a),
                              seq(Num_reef),
                              seq(Num_P1))

  names(combinations) <- c("action",
                           "from_reef",
                           "from_P1")
  for (row_id in seq(nrow(combinations))){
    row <- combinations[row_id,]

    reward_function[obs_index(row$from_reef, row$from_P1), row$action] <-
      (reward_reef[row$from_reef, row$action]+ rew_mdp_P1[row$from_P1, row$action])
  }
  return(reward_function)
}


