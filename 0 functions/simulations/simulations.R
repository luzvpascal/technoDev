belief_state_function <- function(state, belief, N_state){
  #input:
  #state: integer between 1 and number of possible states
  #belief: belief state over possible models
  #N_state: number of possible obs states

  #returns: vector of belief over models and observable states

  mat_sol <- matrix(0,nrow = N_state,ncol=length(belief))
  mat_sol[state,] <- belief

  return(matrix(mat_sol,nrow=1))
}

sum_Nstate_by_Nstate <- function(vector, N_state){
  #vector: vector of beliefs state over possible models and obs states
  #N_state: number of possible obs states

  #return belief over possible models only
  mat <- matrix(vector, nrow=N_state)
  return(colSums(mat))
}

trajectory <- function(state_prior,
                       Tmax,
                       initial_belief_state,
                       tr_mdp,
                       rew_mdp,
                       tr_momdp,
                       obs_momdp,
                       alpha_momdp,
                       disc = 0.95,
                       optimal_policy = TRUE,
                       naive_policy = NA,
                       alpha_indexes=FALSE) {

  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # tr_mdp: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  # initialise Num_mod and Num_state
  Num_mod <- length(initial_belief_state)
  Num_s <- nrow(rew_mdp)
  Num_a <- ncol(rew_mdp)
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c(0) #initial reward is 0

  if (alpha_indexes){indexes <- c()}
  state <- state_prior
  mod_probs <- matrix(initial_belief_state, ncol = Num_mod)

  for (i in seq(Tmax)) {
    #compute next best action0
    if (optimal_policy){
      output <- interp_policy2(mod_probs[i,],
                               obs = state[i],
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)
      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}

    } else {
      act <- naive_policy(state[i],
                          i)
      actions <- c(actions, act)
    }

    #update reward
    V <- c(V, V[i] + disc**(i-1)*rew_mdp[state[i], actions[i]])

    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state <- c(state, sample(seq(Num_s), size=1, replace = TRUE,
                                 prob = c(tr_mdp[state[i], ,actions[i]])))

    #update beliefs reefs
    belief_state<- update_belief(belief_state_function(state[i],mod_probs[i,],Num_s ),
                                 tr_momdp,
                                 obs_momdp,
                                 state[i+1],
                                 actions[i])

    mod_probs <- rbind(mod_probs, sum_Nstate_by_Nstate(belief_state, Num_s))
  }

  data_output <- data.frame(state=state)
  data_output$value <- V
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}




sim_mdp_momdp_policy <- function(state_prior,
                                 Tmax,
                                 initial_belief_state,
                                 tr_mdp,
                                 rew_mdp,
                                 tr_momdp,
                                 obs_momdp,
                                 alpha_momdp,
                                 disc = 0.95,
                                 optimal_policy = TRUE,
                                 naive_policy = NA,
                                 n_it = 100,
                                 average=TRUE,
                                 alpha_indexes=FALSE) {
  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # tr_mdp: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  # n_it = number of iterations, number of times a trajectory is computed
  # average : bool, average the value function among all iterations

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  #create a list of dataframes as returned by trajectory
  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
  doParallel::registerDoParallel(cl)
  list_results <- foreach::foreach(i=1:n_it,
                                   .export=c('trajectory',
                                             'sum_Nstate_by_Nstate',
                                             'belief_state_function',
                                             'interp_policy2',
                                             'update_belief')) %dopar% {
                                               trajectory(state_prior,
                                                          Tmax,
                                                          initial_belief_state,
                                                          tr_mdp,
                                                          rew_mdp,
                                                          tr_momdp,
                                                          obs_momdp,
                                                          alpha_momdp,
                                                          disc,
                                                          optimal_policy,
                                                          naive_policy,
                                                          alpha_indexes)$data_output
                                             }
  parallel::stopCluster(cl)

  #rbind all dataframes
  list_results <- dplyr::bind_rows(list_results, .id = "column_label")
  if (average){
    list_results <- data.table::data.table(list_results)
    list_results <- list_results[,mean(value), by = time]
    return(list_results$V1)
  } else {
    return(list_results)
  }
}

modify_mod_probs <- function(result_list, action_bool=FALSE) {
  modified_list <- lapply(result_list, function(result) {
    state_col <- result$data_output$state
    mod_probs_matrix <- result$mod_probs
    mod_probs_with_state <- cbind(mod_probs_matrix, state = state_col)
    if (action_bool){
      action_col <- result$data_output$action
      mod_probs_with_state <- cbind(mod_probs_with_state, action = action_col)
      }

    result$mod_probs <- mod_probs_with_state
    result
  })
  modified_list
}

beliefs <- function(state_prior,
                   Tmax,
                   initial_belief_state,
                   tr_mdp,
                   rew_mdp,
                   tr_momdp,
                   obs_momdp,
                   alpha_momdp,
                   disc = 0.95,
                   optimal_policy = TRUE,
                   naive_policy = NA,
                   n_it = 100,
                   action_bool=FALSE) {
  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # tr_mdp: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  # n_it = number of iterations, number of times a trajectory is computed
  #action_bool bool indicating if

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  #create a list of dataframes as returned by trajectory
  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
  doParallel::registerDoParallel(cl)
  list_results <- foreach::foreach(i=1:n_it,
                                   .export=c('trajectory',
                                             'sum_Nstate_by_Nstate',
                                             'belief_state_function',
                                             'interp_policy2',
                                             'update_belief',
                                             'best_non_adaptive_res')) %dopar% {
                                               trajectory(state_prior,
                                                          Tmax,
                                                          initial_belief_state,
                                                          tr_mdp,
                                                          rew_mdp,
                                                          tr_momdp,
                                                          obs_momdp,
                                                          alpha_momdp,
                                                          disc,
                                                          optimal_policy,
                                                          naive_policy,
                                                          alpha_indexes=FALSE)
                                             }
  parallel::stopCluster(cl)

  # Apply the modification function to list_results
  list_results <- modify_mod_probs(list_results, action_bool)

  # Create a new list with only the modified mod_probs matrices
  list_results <- lapply(list_results, function(result) {
    result$mod_probs
  })
  list_results <- lapply(list_results, as.data.frame)
  # #rbind all dataframes
  #
  list_results <- dplyr::bind_rows(list_results, .id = "column_label")
  list_results <- list_results %>% select(-column_label)
  return(list_results)
}



sim_mdp_momdp_policy_quadrants <- function(steady_state,
                                 Tmax,
                                 initial_belief_state,
                                 list_tr_mdp,
                                 rew_mdp,
                                 tr_momdp,
                                 obs_momdp,
                                 alpha_momdp,
                                 disc = 0.95,
                                 optimal_policy = TRUE,
                                 naive_policy = NA,
                                 n_it = 100,
                                 average=TRUE,
                                 alpha_indexes=FALSE) {
  #inputs
  # steady_state: steady state under current management
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # list_tr_mdp: possible transition function of the real mdp on which hmMDP policy is tested (list of models) (X, X, A)
  # must have the same size as initial_belief_state
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  # n_it = number of iterations, number of times a trajectory is computed
  # average : bool, average the value function among all iterations

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  #create a list of dataframes as returned by trajectory
  indexes_state_prior <- sample(1:length(steady_state),n_it, replace=TRUE, prob=steady_state)
  initial_belief_state_no <- initial_belief_state[-length(initial_belief_state)]
  indexes_mdps <- sample(1:length(initial_belief_state_no), n_it, replace=TRUE, prob = initial_belief_state_no)
  # indexes_mdps <- sample(1:length(initial_belief_state), n_it, replace=TRUE, prob = initial_belief_state)

  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
  doParallel::registerDoParallel(cl)
  list_results <- foreach::foreach(i=1:n_it,
                                   .export=c('trajectory',
                                             'sum_Nstate_by_Nstate',
                                             'belief_state_function',
                                             'interp_policy2',
                                             'update_belief')) %dopar% {
                                               trajectory(indexes_state_prior[i],
                                                          Tmax,
                                                          initial_belief_state,
                                                          list_tr_mdp[[indexes_mdps[i]]],
                                                          rew_mdp,
                                                          tr_momdp,
                                                          obs_momdp,
                                                          alpha_momdp,
                                                          disc,
                                                          optimal_policy,
                                                          naive_policy,
                                                          alpha_indexes)$data_output
                                             }
  parallel::stopCluster(cl)

  #rbind all dataframes
  list_results <- dplyr::bind_rows(list_results, .id = "simulation")
  list_results$index_mdp <- indexes_mdps[as.numeric(list_results$simulation)]
  if (average){
    list_results <- data.table::data.table(list_results)
    list_results <- list_results[,mean(value), by = time]
    return(list_results$V1)
  } else {
    return(list_results)
  }
}
