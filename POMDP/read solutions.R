## some additional funcitons ####
read_policyx2 <- function(file){
  #inputs:
  #file as tring with location of policyx file

  #outpout:
  #list of
  #vectors: alpha vectors
  #action: optimal action for each alpha vector
  #obs: observation corresponding to each alpha vector
  xml <- xml2::read_xml(file)
  xml_vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)),
                                                " ")[[1]])
  get_action <- function(v) as.numeric(xml2::xml_attr(v, "action"))
  get_obs <- function(v) as.numeric(xml2::xml_attr(v, 'obsValue'))
  n_states <- length(get_vector(xml_vectors[[1]]))
  alpha <- vapply(xml_vectors, get_vector, numeric(n_states))
  alpha_action <- vapply(xml_vectors, get_action, double(1)) + 1
  alpha_obs <- vapply(xml_vectors, get_obs, double(1)) + 1
  list(vectors=matrix(alpha, nrow=n_states), action=alpha_action, obs=alpha_obs, index = seq(ncol(alpha)))
}


interp_policy2 <- function (belief_state_momdp, obs, alpha, alpha_action, alpha_obs, alpha_index){
  #inputs:
  # belief_state_momdp: vector, prior on partially observable variables length Num_mod
  # obs: last observed state integer 0 for low and 1 for high
  # alpha: alpha vectors as returned by read_policyx2
  # alpha_action: actions as returned by read_policyx2
  # alpha_obs: obsevations as returned by read_policyx2
  # alpha_obs: indexes as returned by read_policyx2

  # output: list of: value function, optimal action as integer, index of corresponding alpha vector

  id <- which(alpha_obs == obs)
  alpha2 <- alpha[,id]
  alpha_action2 <- alpha_action[id]
  alpha_index2 <- alpha_index[id]
  a <- belief_state_momdp %*% alpha2
  if (sum(a == 0) == length(a)) {
    output <- list(0, 1)
  }
  else {
    output <- list(max(a), alpha_action2[which.max(a)], alpha_index2[which.max(a)])
  }
  output
}

get_policy <- function(belief_state_momdp, alpha){
  #inputs:
  # belief_state_momdp: vector, prior on partially observable variables length Num_mod
  # alpha: output of read_policyx2

  #output: optimal policy as string "action_unhealthy - action_healthy"

  action_unhealthy <- interp_policy2(belief_state_momdp,
                                     obs = 1,
                                     alpha = alpha$vectors,
                                     alpha_action = alpha$action,
                                     alpha_obs = alpha$obs,
                                     alpha_index = alpha$index)[[2]]
  action_healthy <- interp_policy2(belief_state_momdp,
                                   obs = 2,
                                   alpha = alpha$vectors,
                                   alpha_action = alpha$action,
                                   alpha_obs = alpha$obs,
                                   alpha_index = alpha$index)[[2]]

  return(paste0(action_unhealthy,"-",action_healthy))
}
update_belief <- function(belief_state_momdp, transition, observation, z0, a0){
  #inputs:
  # belief_state_momdp: vector, prior on partially observable variables
  # transition: transition function, array of dim (X x Y, X x Y, A)
  # observation: observation function, array of dim (X x Y, X x Y, A)
  # z0: last observation
  # a0: last action

  # output: updated belief state
  L <- length(belief_state_momdp)
  belief <-
    vapply(seq_len(L), function(i){
      belief_state_momdp %*% transition[, i, a0] * observation[i, z0, a0]
    }, numeric(1))
  belief / sum(belief)
}

mdp_compute_value <- function(P, PR, discount){
  # P	:transition probability array. P can be a 3 dimensions array [S,S,A] or a list [[A]], each element containing a sparse matrix [S,S].
  #
  # PR :reward array. PR can be a 2 dimension array [S,A] possibly sparse.
  #
  # discount:discount factor in ]0; 1].
  # computes the optimal value function using the value iteration algorithm
  iter <- 0
  V <- c(0,0)
  is_done <- F
  epsilon <- 0.0001
  thresh <- epsilon * (1 - discount)/discount
  max_iter <- 50000
  while (!is_done) {
    iter <- iter + 1
    Vprev <- V
    bellman <- MDPtoolbox::mdp_bellman_operator(P, PR, discount, V)
    V <- bellman[[1]]
    policy <- bellman[[2]]
    variation <- max(V - Vprev)
    if (variation < thresh) {
      is_done <- T
    }
    else if (iter == max_iter) {
      is_done <- T
    }
  }
  return(list('V'=V, policy=policy))
}
