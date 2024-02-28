random_policy <- function(current_state, t){
  #naive policy: random action no matter the state
  return(sample(seq(4),1))
}

## helpfull functions ####
mdp_eval_policy <- function(P, R, discount, policy, Tmax){
  
  compute <- mdp_computePpolicyPRpolicy(P, R, policy)
  Ppolicy <- compute[[1]]
  PRpolicy <- compute[[2]]
  S <- dim(Ppolicy)[1]
  Vpolicy <-matrix(0, S, Tmax + 1)
  prob_matrix <- diag(S)
  Vpolicy[,2] <- (prob_matrix %*% PRpolicy)
  iter <- 2
  while (iter <= Tmax) {
    Vprev <- Vpolicy[, iter]
    prob_matrix <- prob_matrix %*% Ppolicy
    Vpolicy[, iter+1] <- Vprev + discount**(iter-1) * prob_matrix %*% PRpolicy
    iter <- iter + 1
  }
  return(Vpolicy)
}
