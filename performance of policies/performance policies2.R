## solve POMDP
models <- solving_POMDP(compute_mean_params,
                        tr_nothing,
                        sd_tr_nothing,
                        gamma,
                        reward_reef,
                        reward_P1_idle,
                        reward_P1_ready,
                        reward_P2_idle,
                        reward_P2_ready,
                        reward_MDP,
                        solve_hmMDP)
alphas <- read_policyx2(file_outpolicy)

## SIMULATIONS ####
## build transition matrices for hmMDP####
#transition momdp
transition_momdp <- transition_hmMDP(models$all_models)
#observation function momdp
observation_momdp <- obs_hmMDP(models$all_models)

## Best non adaptive policy issued from the rEVPI analysis####
best_policy_non_adaptive <- read.csv(best_policy_file)
best_policy_non_adaptive <- best_policy_non_adaptive[
  which(best_policy_non_adaptive$coeffDev==coeffDev &
          best_policy_non_adaptive$coeffDeploy==coeffDeploy),]$best_policy

best_policy_non_adaptive <- as.numeric(unlist(strsplit(best_policy_non_adaptive," ")))

## run simulations ####
set.seed(2013)
mdp_genric <- 0

if (run_simulations){
  # for (mdp_id in seq(N_tests)){
  for (mdp_id in seq(4,10)){
    print(mdp_id)
    ## build real transition function for MDPs ####
    #reefs
    tr_reefsP1only_real <- random_transition(N_actions = 1,
                                             N_states = 2)
    tr_reefsP2only_real <- random_transition(N_actions = 1,
                                             N_states = 2)
    tr_reefsP1P2_real <- random_transition(N_actions = 1,
                                           N_states = 2)

    ## P1 and P2 feasibility ####
    for (feasible_P1 in c(TRUE, FALSE)){
      for (feasible_P2 in c(TRUE, FALSE)){
        if (feasible_P1|feasible_P2){
          mdp_genric <- mdp_genric +1
          #adapt the transition function according to the real feasability of P1 and P2
          tr_mdp_reefs_real <- transition_reefs(tr_nothing, tr_reefsP1only_real,
                                                tr_reefsP2only_real, tr_reefsP1P2_real,
                                                feasible_P1, feasible_P2)
          tr_P1_real <- random_transition_projects(ProjectIndex = 1, feasible = feasible_P1)
          tr_P2_real <- random_transition_projects(ProjectIndex = 2, feasible = feasible_P2)

          tr_mdp_real <- transition_MDP_full_info(tr_mdp_reefs_real, tr_P1_real,tr_P2_real)
          ## simulation with hmMDP ####
          tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                       Tmax = Tmax,
                                       initial_belief_state = models$initial_belief,
                                       tr_mdp = tr_mdp_real,
                                       rew_mdp= reward_MDP,
                                       tr_momdp = transition_momdp,
                                       obs_momdp = observation_momdp,
                                       alpha_momdp = alphas,
                                       disc = gamma,
                                       optimal_policy = TRUE,
                                       naive_policy = NA,
                                       n_it = N_sim,
                                       average=average_results)

          if (average_results){
            data_now <- data.frame(value=tab_,
                                   time=seq(0, Tmax),
                                   feasible_P1 = feasible_P1,
                                   feasible_P2 = feasible_P2,
                                   mdp_genric=mdp_genric,
                                   method="MOMDP")
          } else {
            data_now <- tab_
            data_now$feasible_P1 <- feasible_P1
            data_now$feasible_P2 <- feasible_P2
            data_now$mdp_id <- mdp_id
            data_now$method="MOMDP"
          }

          write.table(data_now, results_file, sep = ",",
                      append = TRUE, row.names = FALSE, col.names = FALSE)

          ## simulations with naive policies: NON ADAPTIVE ####
          if (run_naive){
            if (average_results){
              values <- mdp_eval_policy(tr_mdp_real, reward_MDP, gamma,best_policy_non_adaptive, Tmax)

              data_now <- data.frame(value=values[1,],
                                     time=seq(0, Tmax),
                                     feasible_P1 = feasible_P1,
                                     feasible_P2 = feasible_P2,
                                     mdp_genric=mdp_genric,
                                     method="non-adaptive")
            } else {
              mdp_policy <- function(current_state, t){return(best_policy_non_adaptive[current_state])}

              tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                           Tmax = Tmax,
                                           initial_belief_state = models$initial_belief,
                                           tr_mdp = tr_mdp_real,
                                           rew_mdp= reward_MDP,
                                           tr_momdp = transition_momdp,
                                           obs_momdp = observation_momdp,
                                           alpha_momdp = alphas,
                                           disc = gamma,
                                           n_it = N_sim,
                                           optimal_policy = FALSE,
                                           naive_policy = mdp_policy,
                                           average=average_results)

              data_now <- tab_
              data_now$feasible_P1 <- feasible_P1
              data_now$feasible_P2 <- feasible_P2
              data_now$mdp_id <- mdp_id
              data_now$method="non-adaptive"
            }


            write.table(data_now, results_file, sep = ",",
                        append = TRUE, row.names = FALSE, col.names = FALSE)
          }
          ## simulations with random actions ####
          if (run_random){
            tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                         Tmax = Tmax,
                                         initial_belief_state = models$initial_belief,
                                         tr_mdp = tr_mdp_real,
                                         rew_mdp= reward_MDP,
                                         tr_momdp = transition_momdp,
                                         obs_momdp = observation_momdp,
                                         alpha_momdp = alphas,
                                         disc = gamma,
                                         n_it = N_sim,
                                         optimal_policy = FALSE,
                                         naive_policy = random_policy,
                                         average=average_results)

            if (average_results){
              data_now <- data.frame(value=tab_,
                                     time=seq(0, Tmax),
                                     feasible_P1 = feasible_P1,
                                     feasible_P2 = feasible_P2,
                                     mdp_genric=mdp_genric,
                                     method="random actions")
            } else {
              data_now <- tab_
              data_now$feasible_P1 <- feasible_P1
              data_now$feasible_P2 <- feasible_P2
              data_now$mdp_id <- mdp_id
              data_now$method="random actions"
            }
            write.table(data_now, results_file, sep = ",",
                        append = TRUE, row.names = FALSE, col.names = FALSE)
          }
          ## optimal solution ####
          if (run_optimal){
            mdp_sol<-mdp_finite_horizon(tr_mdp_real, reward_MDP, gamma, Tmax+1)
            if (average_results){
              V_opt <- mdp_eval_policy(tr_mdp_real, reward_MDP, gamma, mdp_sol$policy[,1],Tmax)
              V_opt <- V_opt[1,]
              data_now <- data.frame(value=V_opt,
                                     time=seq(0, Tmax),
                                     feasible_P1 = feasible_P1,
                                     feasible_P2 = feasible_P2,
                                     mdp_genric=mdp_genric,
                                     method="optimal")
            } else {
              mdp_policy <- function(current_state, t){return(mdp_sol$policy[current_state, t])}

              tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                           Tmax = Tmax,
                                           initial_belief_state = models$initial_belief,
                                           tr_mdp = tr_mdp_real,
                                           rew_mdp= reward_MDP,
                                           tr_momdp = transition_momdp,
                                           obs_momdp = observation_momdp,
                                           alpha_momdp = alphas,
                                           disc = gamma,
                                           n_it = N_sim,
                                           optimal_policy = FALSE,
                                           naive_policy = mdp_policy,
                                           average=average_results)

              data_now <- tab_
              data_now$feasible_P1 <- feasible_P1
              data_now$feasible_P2 <- feasible_P2
              data_now$mdp_id <- mdp_id
              data_now$method="optimal"
            }

            write.table(data_now, results_file,
                        sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
          }
        }
      }
    }
  }
}


## PERFORMANCE ON THE DERIVED MODELS ####
N_sim <- 1000
results_file_models_known <- "results_performance_25mods_models_known.csv"
for (mdp_genric in seq(length(models$all_models))){
    print(mdp_genric)
    tr_mdp_real <- models$all_models[[mdp_genric]]
    ## simulation with hmMDP ####
    tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                 Tmax = Tmax,
                                 initial_belief_state = models$initial_belief,
                                 tr_mdp = tr_mdp_real,
                                 rew_mdp= reward_MDP,
                                 tr_momdp = transition_momdp,
                                 obs_momdp = observation_momdp,
                                 alpha_momdp = alphas,
                                 disc = gamma,
                                 optimal_policy = TRUE,
                                 naive_policy = NA,
                                 n_it = N_sim,
                                 average=average_results)

    if (average_results){
      data_now <- data.frame(value=tab_,
                             time=seq(0, Tmax),
                             mdp_genric=mdp_genric,
                             method="MOMDP")
    } else {
      data_now <- tab_
      data_now$mdp_genric <- mdp_genric
      data_now$method="MOMDP"
    }

    write.table(data_now, results_file_models_known, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)

    ## simulations with naive policies: NON ADAPTIVE ####
    if (run_naive){
      if (average_results){
        values <- mdp_eval_policy(tr_mdp_real, reward_MDP, gamma,best_policy_non_adaptive, Tmax)

        data_now <- data.frame(value=values[1,],
                               time=seq(0, Tmax),
                               mdp_genric=mdp_genric,
                               method="non-adaptive")
      } else {
        mdp_policy <- function(current_state, t){return(best_policy_non_adaptive[current_state])}

        tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                     Tmax = Tmax,
                                     initial_belief_state = models$initial_belief,
                                     tr_mdp = tr_mdp_real,
                                     rew_mdp= reward_MDP,
                                     tr_momdp = transition_momdp,
                                     obs_momdp = observation_momdp,
                                     alpha_momdp = alphas,
                                     disc = gamma,
                                     n_it = N_sim,
                                     optimal_policy = FALSE,
                                     naive_policy = mdp_policy,
                                     average=average_results)

        data_now <- tab_
        data_now$mdp_genric <- mdp_genric
        data_now$method="non-adaptive"
      }


      write.table(data_now, results_file_models_known, sep = ",",
                  append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    ## simulations with random actions ####
    if (run_random){
      tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                   Tmax = Tmax,
                                   initial_belief_state = models$initial_belief,
                                   tr_mdp = tr_mdp_real,
                                   rew_mdp= reward_MDP,
                                   tr_momdp = transition_momdp,
                                   obs_momdp = observation_momdp,
                                   alpha_momdp = alphas,
                                   disc = gamma,
                                   n_it = N_sim,
                                   optimal_policy = FALSE,
                                   naive_policy = random_policy,
                                   average=average_results)

      if (average_results){
        data_now <- data.frame(value=tab_,
                               time=seq(0, Tmax),
                               mdp_genric=mdp_genric,
                               method="random actions")
      } else {
        data_now <- tab_
        data_now$mdp_genric <- mdp_genric
        data_now$method="random actions"
      }
      write.table(data_now, results_file_models_known, sep = ",",
                  append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    ## optimal solution ####
    if (run_optimal){
      mdp_sol<-mdp_finite_horizon(tr_mdp_real, reward_MDP, gamma, Tmax+1)
      if (average_results){
        V_opt <- mdp_eval_policy(tr_mdp_real, reward_MDP, gamma, mdp_sol$policy[,1],Tmax)
        V_opt <- V_opt[1,]
        data_now <- data.frame(value=V_opt,
                               time=seq(0, Tmax),
                               mdp_genric=mdp_genric,
                               method="optimal")
      } else {
        mdp_policy <- function(current_state, t){return(mdp_sol$policy[current_state, t])}

        tab_ <- sim_mdp_momdp_policy(state_prior= 1,
                                     Tmax = Tmax,
                                     initial_belief_state = models$initial_belief,
                                     tr_mdp = tr_mdp_real,
                                     rew_mdp= reward_MDP,
                                     tr_momdp = transition_momdp,
                                     obs_momdp = observation_momdp,
                                     alpha_momdp = alphas,
                                     disc = gamma,
                                     n_it = N_sim,
                                     optimal_policy = FALSE,
                                     naive_policy = mdp_policy,
                                     average=average_results)

        data_now <- tab_
        data_now$mdp_id <- mdp_id
        data_now$method="optimal"
      }

      write.table(data_now, results_file_models_known,
                  sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  }
