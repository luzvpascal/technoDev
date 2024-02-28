possible_models <- function(compact_transition_reefs_hmMDP_P1feasible,
                            compact_transition_reefs_hmMDP_P1infeasible,
                            compact_transition_P1){

  models_list <- list()

  #P1feasible
  for (index in seq(length(compact_transition_reefs_hmMDP_P1feasible[[1]]))){
    transition_model <- list(
      compact_transition_reefs_hmMDP_P1feasible[[1]][[index]],
      compact_transition_reefs_hmMDP_P1feasible[[2]][[index]]
    )

    model_index <- transition_MDP_full_info(transition_model,
                                            compact_transition_P1[[1]])
    models_list[[length(models_list) + 1]] <- model_index

  }

  #P1infeasible
  for (index in seq(length(compact_transition_reefs_hmMDP_P1infeasible[[1]]))){
    transition_model <- list(
      compact_transition_reefs_hmMDP_P1infeasible[[1]][[index]],
      compact_transition_reefs_hmMDP_P1infeasible[[2]][[index]]
    )

    model_index <- transition_MDP_full_info(transition_model,
                                            compact_transition_P1[[2]])
    models_list[[length(models_list) + 1]] <- model_index

  }


  return(models_list)
}

