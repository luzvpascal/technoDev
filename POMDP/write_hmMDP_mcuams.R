write_hmMDP <- function(TR_FUNCTION, B_FULL, B_PAR, REW, GAMMA, FILE){
  # TR_projects: list of arrays of size [s,s,a]
  # TR : list of parents states for the reefs, each element of the list
  # corresponds to another list of arrays of size
  # [s,s,a] that represents each model
  # REW: matrix of size [s,a]
  # B_FULL: vector, probability distribution over the fully observable states
  # B_PAR_REEFs:vector, probability distribtution over the non observable states (number of models for reefs)
  # B_PAR_P1:vector, probability distribtution over the non observable states (number of models for P1)
  # B_PAR_P2:vector, probability distribtution over the non observable states (number of models for P2)
  # GAMMA: number between 0 and 1, the discount factor
  # FILE: string, path to the pomdpx file
  Num_s <- dim(TR_FUNCTION[[1]])[1]
  Num_a <-dim(TR_FUNCTION[[1]])[3]
  Num_mod <- length(TR_FUNCTION)

  STATES <- paste0("obs_state", 1:Num_s)
  ACTIONS <- paste0("action", 1:Num_a)
  MODELS <- paste0("model", 1:Num_mod)

  #build header ####
  header <- paste0("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\n",
                   "<pomdpx version =\"1.0\" id=\"sample\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n      xsi:noNamespaceSchemaLocation=\"pomdpx.xsd\">\n\n",
                   "<Description>hmMDP model</Description>\n\n",
                   paste0("<Discount>",  GAMMA, "</Discount>"), "\n\n",
                   "<Variable>", "\n\n")

  header_state <- paste0("<StateVar vnamePrev=\"species_0\" vnameCurr=\"species_1\" fullyObs=\"true\">",
                              "\n", paste0("<ValueEnum>", paste0(STATES, collapse = " "),
                                           "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  header_model <- paste0("<StateVar vnamePrev=\"hidden_0\" vnameCurr=\"hidden_1\" fullyObs=\"false\">",
                              "\r\n", paste0("<ValueEnum>", paste0(MODELS, collapse =" "),"</ValueEnum>",
                                             "\r\n</StateVar>\r\n\n"))

  header_action <- paste0("<ActionVar vname=\"action_control\">\r\n",
                          "<ValueEnum>", paste0(ACTIONS, collapse = " "),
                          "</ValueEnum>\r\n</ActionVar>\r\n\n")

  header_obs_rew <- paste0("<ObsVar vname=\"obs\">\r\n<ValueEnum>o</ValueEnum>\r\n</ObsVar>\r\n\n",
                           " <RewardVar vname=\"reward_agent\" />\r\n</Variable>\r\n\n")

  header_belief <- paste0("<InitialStateBelief>\r\n\n",
                          "<CondProb>\r\n",
                          "<Var>species_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",

                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_FULL, collapse = " "), "</ProbTable>\n",
                          "</Entry>\r\n",

                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "<CondProb>\r\n",
                          "<Var>hidden_0 </Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_PAR, collapse = " "), "</ProbTable>\r\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "</InitialStateBelief>\r\n\n\n"
                          , sep = "")

  header <- paste0(header,
                   header_state,
                   header_model,
                   header_action,
                   header_obs_rew,
                   header_belief)

  # build transition matrices for the reefs####

  mod_tr_header = paste(
    "<StateTransitionFunction>\r\n\n",
    "<CondProb>\r\n",
    "<Var>species_1</Var>\r\n",
    "<Parent>action_control hidden_0 species_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")

  mod_tr_filling <- ""

  for (mod_id in seq(Num_mod)){
    model_text <- MODELS[mod_id]
    model_prob_values <- TR_FUNCTION[[mod_id]]
    mod_tr_filling_mod_id <- ""
    for (act_id in seq(Num_a)){
      action_text <- ACTIONS[act_id]
      action_prob_values <- model_prob_values[,,act_id]
      for (state0_id in seq(Num_s)){
        state0_text <- STATES[state0_id]
        for (state1_id in seq(Num_s)){
          state1_text <- STATES[state1_id]
          mod_tr_filling_mod_id <- paste(mod_tr_filling_mod_id,
                                         "<Entry>\r\n",
                                         "<Instance>",
                                         action_text, model_text, state0_text, state1_text,
                                         " </Instance>\r\n",
                                         "<ProbTable>",
                                         as.character(round(action_prob_values[state0_id,state1_id], digits = 4)),
                                         "</ProbTable>\r\n",
                                         "</Entry>\r\n")
        }
      }

    }
    mod_tr_filling <- paste(mod_tr_filling, "\r\n", mod_tr_filling_mod_id)
  }
  mod_tr_end <- paste(
    "</Parameter>\r\n",
    "</CondProb>\r\n\n")

  mod_tr <- paste(mod_tr_header, mod_tr_filling, mod_tr_end)


  ## transition between models ####
  state_tr_end <- paste("<CondProb>\r\n",
                                     "<Var>hidden_1</Var>\r\n",
                                     "<Parent>hidden_0</Parent>\r\n",
                                     "<Parameter type=\"TBL\">\r\n",
                                     "<Entry>\r\n<Instance> - - </Instance>\r\n",
                                     "<ProbTable>identity</ProbTable>\r\n",
                                     "</Entry>\r\n",
                                     "</Parameter>\r\n",
                                     "</CondProb>\r\n\n",
                        "</StateTransitionFunction>\r\n\n\n")


  state_tr <- paste(mod_tr, state_tr_end)
  # build div for observations ####

  obs_header <- "<ObsFunction>\r\n\n"
  obs_fill <-paste(
    "<CondProb>\r\n",
    "<Var>obs</Var>\r\n",
    "<Parent>hidden_1</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")
  for (mod_id in c(1:Num_mod)){
    model_text <- MODELS[mod_id]
    obs_fill_mod <- paste0(
      "<Entry>\r\n",
      "<Instance> ",
      model_text,
      " o</Instance>\r\n",
      "<ProbTable>1</ProbTable>\r\n",
      "</Entry>\r\n"
    )
    obs_fill <- paste(obs_fill, obs_fill_mod)
  }

  obs_end <-"</Parameter>\r\n</CondProb>\r\n</ObsFunction>\r\n\n\n"

  obs <- paste(obs_header, obs_fill, obs_end)


  # reward ####
  rew_header <- paste0(
    "<RewardFunction>\r\n",
    "<Func>\r\n",
    "<Var>reward_agent</Var>\r\n",
    "<Parent>action_control species_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n"
  )

  rew_fill <- paste0(
    "<Entry>\r\n",
    "<Instance> - - </Instance>\r\n",
    "<ValueTable>", paste0((REW), collapse = " "), "</ValueTable>\r\n",
    "</Entry>\r\n"
  )


  rew_end <- "</Parameter>\r\n
    </Func>\r\n
    </RewardFunction>\r\n\n\n"

  rew <- paste(rew_header, rew_fill, rew_end)

  end <- "</pomdpx>"

  # paste everything ####
  a = paste(header, state_tr, obs, rew, end, sep = '')
  writeLines(a,FILE)

}
