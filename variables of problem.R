## MDP definition ####
#factored states : reef = bad/good
#technology P1: idle/ongoing
#actions: nothing actP1

#transitions: unknown for the reefs, except for the action NOTHING
#known for P1 and P2
gamma <- 0.9 #discount factor

#reward
V_max <- 1 # the reef is worth 1 trillion dol
V_min <- 0.5*V_max
coeffDev <- 0.001
coeffDeploy <- 5

costDev_P1 <- coeffDev*V_max #might be even smaller

costImp_P1 <- coeffDeploy*costDev_P1 #getting massive boats, build labs
#reward for the reefs depending on development P1 and P2
#line 1: reefs bad, line 2: reefs good
#columns: nothing actP1 actP2 actP1P2
reward_reef <- matrix(c(V_min,V_max,V_min,V_max), nrow=2)
reward_P1_idle <- matrix(c(0,0,-costDev_P1,-costDev_P1), nrow=2)
reward_P1_ready <- matrix(c(0,0,-costImp_P1,-costImp_P1), nrow=2)
## reward function ####
rew_mdp_P1 <- matrix(c(0,0,-costDev_P1,-costImp_P1), nrow = 2)

reward_MDP <- reward_MDP_full_info(reward_reef, rew_mdp_P1)

## specifications technologies ####
prob_ready <- 0.9

##initial beliefs####
initial_belief_state_P1 <- rep(1/2,2) #initial belief tech feasible

initial_belief_benef_low <- 0.8 #initial belief tech beneficial when low
initial_belief_benef_high <- 0.8 #initial belief tech beneficial when high
initial_belief_benef <- c( #initial belief tech beneficial
  (1-initial_belief_benef_low)*(1-initial_belief_benef_high), ##initial belief do nothing better than tech
  (1-initial_belief_benef_low)*(initial_belief_benef_high),##initial belief tech better than do nothing high
  (initial_belief_benef_low)*(1-initial_belief_benef_high), ##initial belief tech better than do nothing low
  (initial_belief_benef_low)*(initial_belief_benef_high)##initial belief tech better than do nothing
)


##reward file name####
reward_ready_file <- paste0("res/rewards/reward_",coeffDev,".csv") #for MCUAMS
write.table(reward_reef+matrix(c(0,0,-costImp_P1,-costImp_P1), ncol=2),
            reward_ready_file, col.names = FALSE,
            row.names = FALSE, sep = ",")

## transitions reef ####
tr_nothing <- matrix(c(0.8, 0.2, 0.8, 0.2), ncol=2, byrow = TRUE)

#solve hmMDP parameters ####
solve_hmMDP <- TRUE
timeout <- 500 #set timeout for solving
precision <- 1e-2

runMCUAMS <- TRUE #change to run MC UAMS

## performance simulations parameters ####
run_simulations <- TRUE
average_results <- TRUE #boolean indicating if the simulated trajectories should be averaged or not
results_performance_file <- paste("res/performance/results_performance.csv", sep="")
results_figure <- paste("res/figures paper/performance/",tr_nothing[1],tr_nothing[2],
                        "results_performance.csv", sep="")

run_naive <- TRUE #boolean to run naive simulations (non adaptive)
run_random <- TRUE #boolean to run naive policy (random actions)
run_optimal <- TRUE #boolean to run naive simulations
N_tests <- 100 #number of real transitions policy is tested on
N_sim <- 10000 #number of simulations per instance
Tmax <- 30 #number of time steps considered in the simulations

###################
## RD vs no RD?####
###################

# coeffs_tr_low_low <- seq(0,1, length.out=101)
# coeffs_tr_high_low <- seq(0,1, length.out=101)
coeffs_tr_low_low <- seq(0,1, length.out=51)
coeffs_tr_high_low <- seq(0,1, length.out=51)
heatmap_valueNoRD_valueRD_varying_transitions_file <- paste("res/RDnoRD/",coeffDev,"cost",
                                                       prob_ready,"belief","valueNoRD_valueRD.csv", sep="")
heatmap_valueNoRD_valueRD_varying_transitions_figure <- paste("res/figures paper/RDnoRD/",coeffDev,"cost",
                                                    prob_ready,"belief","valueNoRD_valueRD.svg", sep="")

heatmap_valueNoRD_valueRD_varying_transitions_file_analytical <- paste("res/RDnoRD/",coeffDev,"cost",
                                                            prob_ready,"belief","valueNoRD_valueRD_analytical.csv", sep="")
heatmap_valueNoRD_valueRD_varying_transitions_figure_analytical <- paste("res/figures paper/RDnoRD/",coeffDev,"cost",
                                                              prob_ready,"belief","valueNoRD_valueRD_analytical.svg", sep="")

######################################################################################################
## interpreting policy parameters: max time investments varying costs and initial belief feasible ####
######################################################################################################
belief_tests <- c(seq(0.001,0.009,0.001),seq(0.01,0.1,0.01), seq(0.2,0.9,0.1), 0.95)
Tmax_SIM <- 1000 #number of time steps considered in the simulations
coeffs_costDev_heatmap <- c(0.1,0.05, 0.025, 0.01, 0.005,0.0025, 0.001, 0.0005,0.00025, 0.0001)
coeffs_costDep_heatmap <- c(5)
graph_maxTime_file <- paste("res/stop dev/",tr_nothing[1],tr_nothing[2],"graph MaxTime Costs Init Belief Varying.csv", sep="")
graph_maxTime_figure<- paste("res/figures paper/stop dev/",tr_nothing[1],tr_nothing[2],"graph MaxTime Costs Init Belief Varying.svg", sep="")

graph_maxTime_file_analytical <- paste("res/stop dev/",tr_nothing[1],tr_nothing[2],"graph MaxTime Costs Init Belief Varying_analytical.csv", sep="")
###########################################################################################################
##interpreting policy parameters: max time investments fixed costs, fixed beliefs, varying transitions ####
###########################################################################################################
heatmap_maxTime_file_varying_transitions <- paste("res/stop dev/",coeffDev,"cost",0.5,"belief","heatmapMaxTime.csv", sep="")
heatmap_maxTime_figure_varying_transitions <- paste("res/figures paper/stop dev/",coeffDev,"cost",0.5,"belief","heatmapMaxTime.svg", sep="")

heatmap_maxTime_file_varying_transitions_analytical <- paste("res/stop dev/",coeffDev,"cost",0.5,"belief","heatmapMaxTime analytical.csv", sep="")
heatmap_maxTime_figure_varying_transitions_analytical <-  paste("res/figures paper/stop dev/",coeffDev,"cost",0.5,"belief","heatmapMaxTime analytical.svg", sep="")

###############################################################################################
## interpreting policy parameters: max time investments, Expected values of time develoment####
###############################################################################################
prob_ready_tests <- c(seq(0.2,0.9,0.1), 0.95,0.98)
expected_time_Tmax_file <-  paste("res/stop dev/",coeffDev,"cost", 0.5,"belief",tr_nothing[1],tr_nothing[2],"graph MaxTime expected time varying.csv", sep="")
analytical_expected_time_Tmax_file <-  paste("res/stop dev/",coeffDev,"cost", 0.5,"belief",tr_nothing[1],tr_nothing[2],"graph MaxTime expected time varying.csv", sep="")


############################################################
## interpreting policy parameters: identifying profiles ####
############################################################
profiles_definition_file <- paste("res/profiles/",coeffDev,"cost", 0.5,"belief","_profiles.csv", sep="")
profiles_definition_figure <- paste("res/figures paper/profiles/",coeffDev,"cost", 0.5,"belief","_profiles.svg", sep="")


combination_beliefs <- expand.grid(seq(0,1,length.out=101),seq(0,1,length.out=101))
names(combination_beliefs) <- c("belief_benef_low","belief_benef_high")
sample_tested_beliefs <- combination_beliefs %>%
  mutate(bA1A1 = (1-belief_benef_low)*(1-belief_benef_high),
         bA1A2 = (1-belief_benef_low)*(belief_benef_high),
         bA2A1 = (belief_benef_low)*(1-belief_benef_high),
         bA2A2 = (belief_benef_low)*(belief_benef_high))
sample_tested_beliefs <- unname(as.matrix(sample_tested_beliefs[-c(1,2)]))

##########################################################################################
## interpreting policy parameters: influence of initial belief beneifts techno on Tmax####
###############################################################################################
coeffs_initial_belief_benefits_low <- seq(0,1, length.out=51)
coeffs_initial_belief_benefits_high <- seq(0,1, length.out=51)

heatmap_maxTime_file_varying_priors <- paste("res/initial belief benefits/",coeffDev,"cost",0.5,"belief","heatmapMaxTime preset priors.csv", sep="")
heatmap_maxTime_figure_varying_priors <- paste("res/figures paper/initial belief benefits/",coeffDev,"cost",0.5,"belief","heatmapMaxTime preset priors.svg", sep="")

AM_strategy_varying_priors_file <- paste("res/initial belief benefits/",coeffDev,"cost", 0.5,"belief","_AM_strategy.csv", sep="")
AM_strategy_varying_priors_figure <- paste("res/figures paper/initial belief benefits/",coeffDev,"cost", 0.5,"belief","_AM_strategy.svg", sep="")

heatmap_maxTime_overlap_AM_strategy_figure_varying_priors <- paste("res/figures paper/initial belief benefits/",
                                                                   coeffDev,"cost",0.5,"belief","heatmapMaxTime overlap preset priors.svg", sep="")

##single example at 0.5 0.5
AM_strategy_varying_priors_example_figure <- paste("res/figures paper/initial belief benefits/",
                                           coeffDev,"cost", 0.5,"belief","_AM_strategy_example.svg", sep="")

expected_value_varying_priors_example_figure <- paste("res/figures paper/initial belief benefits/",
                                                   coeffDev,"cost", 0.5,"belief","_expected_value_example.svg", sep="")

heatmap_maxTime_varying_priors_example_figure <- paste("res/figures paper/initial belief benefits/",
                                                      coeffDev,"cost", 0.5,"belief","_heatmapMaxTime_example.svg", sep="")
#analytical
AM_strategy_varying_priors_example_figure_analytical <- paste("res/figures paper/initial belief benefits/",
                                                   coeffDev,"cost", 0.5,"belief","_AM_strategy_example_analytical.svg", sep="")
expected_value_varying_priors_example_figure_analytical <- paste("res/figures paper/initial belief benefits/",
                                                      coeffDev,"cost", 0.5,"belief","_expected_value_example_analytical.svg", sep="")

heatmap_maxTime_varying_priors_example_figure_analytical <- paste("res/figures paper/initial belief benefits/",
                                                       coeffDev,"cost", 0.5,"belief","_heatmapMaxTime_example_analytical.svg", sep="")
