# here we fix initial belief in project development
# we fix the transitions under BAU
# fix the costs of development and deployment
# we vary the initial belief that the project will be beneficial when low and high
#we determine the max number of time steps investing in project dev before we give up

## preset transition matrix if do nothing ####
profile_possibilities <- c(
  "A fast degradation\nfast recovery",
  "B fast degradation\nslow recovery",
  "C slow degradation\nfast recovery",
  "D slow degradation\nslow recovery"
)

for (profile in profile_possibilities){
  if (profile == "B fast degradation\nslow recovery"){
    tr_low_low <- 0.9
    tr_high_low <- 0.9
  } else if (profile=="A fast degradation\nfast recovery"){
    tr_low_low <- 0.1
    tr_high_low <- 0.9
  } else if (profile=="D slow degradation\nslow recovery"){
    tr_low_low <- 0.9
    tr_high_low <- 0.1
  } else if (profile=="C slow degradation\nfast recovery"){
    tr_low_low <- 0.1
    tr_high_low <- 0.1
  }

  alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,0.9,0.9)) #alpha vectors
  ## influence of initial belief ####
  data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
  sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
  result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy, alpha = alphas_AM)

  combination_beliefs_index <- combination_beliefs
  combination_beliefs_index$policy <- result
  combination_beliefs_index$profile <- profile

  write.table(combination_beliefs_index, profiles_definition_policy_file, sep = ",",
              append = TRUE, row.names = FALSE, col.names = FALSE)
}
