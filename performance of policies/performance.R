tr_low_low <- tr_nothing[1,1]
tr_high_low <- tr_nothing[2,1]
steadyStateLow <- -tr_high_low/(tr_low_low-1-tr_high_low)
steady_state <- c(steadyStateLow,1-steadyStateLow)
value
#read adaptive management solution
alphas <- read_policyx2(file_outpolicy_AM(costDev_P1,
                                          tr_low_low,
                                          tr_high_low,
                                          initial_belief_benef_low,
                                          initial_belief_benef_high))

##Models reefs####
data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1,
                                                      tr_low_low,
                                                      tr_high_low))

Num_mod <- nrow(data_mean_parameters)
models_list <- list()
for (mod_id in seq(Num_mod)){
  params <- unlist(c(data_mean_parameters[mod_id,-1]))
  model <- transition_from_parameters(params, N_actions=2)
  models_list[[mod_id]] <- model
}

#transition function momdp
transition_momdp <- transition_hmMDP(models_list)
#observation function momdp
observation_momdp <- obs_hmMDP(models_list)

## optimal adaptive management strategy ####
tab_ <- sim_mdp_momdp_policy_quadrants(steady_state=steady_state,
                                       Tmax=Tmax,
                                       initial_belief_state=initial_belief_benef,
                                       list_tr_mdp=models_list,
                                       rew_mdp=reward_reef+reward_P1_ready,
                                       tr_momdp=transition_momdp,
                                       obs_momdp=observation_momdp,
                                       alpha_momdp=alphas,
                                       disc = gamma,
                                       optimal_policy = TRUE,
                                       naive_policy = NA,
                                       n_it = 10000,
                                       average=FALSE,
                                       alpha_indexes=FALSE)
tab_$policy <- "POMDP"
## EVPI strategy ####
naive_policy_function <- function(...){
  return(2)
}
tab_2 <- sim_mdp_momdp_policy_quadrants(steady_state=steady_state,
                                       Tmax=Tmax,
                                       initial_belief_state=initial_belief_benef,
                                       list_tr_mdp=models_list,
                                       rew_mdp=reward_reef+reward_P1_ready,
                                       tr_momdp=transition_momdp,
                                       obs_momdp=observation_momdp,
                                       alpha_momdp=alphas,
                                       disc = gamma,
                                       optimal_policy = FALSE,
                                       naive_policy = naive_policy_function,
                                       n_it = 10000,
                                       average=FALSE,
                                       alpha_indexes=FALSE)
tab_2$policy <- "EVPI"

## combine results####
results <- rbind(tab_, tab_2)

## plot steady state ####
res_uncertainty <- results %>%
  filter(time<=Tmax)%>%
  mutate(propHealthy=ifelse(state==2, 1, 0))%>%
  group_by(time, policy)%>%
  summarise(prop=mean(propHealthy),
            nobs=n(),
            stdErr=sqrt(prop*(1-prop)/nobs),
            lower_bound=prop-1.96*stdErr,
            upper_bound=prop+1.96*stdErr
  )
res_uncertainty <- rbind(res_uncertainty,
                         data.frame(time =c(0,Tmax),
                                    policy="BAU",
                                    prop= 1-steadyStateLow,
                                    nobs=1,
                                    stdErr=0,
                                    lower_bound=1-steadyStateLow,
                                    upper_bound=1-steadyStateLow))
res_uncertainty %>%
  ggplot(aes(x=time, y=prop, group=policy))+
  geom_line(aes(col=policy))+
  theme_bw() +
  # lims(y=c(0,1))+
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound,
                  fill=policy, alpha=0.3))

## plot value ####
res_uncertainty <- results %>%
  filter(time<=Tmax)%>%
  group_by(time, policy)%>%
  summarise(mean_value=mean(value),
            nobs=n(),
            stdErr=sd(value),
            lower_bound=mean_value-1.96*stdErr,
            upper_bound=mean_value+1.96*stdErr
  )
data_BAU <- data.frame(time=seq(0,Tmax),
                       policy="BAU",
                       nobs=1,
                       stdErr=0,
                       lower_bound=1-steadyStateLow,
                       upper_bound=1-steadyStateLow)
data_BAU <- data_BAU%>%
  mutate(mean_value=)

res_uncertainty <- rbind(res_uncertainty,
                         data_BAU))

res_uncertainty %>%
  ggplot(aes(x=time, y=mean_value, group=policy))+
  geom_line(aes(col=policy))+
  theme_bw()
