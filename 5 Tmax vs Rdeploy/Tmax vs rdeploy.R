## TIME LIMIT TECH DEV ####
res <- data.frame()

## influence of initial belief ####
for (coeffDev_index in coeffs_costDev_heatmap[1]){
  print(coeffDev_index)
  costDev_P1_index <- coeffDev_index*V_max #might be even smaller
  for (value_noRD in coeffs_r_deploy){
  for (r_deploy in coeffs_r_deploy){
      ## setting the value of reward_POMDP####
      reward_POMDP <- matrix(c(value_noRD, value_noRD,
                               value_noRD-costDev_P1_index, r_deploy), ncol=2)

      ## Solving POMDP
      models <- solving_POMDP(prob_idle_idle,
                              initial_belief_state_P1,
                              reward_POMDP,
                              solve_hmMDP = solve_hmMDP,
                              file_pomdpx_index=file_pomdpx_techdev(costDev_P1_index,
                                                                    tr_low_low,
                                                                    tr_high_low,
                                                                    initial_belief_benef_low,
                                                                    initial_belief_benef_high),
                              file_outpolicy_index=file_outpolicy_techdev(costDev_P1_index,
                                                                          tr_low_low,
                                                                          tr_high_low,
                                                                          initial_belief_benef_low,
                                                                          initial_belief_benef_high))

      ## figure out Tmax ####
      models <-list(transition_function_P1(prob_idle_idle),
                    transition_function_P1(1))

      alphas <- read_policyx2(file_outpolicy_techdev(costDev_P1_index,
                                                     tr_low_low,
                                                     tr_high_low,
                                                     initial_belief_benef_low,
                                                     initial_belief_benef_high)) #alpha vectors

      #transition function momdp
      transition_momdp <- transition_hmMDP(models)
      #observation function momdp
      observation_momdp <- obs_hmMDP(models)

      #real dynamics
      tr_mdp_real <-  transition_function_P1(1)

      tab_ <- trajectory(state_prior=1,
                         Tmax = Tmax_SIM,
                         initial_belief_state = initial_belief_state_P1,
                         tr_mdp = tr_mdp_real,
                         rew_mdp= reward_reef,#dont care
                         tr_momdp = transition_momdp,
                         obs_momdp = observation_momdp,
                         alpha_momdp = alphas,
                         disc = gamma,
                         optimal_policy = TRUE,
                         naive_policy = NA)

      results <- tab_$data_output %>%
        mutate(invest=if_else(action >=2, 1,0)) %>%
        group_by(invest) %>%
        summarize(max_invest=max(max(time+1)*invest)) %>%
        select(-invest) %>%
        filter(max_invest==max(max_invest))

      ## analytical ####
      belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1_index,
                                              p_idle = prob_idle_idle,
                                              Rnord = value_noRD,
                                              Wready = r_deploy/(1-gamma),
                                              disc = gamma)

      Tmax_years <- max_years(initial_belief_state_P1[1],
                              prob_idle_idle,
                              belief_IS)

      res <- rbind(res, data.frame(value_noRD=value_noRD,
                                   r_deploy = r_deploy,
                                   max_invest=results$max_invest[1],
                                   max_invest_analytical = Tmax_years,
                                   costDev_P1_index=costDev_P1_index))
  }
  }
}
write.csv(res,
          results_tmax_vs_rdeploy_file, row.names = FALSE)

############################
## FIGURE ####
##############

## map TMAX#####
res <- read.csv(results_tmax_vs_rdeploy_file)

res <- res %>%
  mutate(cost = ifelse(costDev_P1_index==costDev_P1,
                       paste0(costDev_P1_index, ": GBR"),
                       as.character(costDev_P1_index))) %>%
  mutate(cost_fact= factor(costDev_P1_index,
                          levels=c('1e-04', '0.001', '0.01', '0.1'))) %>%
  filter(r_deploy-value_noRD >=-0.1
         )

tmax_vs_rdeploy <- ggplot(res,
     aes(x = (r_deploy-value_noRD+costDev_P1_index)
         ,color=cost_fact
         )) +
  geom_line(aes(y=max_invest_analytical,  linetype="Analytical approx."))+
  geom_line(aes(y=max_invest, linetype="Numerical"))+
  scale_linetype_manual(values = c("dashed", "solid"))+
  labs(x = TeX("$\\Delta_R =R_{dep} - (R_{BAU} - C_{dev})$"),
       y = TeX("$T_{max}$"),
       color=TeX("$C_{dev}$"),
       linetype="")+
  theme_bw()+
  theme(
    title=element_text(size=20),
    panel.grid = element_blank(),
    axis.text=element_text(size=20),
    axis.title = element_text(size=20),
    legend.text=element_text(size=20))  +
  lims(x=c(-0.1,0.8))+
  # scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_color_manual(values=c('dodgerblue1',
                            'dodgerblue3',
                            "dodgerblue4",
                            "darkblue"))

tmax_vs_rdeploy

ggsave(results_tmax_vs_rdeploy_figure,
       plot = tmax_vs_rdeploy, width = 10, height = 4, units = "in")

## SUPP INFO FIGURE ####
min_vals <- res %>%
  filter(costDev_P1_index<=costDev_P1) %>%
  group_by(costDev_P1_index)%>%
  filter(max_invest==0)%>%
  summarise(min_vals = max(r_deploy-value_noRD+costDev_P1_index))%>%
  mutate(max_invest=0)

tmax_vs_rdeploy_supp <- res %>%
  filter(costDev_P1_index<=costDev_P1) %>%
  ggplot(aes(x = r_deploy-value_noRD+costDev_P1_index,
        color=cost_fact)) +
  geom_line(aes(y=max_invest_analytical,  linetype="Analytical approx."))+
  geom_line(aes(y=max_invest, linetype="Numerical"))+
  scale_linetype_manual(values = c("dashed", "solid"))+
  labs(x = TeX("$\\Delta_R = R_{dep} - R_{BAU} + C_{dev}$"),
       y = TeX("$T_{max}$"),
       color=TeX("$C_{dev}/B_{h}$"),
       linetype="")+
  theme_bw()+
  theme(
    title=element_text(size=20),
    panel.grid = element_blank(),
    axis.text=element_text(size=20),
    axis.title = element_text(size=20),
    legend.text=element_text(size=20))  +
  scale_x_continuous(breaks = c(0,0.5,1)) +
  scale_color_manual(values=c('dodgerblue1',
                              'red',
                              "dodgerblue4",
                              "darkblue"))+
  lims(x=c(-0.001,0.01))+
  geom_point(data=min_vals, aes(x=min_vals, y=max_invest),
             shape = 23, size = 3, col="black", fill="black")+
  annotate("text", x=min_vals$min_vals-0.001,
           y=min_vals$max_invest+4,
           label= paste(min_vals$min_vals),
           size = 5,
           colour = "black")

tmax_vs_rdeploy_supp

ggsave(results_tmax_vs_rdeploy_supps_figure,
       plot = tmax_vs_rdeploy_supp, width = 10, height = 6, units = "in")
