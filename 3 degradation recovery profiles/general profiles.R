#here we aim to identify the frontiers of profiles of ecosystems:
#we aim to analyse the importance of the initial belief in benefits of technology deployment
#for the ecoystem

for (tr_low_low in seq(0.1,0.9,length.out=9)){

  for (tr_high_low in seq(0.1,0.9,length.out=9)){
    ##steady states####
    tr_nothing_index <- transition_from_parameters(c(tr_low_low,
                                                     tr_high_low),
                                                   N_actions=1)[,,1]
    ## solving AM ####
    solving_AM(compute_mean_params=runMCUAMS,
               tr_nothing_index,
               reward_ready_file,
               output_meanPars_file = output_meanPars_file(costDev_P1, tr_low_low,tr_high_low),
               output_priors_file=output_priors_file(costDev_P1, tr_low_low,tr_high_low),
               solve_hmMDP = solve_hmMDP,
               file_pomdpx_index=file_pomdpx_AM(costDev_P1, tr_low_low,tr_high_low, 0.5, 0.5),
               file_outpolicy_index=file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low, 0.5, 0.5),
               initial_belief_preset=rep(0.25,4)
    )

    alphas_AM <- read_policyx2(file_outpolicy_AM(costDev_P1, tr_low_low,tr_high_low,0.5,0.5))

    ## sampling benef beliefs
    #adapt sampled tests
    data_mean_parameters <- read.csv(output_meanPars_file(costDev_P1, tr_low_low,tr_high_low))
    sample_tested_beliefs_index <- sample_tested_beliefs[,data_mean_parameters$opt]
    result <- apply(sample_tested_beliefs_index, MARGIN = 1, FUN = get_policy, alpha = alphas_AM)

    result <- as.data.frame(table(result))
    result <- result%>%
      mutate(tr_low_low=tr_low_low,
             tr_high_low=tr_high_low)

    write.table(result, profiles_definition_file, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)
  }
}


res <- read.csv(profiles_definition_file, sep=",",
                header=FALSE)

names(res) <- c("policy", "Freq","tr_low_low","tr_high_low")
res <- res %>%
  group_by(tr_low_low,tr_high_low)%>%
  summarise(max_freq=max(Freq)/sum(Freq),
            max_policy=policy[which.max(Freq)])%>%
  mutate(max_policy = case_when(
    max_policy == "1-Jan" ~ "Never deploy",
    max_policy == "1-Feb" ~ "Deploy healthy",
    max_policy == "2-Jan" ~ "Deploy unhealthy",
    max_policy == "2-Feb" ~ "Always deploy"
  ))

##figure####
dominant_policy_plot <- ggplot()+
  geom_tile(data= filter(res, max_policy == "Never deploy"),
            aes(x = tr_low_low, y = tr_high_low, fill = max_freq))+
  scale_fill_gradient(low = "lightpink", high = "red", limits = c(0.28,1))+
  labs(fill=("Never\ndeploy"))+
  new_scale_fill() +

  geom_tile(data= filter(res, max_policy == "Deploy unhealthy"),
            aes(x = tr_low_low, y = tr_high_low, fill = max_freq))+
  scale_fill_gradient(low = "lightyellow", high = "orange", limits = c(0.28,1))+
  labs(fill=TeX("Deploy\nunhealthy"))+
  new_scale_fill() +

  geom_tile(data= filter(res, max_policy == "Deploy healthy"),
            aes(x = tr_low_low, y = tr_high_low, fill = max_freq))+
  scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0.28,1))+
  labs(fill=TeX("Deploy\nhealthy"))+
  new_scale_fill() +

  geom_tile(data= filter(res, max_policy == "Always deploy"),
            aes(x = tr_low_low, y = tr_high_low, fill = max_freq))+
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", limits = c(0.28,1))+
  labs(fill=TeX("Always\ndeploy"))+
  theme_bw() +
  theme(
    panel.grid = element_blank()
    # , legend.position = "bottom"
    , legend.box = "horizontal"   # Set legend box to horizontal
  ) +
  coord_equal()+
  labs(
    x = "Difficulty of recovery\n Pr(unhealthy| unhealthy, BAU)",
    y = "Degradation rate\n Pr(unhealthy| healthy, BAU)"
  )+
  geom_point(data = data.frame(x = c(0.1, 0.9, 0.1,0.9), y = c(0.1, 0.1, 0.9,0.9),
                               policy_max="black"),
             aes(x = x, y = y), color = "black", size = 3)

ggsave(profiles_definition_figure,
       plot = dominant_policy_plot, width = 10, height = 6, units = "in")


