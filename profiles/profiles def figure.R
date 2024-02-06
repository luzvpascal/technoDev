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

