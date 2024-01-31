### graphical representation####
library(latex2exp)
results <- read.csv(graph_maxTime_file,
                    header=FALSE)

names(results) <- c("tr_low_low",
                     "tr_high_low",
                     "belief",
                     "max_invest",
                     "cost",
                     "coeffDeploy_index")
results_graph <- results %>%
  filter(cost %in% c(1e-4,1e-3,1e-2,1e-1)) %>%
  arrange(cost, belief) %>%
  mutate(cost = ifelse(cost==costDev_P1, paste0(cost, " : Great Barrier Reef"), as.character(cost))) %>%
  mutate(cost_fact= factor(cost,
                           levels=c('1e-04', '0.001 : Great Barrier Reef', '0.01', '0.1')))

graph_maxTime <- ggplot(results_graph,
                        aes(x=belief, y=max_invest,
                            group=cost_fact,
                            fill=cost_fact))+
  geom_line(aes(color=cost_fact))+
  geom_point(aes(color=cost_fact),shape = 15, size = 3) +
  scale_color_manual(values=c('dodgerblue1',
                              'red',
                              "dodgerblue4",
                              "darkblue"))+
  labs(x = "Initial belief in project feasibility (log-scale)",
       y = TeX("$T_{max}$: time limit for project development"),
       col = ("Relative cost\nof development"),
       fill=("Relative cost\nof development"))+
  theme_minimal()+
  scale_x_log10()+
  theme(text = element_text(size = 16))

##load analytical solutions ####
analytical <- read.csv(graph_maxTime_file_analytical)
analytical <- analytical%>%
  mutate(cost = ifelse(coeffDev_index==costDev_P1,
                       paste0(coeffDev_index, " : Great Barrier Reef"),
                       as.character(coeffDev_index))) %>%
  mutate(cost_fact= factor(cost,
                           levels=c('1e-04', '0.001 : Great Barrier Reef', '0.01', '0.1')))

graph_maxTime <- graph_maxTime +
  geom_line(data=analytical,
            aes(x=belief, y=Tmax_years,group=cost_fact
                , color=cost_fact
                )
            , linetype="dashed"
            )

print(graph_maxTime)
ggsave(graph_maxTime_figure,
       plot = graph_maxTime, width = 10, height = 6, units = "in")
