setwd("~/baseline program/code clean/")
# results_file <- "results_performance_25mods.csv"
results_file <- "results_performance_25mods_models_known.csv"

## PLOTS SIMULATIONS ####
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

data_simulations <- read_csv(results_file, col_names = FALSE)
average_results <- TRUE
if (average_results){
  data_averaged <- data_simulations
  if(results_file == "results_performance_25mods_models_known.csv"){
    names(data_averaged) <- c("value", "time", "mdp_id","method")
    labels_feasibility <- c(rep("3 both projects feasible", 16),
                            rep("2 at least one project feasible", 8),
                            "1 no project feasible")
    data_averaged$project_feasibility <- labels_feasibility[data_averaged$mdp_id]
  } else {
    names(data_averaged) <- c("value", "time","feasible_P1", "feasible_P2", "mdp_id","method")
    labels_feasibility <- c("1 no project feasible", "2 at least one project feasible",
                            "3 both projects feasible")
    data_averaged <- data_averaged%>%mutate(project_feasibility = labels_feasibility[feasible_P1+feasible_P2+1])
  }
} else {
  names(data_simulations) <- c("column.label", "state_reefs", "state_P1",
                               "state_P2", "value", "action", "time",
                               "feasible_P1", "feasible_P2", "mdp_id","method")
  data_averaged <- summarise(group_by(data_simulations,
                                      method, feasible_P1,
                                      feasible_P2,
                                      mdp_id,
                                      time),
                             value = mean(value),
                             sd_value=sd(value),

                             state_reefs = mean(state_reefs-1),
                             sd_state_reefs = sd(state_reefs-1),

                             state_P1 = mean(state_P1-1),
                             sd_state_P1 = sd(state_P1-1),

                             state_P2 = mean(state_P2-1),
                             sd_state_P2 = sd(state_P2-1)
  )
}

## process data-averaged ####
#generic mpd id
Tmax <- max(data_averaged$time)
data_optimal <- data_averaged %>%
  filter((method=="optimal"&time==Tmax)) %>%
  select(value, mdp_id)
names(data_optimal) <- c("Vopt", "mdp_id")

data_averaged <- merge(data_averaged, data_optimal, by = c("mdp_id"))

data_averaged <- data_averaged %>%
  mutate(rEVPI = (Vopt-value)/Vopt)

## Choosing colors ####
filling_data <- data.frame(method = c("random actions",
                                      "non-adaptive",
                                      "MOMDP",
                                      # "interpretable",
                                      "optimal"),
                           col = c("indianred1",
                                   "lightsalmon",
                                   "lightgreen",
                                   # "steelblue",
                                   "hotpink"))

## plots at Tmax####
data_Tmax <- filter(data_averaged, time==Tmax)
data_Tmax <- group_by(data_Tmax,
                      method,
                      project_feasibility,
                      mdp_id)
data_Tmax <- summarise(data_Tmax,
                       rEVPI = mean(rEVPI))
fp1p2 <- ggplot(data = filter(group_by(data_Tmax,
                                method, project_feasibility), method !="optimal") ,
                aes(y = rEVPI, fill=method)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "method", y = "Relative Expected Value of Perfect Information") +
  facet_wrap(~project_feasibility,nrow = 1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = filling_data$col,
                    breaks = filling_data$method)+
  theme(text = element_text(size = 15))
fp1p2

f <- ggplot(data = filter(group_by(data_Tmax,
                                   method), method !="optimal"),
            aes(x = method, y = rEVPI, fill=method)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "method", y = "Relative gap to the optimal") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = filling_data$col,
                    breaks = filling_data$method)+
  theme(text = element_text(size = 15))
f

## plots average value ####
data_values_avg <- summarise(group_by(data_averaged,
                                      method, time),
                             mean_value = mean(rEVPI),
                             sd_value=sd(rEVPI))
f_time <- ggplot(data_values_avg,
                 aes(x=time, y=mean_value,
                     group = interaction( method),
                     col = method))+
  geom_line(size=1.5) +
  theme_bw()+
  labs(x = "Time steps", y = "Average relative expected sum of discounted rewards") +
  scale_colour_manual(values = filling_data$col,
                      breaks = filling_data$method)+
  theme(text = element_text(size = 15))

f_time
ggarrange(f_time, f, ncol = 2,
          labels = c("A", "B"),
          common.legend = TRUE, legend = "bottom",
          nrow = 1)

figure <- ggarrange(ggarrange(f, f_time, ncol = 2,
                              labels = c("A", "B"),
                              legend = "none"),
                    ggarrange(fp1p2,
                              labels = c("C"), legend = "bottom"),
                    common.legend = TRUE, legend = "bottom",
                    nrow = 2)
figure

## histogram ####
data_hist <- group_by(data_Tmax,
                      method,
                      mdp_id)
data_hist <- summarise(data_hist,
                       rEVPI = mean(rEVPI),
                       project_feasibility=project_feasibility)

data_hist_pomdp <- filter(data_hist, method=="MOMDP")
data_hist_pomdp <- arrange(data_hist_pomdp, mdp_id)

data_hist_non_ad <- filter(data_hist, method=="non-adaptive")
data_hist_non_ad <- arrange(data_hist_non_ad, mdp_id)
data_hist_pomdp$rEVPI_non_ad <- data_hist_non_ad$rEVPI

data_hist_pomdp$diff <- data_hist_pomdp$rEVPI_non_ad -data_hist_pomdp$rEVPI

mean_positive <- mean(data_hist_pomdp[which(data_hist_pomdp$diff>0),]$diff)
mean_negative <- mean(data_hist_pomdp[which(data_hist_pomdp$diff<0),]$diff)
mean_val <- mean(data_hist_pomdp$diff)
max_val <- max(data_hist_pomdp$diff)

ggplot(data_hist_pomdp,#%>%filter(project_feasibility=="3 both projects feasible"),
       aes(x=diff))+
  geom_density(fill="steelblue")+
  theme_minimal()+
  geom_vline(xintercept=0, col="red", size=1)+
  # geom_vline(xintercept=mean_negative, col="orange", size=1)+
  # geom_vline(xintercept=mean_positive, col="green", size=1)+
  # geom_vline(xintercept=mean_val, col="blue", size=1)+
  facet_wrap(~project_feasibility ,nrow=3)+
  xlab("Difference in relative performance between best non-adaptive policy and hidden model MDP strategy")+
  ylab("Density of tested scenarios")+
  theme(text = element_text(size = 15))



