library(markovchain)
library(zoo)
library(tidyverse)
library(dplyr)
library(ggpubr)
threshold <- 30

## average aims ####
data <- read.csv("~/baseline program/code clean/data AIMS/aims.csv")

data <- data %>% mutate(state = if_else(cover  <threshold , 1, 2))

## plots ####
cover_plot <- data %>%
  ggplot(aes(x = year, y = cover, fill = "Uncertainty")) +
  geom_line(aes(group = 1), color = "black", linetype = "solid") +  # Solid line for average
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray", alpha = 0.5) +  # Ribbon for uncertainty
  theme_bw() +
  labs(x = NULL, y = "Hard coral cover (%)") +
  scale_fill_manual(values = c("Uncertainty" = "gray")) +
  theme(
    axis.title.y = element_text(margin = margin(r = -10)),  # Adjust the margin
    axis.text.y = element_text(size = 12)  # Adjust y-axis text size
  )

cols_state <- c("black", "lightgreen")
data$state_cols <- cols_state[data$state]
state_plot <- data %>%
  ggplot(aes(x = year, y = state, col = state_cols)) +
  geom_point(size = 3) +
  scale_color_manual(values = unique(data$state_cols)) +
  theme_bw() +
  labs(x = "Year", y = "State") +  # Remove y-axis label
  scale_y_continuous(breaks = c(1, 2), labels = c("Unhealthy", "Healthy"), limits = c(0.8, 2.2)) +
  theme(
    axis.title.y = element_text(margin = margin(r = -10)),  # Adjust the margin
    axis.text.y = element_text(size = 12)  # Adjust y-axis text size
  )


ggarrange(cover_plot, state_plot, ncol = 1,
          labels = c("A", "B"),
          common.legend = FALSE, legend = "bottom",
          nrow = 2,
          heights = c(2, 1),
          align = "hv")  # Align both horizontally and vertically

## Markov chain estimation ####
sequence_markov <- data$state

createSequenceMatrix(sequence_markov)
results <- data.frame()
for (i in seq(100)){
  set.seed(83)
  fit_markov <- markovchainFit(sequence_markov,
                                confidencelevel = 0.95,
                                method="bootstrap")

  results <- rbind(results,
                   data.frame(low_low = fit_markov$estimate[1][1],
                              high_low = fit_markov$estimate[2][1]))
}

results %>%
  pivot_longer(cols=c(low_low, high_low), names_to = "trans") %>%
  ggplot(aes(y=value))+
  geom_boxplot()+
  facet_wrap(~trans)
