## Ram known ####
res <- read.csv(results_varying_priors_example_file)
res$max_invest_analytical <- 0
value_noRD_index <- res$value_AM_infty[1]*(1-gamma)
for (index in seq(nrow(res))){

  belief_IS <- belief_invest_to_surrender(cost_dev =costDev_P1,
                                          p_idle = prob_ready,
                                          Rnord = value_noRD_index,
                                          Wready = res$value_AM_infty[index],
                                          disc = gamma)

  if (belief_IS >0){
    Tmax_years <- max_years(initial_belief_state_P1[1],
                            prob_ready,
                            belief_IS)
    res$max_invest_analytical[index] <- Tmax_years
  }
}

fig_RAM_known <- res %>%
  ggplot()+
  geom_point(aes(x=max_invest,
                 y=max_invest_analytical),
             shape=15, size=1)+
  geom_abline(intercept = 0, slope = 1, col="red")+
  coord_equal()+
  theme_bw()+
  labs(x=TeX("Numerical $T_{max}$"),
       y=TeX("Analytical $T_{max}$"),
       title="A")

## Ram unknown####
res <- read.csv(results_varying_priors_example_file)
res_analytical <- read.csv(results_varying_priors_example_file_analytical)
names(res_analytical)[4] <- "value_AM_infty_analytical"

res <- merge(res, res_analytical, by=c("belief_benef_low" , "belief_benef_high"))

fig_RAM_unknown <- res %>%
  ggplot()+
  geom_point(aes(x=value_AM_infty,
                 y=value_AM_infty_analytical),
             shape=15, size=1)+
  geom_abline(intercept = 0, slope = 1, col="red")+
  coord_equal()+
  theme_bw()+
  labs(x=TeX("Numerical $R_{AM}$"),
       y=TeX("Analytical $R_{AM}$"),
       title = "B")

#### save figure ####
fig <- ggarrange(fig_RAM_known,fig_RAM_unknown,
          ncol=2)

ggsave(numerical_vs_analytical_figure,
       plot = fig, width = 10, height = 6, units = "in")
