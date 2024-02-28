belief_invest_to_surrender <- function(cost_dev,
                                       p_idle,
                                       Rnord,
                                       Wready,
                                       disc){
  #cost_dev: cost of development
  #p_idle: probability of staying idle after one investment
  #Rnord: instant expect reward when doing nothing
  #Wready: expected benefits obtained once the technology is ready
  # (ie value function of POMDP with 4 possible scenarios)
  return(cost_dev*(1-p_idle*disc)/(
    (Rnord-cost_dev)*(p_idle*disc-disc)+(1-disc)*(1-p_idle)*disc*Wready
))
}

max_years <- function(belief_init,
                      p_idle,
                      belief_IS){
  #belief_init: initial belief state, initial belief in project feasibility
  #p_idle: probability of staying idle after investing
  #belief_IS belief where policy changes
  if (belief_init>=belief_IS){
    return(max((log((1-belief_init)*belief_IS) -
             log(belief_init*(1-p_idle*belief_IS)))/
             log(p_idle)
           , 0 ))
  } else {
    return(0)
  }
}

expected_value_feasible <- function(cost_dev,
                                    p_idle,
                                    Rnord,
                                    Wready,
                                    disc){
  return((Rnord-cost_dev+(1-p_idle)*disc*Wready)/(1-p_idle*disc))
}

expected_value_infeasible <- function(cost_dev,
                                    Rnord,
                                    disc){
  return((Rnord-cost_dev)/(1-disc))
}

expected_value_analytical <- function(belief_init,
                                      cost_dev,
                                      p_idle,
                                      Rnord,
                                      Wready,
                                      disc){

  return(max(expected_value_feasible(cost_dev, p_idle, Rnord, Wready, disc)*belief_init+
           expected_value_infeasible(cost_dev, Rnord, disc)*(1-belief_init),
         expected_value_infeasible(0, Rnord, disc)))

}
