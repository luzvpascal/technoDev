##################
#volume functions#
##################
belief_A1A1 <- function(belief_benef_low,belief_benef_high){
  (1-belief_benef_low)*(1-belief_benef_high)
}

belief_A1A2 <- function(belief_benef_low,belief_benef_high){
  (1-belief_benef_low)*belief_benef_high
}

belief_A2A1 <- function(belief_benef_low,belief_benef_high){
  belief_benef_low*(1-belief_benef_high)
}

belief_A2A2 <- function(belief_benef_low,belief_benef_high){
  belief_benef_low*belief_benef_high
}


#####################
#value functions low#
#####################

valA1A1_low <- function(pL,pH,costImp_P1_index){
  (V_min*(1-gamma+gamma*pH)+V_max*(gamma-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}

valA1A2_low <- function(pL,pH,costImp_P1_index){
  (V_min*(1-gamma+gamma*pH)+(V_max-costImp_P1_index)*(gamma-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}

valA2A1_low <- function(pL,pH,costImp_P1_index){
  ((V_min-costImp_P1_index)*(1-gamma+gamma*pH)+V_max*(gamma-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}

valA2A2_low <- function(pL,pH,costImp_P1_index){
  ((V_min-costImp_P1_index)*(1-gamma+gamma*pH)+(V_max-costImp_P1_index)*(gamma-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}


#################
# alpha vectors #
#################

alphaA1A1_low <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA1A1_low(p1,p2)
}

alphaA1A2_low <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA1A2_low(p1,(1+p2)/2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
    valA1A2_low(p1,(p2)/2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
    valA1A2_low(p1,(1+p2)/2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
    valA1A2_low(p1,(p2)/2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

alphaA2A1_low <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA2A1_low((1+p1)/2,p2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
    valA2A1_low((1+p1)/2,p2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
    valA2A1_low((p1)/2,p2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
    valA2A1_low((p1)/2,p2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

alphaA2A2_low <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA2A2_low((1+p1)/2,(1+p2)/2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
  valA2A2_low((1+p1)/2,(p2)/2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
  valA2A2_low((p1)/2,(1+p2)/2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
  valA2A2_low((p1)/2,(p2)/2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

#####################
#value functions high#
#####################

valA1A1_high <- function(pL,pH,costImp_P1_index){
  (V_min*(gamma*pH)+V_max*(1-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}


valA1A2_high <- function(pL,pH,costImp_P1_index){
  (V_min*(gamma*pH)+(V_max-costImp_P1_index)*(1-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}

valA2A1_high <- function(pL,pH,costImp_P1_index){
  ((V_min-costImp_P1_index)*(gamma*pH)+V_max*(1-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}

valA2A2_high <- function(pL,pH,costImp_P1_index){
  ((V_min-costImp_P1_index)*(gamma*pH)+(V_max-costImp_P1_index)*(1-gamma*pL))/((1-gamma)*(1-pL*gamma+pH*gamma))
}


#################
# alpha vectors #
#################

alphaA1A1_high <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA1A1_high(p1,p2)
}

alphaA1A2_high <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA1A2_high(p1,(1+p2)/2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
    valA1A2_high(p1,(p2)/2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
    valA1A2_high(p1,(1+p2)/2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
    valA1A2_high(p1,(p2)/2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

alphaA2A1_high <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA2A1_high((1+p1)/2,p2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
    valA2A1_high((1+p1)/2,p2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
    valA2A1_high((p1)/2,p2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
    valA2A1_high((p1)/2,p2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

alphaA2A2_high <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  valA2A2_high((1+p1)/2,(1+p2)/2,costImp_P1_index)*belief_A1A1(belief_benef_low,belief_benef_high)+
    valA2A2_high((1+p1)/2,(p2)/2,costImp_P1_index)*belief_A1A2(belief_benef_low,belief_benef_high)+
    valA2A2_high((p1)/2,(1+p2)/2,costImp_P1_index)*belief_A2A1(belief_benef_low,belief_benef_high)+
    valA2A2_high((p1)/2,(p2)/2,costImp_P1_index)*belief_A2A2(belief_benef_low,belief_benef_high)
}

########################
## analytical solution#
#######################
analytical_Wready <- function(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high){
  vect_vals_low <- c(alphaA1A1_low(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                     alphaA1A2_low(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                     alphaA2A1_low(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                     alphaA2A2_low(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high))

  max_index_low <- which.max(vect_vals_low)

  vect_vals_high <- c(alphaA1A1_high(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                      alphaA1A2_high(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                      alphaA2A1_high(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high),
                      alphaA2A2_high(p1,p2,costImp_P1_index,belief_benef_low,belief_benef_high))

  max_index_high <- which.max(vect_vals_high)
  ## steady state
  statesNames <- c("unhealthy", "healthy")
  markovB <- new("markovchain", states = statesNames,
                 transitionMatrix=transition_from_parameters(c(p1,p2),N_actions = 1)[,,1],
                 name = "A markovchain Object"
  )
  steady_states <- steadyStates(markovB)[1,]

  value <- steady_states%*%c(vect_vals_low[max_index_low[1]],vect_vals_high[max_index_high[1]])
  return(list(value, max_index_low, max_index_high))
}

policy_function <- function(solution){
  action_low <- ifelse(solution[[2]]<=2,1,2)
  action_high <- ifelse(solution[[3]]==1|solution[[3]]==3,1,2)
  return(paste0(action_low,"-",action_high))
}

#########################################
#functions for simulations of management#
#########################################

write_alpha_vectors_analytical <- function(p1,p2,costImp_P1_index){
  header <- '<?xml version="1.0" encoding="ISO-8859-1"?>
<Policy version="0.1" type="value" model="res/pomdpx/AM/0.001cost0.620.38adaptive_management.pomdpx" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="policyx.xsd">
<AlphaVector vectorLength="4" numObsValue="2" numVectors="8">'

  #a1a1 low
  a1a1low <- paste0('<Vector action="0" obsValue="0">',
         valA1A1_low(p1,p2,costImp_P1_index), " ",
         valA1A1_low(p1,p2,costImp_P1_index)," ",
         valA1A1_low(p1,p2,costImp_P1_index)," ",
         valA1A1_low(p1,p2,costImp_P1_index)," ",
         '</Vector>')
  #a1a1 high
  a1a1high <- paste0('<Vector action="0" obsValue="1">',
         valA1A1_high(p1,p2,costImp_P1_index)," ",
         valA1A1_high(p1,p2,costImp_P1_index)," ",
         valA1A1_high(p1,p2,costImp_P1_index)," ",
         valA1A1_high(p1,p2,costImp_P1_index)," ",
         '</Vector>')

  #a1a2 low
  a1a2low <- paste0('<Vector action="0" obsValue="0">',
              valA1A2_low(p1,(1+p2)/2,costImp_P1_index)," ",
              valA1A2_low(p1,(p2)/2,costImp_P1_index)," ",
              valA1A2_low(p1,(1+p2)/2,costImp_P1_index)," ",
              valA1A2_low(p1,(p2)/2,costImp_P1_index)," ",
              '</Vector>')
  #a1a2 high
  a1a2high <- paste0('<Vector action="1" obsValue="1">',
                    valA1A2_high(p1,(1+p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(1+p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(p2)/2,costImp_P1_index)," ",
                    '</Vector>')

  #a2a1 low
  a2a1low <- paste0('<Vector action="1" obsValue="0">',
                    valA2A1_low((1+p1)/2,p2,costImp_P1_index)," ",
                    valA2A1_low((1+p1)/2,p2,costImp_P1_index)," ",
                    valA2A1_low((p1)/2,p2,costImp_P1_index)," ",
                    valA2A1_low((p1)/2,p2,costImp_P1_index)," ",
                    '</Vector>')
  #a2a1 high
  a2a1high <- paste0('<Vector action="0" obsValue="1">',
                    valA1A2_high(p1,(1+p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(1+p2)/2,costImp_P1_index)," ",
                    valA1A2_high(p1,(p2)/2,costImp_P1_index)," ",
                    '</Vector>')
  #a2a2 low
  a2a2low <- paste0('<Vector action="1" obsValue="0">',
                      valA2A2_low((1+p1)/2,(1+p2)/2,costImp_P1_index)," ",
                      valA2A2_low((1+p1)/2,(p2)/2,costImp_P1_index)," ",
                      valA2A2_low((p1)/2,(1+p2)/2,costImp_P1_index)," ",
                      valA2A2_low((p1)/2,(p2)/2,costImp_P1_index)," ",
                    '</Vector>')
  #a2a2 high
  a2a2high <- paste0('<Vector action="1" obsValue="1">',
                     valA2A2_high((1+p1)/2,(1+p2)/2,costImp_P1_index)," ",
                     valA2A2_high((1+p1)/2,(p2)/2,costImp_P1_index)," ",
                     valA2A2_high((p1)/2,(1+p2)/2,costImp_P1_index)," ",
                     valA2A2_high((p1)/2,(p2)/2,costImp_P1_index)," ",
                    '</Vector>')

  ending <- '</AlphaVector> </Policy>'

  text <- paste(header,
                a1a1low,
                a1a1high,
                a1a2low,
                a1a2high,
                a2a1low,
                a2a1high,
                a2a2low,
                a2a2high,
                ending,
                sep="\n")

  fileConn<-file(file_outpolicy_AM_analytical(costImp_P1_index,p1,p2))
  writeLines(text, fileConn)
  close(fileConn)
}
