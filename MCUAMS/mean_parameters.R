#this file aims to find empirically the average parameters 
#for each opt space for each policy
#we consider the case of 2 states and n actions
library(MDPtoolbox)
library(data.table)

#value function of an action A in a problem with 2 states
value_function<-function(init, a, b, Rlow, Rhigh,disc){
  #init is the initial state of the system as string ("low" or "high") 
  # a is the transition problability to go from low to low
  # b is the transition probability to go from high to low
  # Rlow is the reward obtained if low and A
  # Rhigh is the reward obtained if high and A
  # disc is the discount factor
  if (init == "low"){
    return((Rlow*(1-disc+disc*b) + Rhigh*(disc-disc*a))/((1-disc)*(1-a*disc+b*disc)))
  } else {
    return((Rhigh*(1-a*disc)+Rlow*b*disc)/((1-disc)*(1-a*disc+b*disc)))
  }
}

value_all_policies<-function(init, parameters, rew, disc){
  #init is the initial state of the system as string ("low" or "high") 
  #parameters must be inputed as a matrix of 2 lines
  #one line for all the possible parameters of each state
  #rew is a matrix of size 2*A of the rewards.
  #disc is the discount
  values <- c()
  for (i in seq(ncol(parameters))){
    for(j in seq(ncol(parameters))){
      v <- value_function(init, parameters[1,i],
                          parameters[2,j], rew[1,i], rew[2,j], disc)
      values <- c(values, v)
    }
  }
  values
}

mean_parameters <- function(init, rew, disc, N_trials=1e4){
  # inputs:
  #init is the initial state of the system as string ("low" or "high") 
  # rew : matrix of reward function with 2 lines and n columns
  # disc is the discount factor, between [0,1[
  # N_trials is the number of iterations
  
  #function:
  #samples space [0,1]**2N to estimate the average model for each policy
  
  #outputs list
  #mean_params : data.table with the mean parameters for each policy
  #policies are ordered as follows (example for 3 actions)
  # policy 1: low:Action1 high:Action1
  # policy 2: low:Action1 high:Action2
  # policy 3: low:Action1 high:Action3
  # policy 4: low:Action2 high:Action1
  # policy 5: low:Action2 high:Action2
  # policy 6: low:Action2 high:Action3
  # policy 7: low:Action3 high:Action1
  # policy 8: low:Action3 high:Action2
  # policy 9: low:Action3 high:Action3
  #priors: frequency of each policy
  
  #loop to estimate the parameters
  
  #number of parameters to be drawn
  N_param <- 2*ncol(rew)
  
  data <- as.data.frame(matrix(0, ncol = N_param+1, nrow = 0))
  names(data) <- c(paste("p",seq(N_param), sep = ''), "opt")
  data <- data.table(data)
  
  for (k in seq(N_trials)){
    params <- matrix(runif(N_param), nrow = 2)
    
    #values of the policies
    values <- value_all_policies(init, params, rew, disc)
    
    #optimal policy for low and high
    pol_opt <- which.max(values)
    
    data_new <- as.data.frame(matrix(c(params), ncol = N_param, nrow =1,
                                     byrow = T))
    data_new$opt <- pol_opt
    
    names(data_new) <- c(paste("p",seq(N_param), sep = ''), "opt")
    data_new <- data.table(data_new)
    
    data <- rbind(data, data_new)
  }
  
  mean_params <- data[, lapply(.SD,mean), by=.(opt),
                     .SDcols =paste("p",seq(N_param), sep = '')]
  mean_params <- mean_params[order(mean_params$opt),]
  
  priors <- as.data.frame(table(data$opt)/nrow(data)) 
  names(priors)<- c("opt", "freq")
  
  return(list(mean_params = mean_params,
             priors=priors)
  )
}
