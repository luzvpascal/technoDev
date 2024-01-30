###file names for MC UAMS outputs ####
output_meanPars_file  <- function(costDev, p1,p2){
  return(paste0("res/mean_params/", costDev,
                              "cost", p1, p2,
                              "reefs_P1ready_meanparams.csv"))
}

output_priors_file <- function(costDev, p1,p2){
  return(paste0("res/priors/", costDev,
                          "cost",p1, p2,
                          "reefs_P1ready_priors.csv"))
}


###file names AM ####
file_pomdpx_AM <- function(costDev, p1,p2,belief_benef_low,belief_benef_high){
  return(paste0("res/pomdpx/AM/",
                costDev, "cost",
                p1,"_", p2,"trans",
                belief_benef_low,"_",belief_benef_high,"belief_benef",
                "adaptive_management.pomdpx"))
}

file_outpolicy_AM <- function(costDev, p1,p2,belief_benef_low,belief_benef_high){
  return(paste0("res/policyx/AM/",
                costDev, "cost",
                p1,"_", p2,"trans",
                belief_benef_low,"_",belief_benef_high,"belief_benef",
                "adaptive_management.policyx"))
}

file_outpolicy_AM_analytical <- function(costDev, p1,p2,belief_benef_low,belief_benef_high){
  return(paste0("res/policyx_analytical/AM/",
                costDev, "cost",
                p1,"_", p2,"trans",
                belief_benef_low,"_",belief_benef_high,"belief_benef",
                "adaptive_management_analytical.policyx"))
}

#file names tech_dev####
file_pomdpx_techdev <- function(costDev, p1,p2,belief_benef_low,belief_benef_high){
  return(paste0("res/pomdpx/tech_dev/",
                costDev, "cost",
                p1,"_", p2,"trans",
                belief_benef_low,"_",belief_benef_high,"belief_benef",
                 "techdev_adaptive_management1techno.pomdpx"))
}

file_outpolicy_techdev <- function(costDev, p1,p2,belief_benef_low,belief_benef_high){
  return(paste0("res/policyx/tech_dev/",
                costDev, "cost",
                p1,"_", p2,"trans",
                belief_benef_low,"_",belief_benef_high,"belief_benef",
                "techdev_adaptive_management1techno.policyx"))
}
