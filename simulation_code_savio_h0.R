###load libraries
#Sys.setenv(R_LIBS_USER="/global/scratch/users/odedritov/R")


#library(dplyr)
library(lme4)
library(nloptr)
library(kyotil, lib.loc="/global/scratch/users/odedritov/R")
library(doSNOW, lib.loc="/global/scratch/users/odedritov/R")

###connect to nodes
ncoresPerNode <-as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))
nodeNames <-strsplit(Sys.getenv("SLURM_NODELIST"), ",")[[1]]
machines=rep(nodeNames, each = ncoresPerNode)
cl = makeCluster(machines, type = "SOCK")
registerDoSNOW(cl)



###import covariates and probabilities
probabilities_table = read.csv2("test_probabilities_h0.csv", col.names = c("distribution_inequality", "effort_inequality", "effort", "partner_present", "p")) #table with a priori probabilities for each type of condition

covariates = read.csv2("covariates_table.csv") #table with all covariates based on preliminary list of studies

covariates_with_probabilities = merge(covariates, probabilities_table, by = c("distribution_inequality","effort_inequality","effort","partner_present"), sort = FALSE)


###get fixed coefficients

covariates_with_probabilities["social_inequality"] = ifelse(covariates_with_probabilities$partner_present == "y" & (covariates_with_probabilities$distribution_inequality == "y" | covariates_with_probabilities$effort_inequality == "y"), "y", "n")

###define function for simulation
sd_intercept_species = 0.5 
sd_effort_slope_species = 0.6
sd_social_ineqality_slope_species = 0.2
sd_effortXsocial_inequality_slope_species = 1.5

sd_intercept_study = 0.5 
sd_effort_slope_study = 0.2
sd_social_inequality_slope_study = 0.1
sd_effortXsocial_inequality_study = 0.4

sd_intercept_subject = 0.5
sd_effort_slope_subject = 0.3
sd_social_ineqality_slope_subject = 0.1
sd_effortXsocial_inequality_slope_subject = 0.75

random_effects_v = data.frame(sd_intercept_species, sd_effort_slope_species, sd_social_ineqality_slope_species, sd_effortXsocial_inequality_slope_species,sd_intercept_study, sd_social_inequality_slope_study, sd_effort_slope_study, sd_effortXsocial_inequality_study, sd_intercept_subject, sd_effort_slope_subject, sd_social_ineqality_slope_subject, sd_effortXsocial_inequality_slope_subject)

simulate_data = function(seed = 1, sim_data = covariates_with_probabilities, random_effects = random_effects_v, .combine = "cbind"){
  sim_data["b_intercept"] = 0
  sim_data["b_effort"] = 0
  sim_data["b_social_inequality"] = 0
  sim_data["b_effortXsocial_inequality"] = 0
  
  for(s in unique(sim_data$subject)){
    sim_data[which(sim_data$subject == s),]$b_intercept = sim_data[which(sim_data$subject == s),]$b_intercept + rnorm(1, 0, random_effects[["sd_intercept_subject"]])
    sim_data[which(sim_data$subject == s),]$b_effort = sim_data[which(sim_data$subject == s),]$b_effort + rnorm(1, 0, random_effects[["sd_effort_slope_subject"]])
    sim_data[which(sim_data$subject == s),]$b_social_inequality = sim_data[which(sim_data$subject == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_social_ineqality_slope_subject"]])
    sim_data[which(sim_data$subject == s),]$b_effortXsocial_inequality = sim_data[which(sim_data$subject == s),]$b_effortXsocial_inequality + rnorm(1, 0, random_effects[["sd_effortXsocial_inequality_slope_subject"]])
  }
  
  for(s in unique(sim_data$species)){
    sim_data[which(sim_data$species == s),]$b_intercept = sim_data[which(sim_data$species == s),]$b_intercept + rnorm(1, 0, random_effects[["sd_intercept_species"]])
    sim_data[which(sim_data$species == s),]$b_effort = sim_data[which(sim_data$species == s),]$b_effort + rnorm(1, 0, random_effects[["sd_effort_slope_species"]])
    sim_data[which(sim_data$species == s),]$b_social_inequality = sim_data[which(sim_data$species == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_social_ineqality_slope_species"]])
    sim_data[which(sim_data$species == s),]$b_effortXsocial_inequality = sim_data[which(sim_data$species == s),]$b_effortXsocial_inequality + rnorm(1, 0, random_effects[["sd_effortXsocial_inequality_slope_species"]])
  }
  
  for(s in unique(sim_data$paper)){
    sim_data[which(sim_data$paper == s),]$b_intercept = sim_data[which(sim_data$paper == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_intercept_study"]])
    sim_data[which(sim_data$paper == s),]$b_effort = sim_data[which(sim_data$paper == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_effort_slope_study"]])
    sim_data[which(sim_data$paper == s),]$b_social_inequality = sim_data[which(sim_data$paper == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_social_inequality_slope_study"]])
    sim_data[which(sim_data$paper == s),]$b_effortXsocial_inequality = sim_data[which(sim_data$paper == s),]$b_social_inequality + rnorm(1, 0, random_effects[["sd_effortXsocial_inequality_study"]])
  }
  
  sim_data$y_log = sim_data$b_intercept + sim_data$b_effort*(sim_data$effort == "y") + sim_data$b_social_inequality*(sim_data$social_inequality == "y") + sim_data$b_effortXsocial_inequality*((sim_data$effort == "y") & (sim_data$social_inequality == "y"))
  
  sim_data["y_prob"] = (1/(1+exp(-sim_data$y_log)))
  
  sim_data["y_simulated"] = rbinom(n = nrow(sim_data), size = 1, prob = sim_data$y_prob)
  
  return(sim_data)
}

###define function to run n simulations
run_sims = function(i, n, model, restricted, announce_progress = TRUE, subset_rows = TRUE){
  
  directory = paste("/global/scratch/users/odedritov/sim_results/simulation_", i, "_h0/", sep="")
  dir.create(directory)
  
  sim_results <- foreach(i = 1:n, .combine = "cbind", .export = c("simulate_data", "random_effects_v", "covariates_with_probabilities", "subset_rows", "directory")) %dopar% {
    
    library(lme4, lib.loc="/global/scratch/users/odedritov/R")
    library(kyotil, lib.loc="/global/scratch/users/odedritov/R")
    
    sim_summary = data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("iteration", "b_intercept", "b_effort", "b_social_inequality", "b_effortXsocial_inequality", "p(social_inequality)", "p(model comparison)", "isSingular", "warnings"))))
    
    
    temp_data = simulate_data(seed = i)
    
    temp_data = temp_data[subset_rows,]
    
    temp_data$subject = droplevels(temp_data$subject)
    
    temp_data$species = droplevels(temp_data$species)
    
    
    temp_model = keepWarnings(glmer(model, data = temp_data, family = binomial, 
                                    control=glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5))))
    
    temp_model_restricted = glmer(restricted, data = temp_data, family = binomial, 
                                  control=glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
    
    p_comparison = anova(temp_model$value, temp_model_restricted)$`Pr(>Chisq)`[2]
    
    sim_summary = c(i, fixef(temp_model$value)[1:4],
                    summary(temp_model$value)$coefficients[2,4],
                    p_comparison,
                    isSingular(temp_model$value),
                    paste(as.character(unlist(temp_model$warnings)), 
                          collapse = " | "))
    
    iteration_data = c(sim_summary, temp_model)
    
    
    save(iteration_data, file=paste(directory, i, ".RData", sep=""))
    
    
    return(sim_summary)
    
  }
  
  rownames(sim_results) = c("iteration", "b_intercept", "b_effort", "b_social_inequality", "b_effortXsocial_inequality", "p(effort:social_inequality)", "p(model comparison)", "isSingular", "warnings")
  
  return(sim_results)
}

###run simulations
n_sim = 1000

model_sim1 = y_simulated ~ effort * social_inequality + (social_inequality || subject) + (effort * social_inequality || species) + (social_inequality || paper)
 
model_sim1_restricted = y_simulated ~ effort + (social_inequality || subject) + (effort * social_inequality || species) + (social_inequality || paper)
 
simulation1 = run_sims(2, n_sim, model_sim1, model_sim1_restricted)
 
save(simulation1, file="/global/scratch/users/odedritov/simulation2_h0.RData")
