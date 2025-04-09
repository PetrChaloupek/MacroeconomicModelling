################################################################################
##################### THREE EQUATION NEW KEYNESIAN MODEL #######################
################################################################################

rm(list = ls())
cat("\014")

# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("reshape2")
library(ggplot2)
library(gridExtra)
library(reshape2)

#### Definition of the model parameters ########################################
# IS curve
alpha_1 <- 0.15  
alpha_2 <- 0.6   
alpha_3 <- 0.15  

# Phillips curve
beta_1 <- 0.6    
beta_2 <- 0.4    
beta_3 <- 0.3    

# Taylor rule
gamma_1 <- 0.8   
gamma_2 <- 0.3   
gamma_3 <- 0.7   

# Shock persistence
rho_y <- 0.7     # Persistence of demand shock
rho_pi <- 0.7    # Persistence of supply shock
rho_r <- 0.7     # Persistence of monetary shock

#### Simulation of model #######################################################
simulate_nk_model <- function(T, shock_type = NULL, shock_size = 0.01) {
  # shock_type: "demand", "supply", "monetary", or NULL
  # shock_size: size of the shock
  
  # Vectors of variables
  y_gap <- rep(0, T)
  pi <- rep(0, T)
  r <- rep(0, T)
  
  # Vectors of shocks
  shock_y <- rep(0, T)
  shock_pi <- rep(0, T)
  shock_r <- rep(0, T)
  
  if (!is.null(shock_type)) {
    if (shock_type == "demand") {
      shock_y[1] <- shock_size
    } else if (shock_type == "supply") {
      shock_pi[1] <- shock_size
    } else if (shock_type == "monetary") {
      shock_r[1] <- shock_size
    }
  }
  
  # Persistence of shocks
  for (t in 2:T) {
    shock_y[t] <- rho_y * shock_y[t-1]
    shock_pi[t] <- rho_pi * shock_pi[t-1]
    shock_r[t] <- rho_r * shock_r[t-1]
  }
  
  # Simulation of the model
  for (t in 2:T) {
    if (t < T) {
      E_y_gap_t1 <- y_gap[t+1] # Future output gap (will be updated iteratively)
      E_pi_t1 <- pi[t+1]       # Future inflation (will be updated iteratively)
    } else {
      # For the last period, we assume a return to steady state
      E_y_gap_t1 <- y_gap[t-1]
      E_pi_t1 <- pi[t-1]
    }
    
    # IS curve
    y_gap[t] <- alpha_1 * E_y_gap_t1 + alpha_2 * y_gap[t-1] - alpha_3 * r[t-1] + shock_y[t]
    
    # Phillips curve
    pi[t] <- beta_1 * pi[t-1] + beta_2 * E_pi_t1 + beta_3 * y_gap[t] + shock_pi[t]
    
    # Taylor rule
    r[t] <- gamma_3 * r[t-1] + (1-gamma_3) * (gamma_1 * pi[t] + gamma_2 * y_gap[t]) + shock_r[t]
  }
  for (iter in 1:30) {
    for (t in 2:(T-1)) {
      E_y_gap_t1 <- y_gap[t+1]
      E_pi_t1 <- pi[t+1]
      
      y_gap[t] <- alpha_1 * E_y_gap_t1 + alpha_2 * y_gap[t-1] - alpha_3 * r[t-1] + shock_y[t]
      
      pi[t] <- beta_1 * pi[t-1] + beta_2 * E_pi_t1 + beta_3 * y_gap[t] + shock_pi[t]
      
      r[t] <- gamma_3 * r[t-1] + (1-gamma_3) * (gamma_1 * pi[t] + gamma_2 * y_gap[t]) + shock_r[t]
    }
    
    t <- T
    E_y_gap_t1 <- 0  # We assume a return to steady state
    E_pi_t1 <- 0     # We assume a return to steady state
    
    y_gap[t] <- alpha_1 * E_y_gap_t1 + alpha_2 * y_gap[t-1] - alpha_3 * r[t-1] + shock_y[t]
    
    pi[t] <- beta_1 * pi[t-1] + beta_2 * E_pi_t1 + beta_3 * y_gap[t] + shock_pi[t]
    
    r[t] <- gamma_3 * r[t-1] + (1-gamma_3) * (gamma_1 * pi[t] + gamma_2 * y_gap[t]) + shock_r[t]
  }
  
  # Returning the results as a data frame
  return(data.frame(t = 1:T, y_gap = y_gap, pi = pi, r = r))
}

#### Functions for IRF results #################################################
plot_irf <- function(shock_type, horizon = 40, shock_size = 0.01) {
  # Simulate the model with a shock
  sim_results <- simulate_nk_model(horizon, shock_type, shock_size)
  
  df_melted <- melt(sim_results, id.vars = "t", variable.name = "variable", 
                    value.name = "value")
  
  shock_title <- switch(shock_type,
                        "demand" = "Demand shock",
                        "supply" = "Supply shock",
                        "monetary" = "Monetary shock")
  
  p <- ggplot(df_melted, aes(x = t, y = value, color = variable)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("IRF:", shock_title),
         x = "Period", y = "IRF") +
    scale_color_manual(values = c("y_gap" = "blue", "pi" = "red", "r" = "green"),
                       labels = c("y_gap" = "Output gap", "pi" = "Inflation", 
                                  "r" = "Interest rate")) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(p)
}

plot_irf_separate <- function(shock_type, horizon = 40, shock_size = 0.01) {
  sim_results <- simulate_nk_model(horizon, shock_type, shock_size)
  
  shock_title <- switch(shock_type,
                        "demand" = "Demand shock",
                        "supply" = "Supply shock",
                        "monetary" = "Monetary shock")
  
  p1 <- ggplot(sim_results, aes(x = t, y = y_gap)) +
    geom_line(color = "blue", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("IRF:", shock_title), y = "Output gap", x = "Period") +
    theme_minimal()
  
  p2 <- ggplot(sim_results, aes(x = t, y = pi)) +
    geom_line(color = "red", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "", y = "Inflation", x = "Period") +
    theme_minimal()
  
  p3 <- ggplot(sim_results, aes(x = t, y = r)) +
    geom_line(color = "green", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "", y = "Interest rate", x = "Period") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, nrow = 3)
}

# Function for sensitivity analysis
sensitivity_analysis <- function(parameter_name, values, shock_type = "demand",
                                 variable = "y_gap", horizon = 40) {
  original_params <- list(
    alpha_1 = alpha_1, alpha_2 = alpha_2, alpha_3 = alpha_3,
    beta_1 = beta_1, beta_2 = beta_2, beta_3 = beta_3,
    gamma_1 = gamma_1, gamma_2 = gamma_2, gamma_3 = gamma_3
  )
  
  results_df <- data.frame()
  
  for (value in values) {
    if (parameter_name == "alpha_1") alpha_1 <<- value
    else if (parameter_name == "alpha_2") alpha_2 <<- value
    else if (parameter_name == "alpha_3") alpha_3 <<- value
    else if (parameter_name == "beta_1") beta_1 <<- value
    else if (parameter_name == "beta_2") beta_2 <<- value
    else if (parameter_name == "beta_3") beta_3 <<- value
    else if (parameter_name == "gamma_1") gamma_1 <<- value
    else if (parameter_name == "gamma_2") gamma_2 <<- value
    else if (parameter_name == "gamma_3") gamma_3 <<- value
    
    sim_results <- simulate_nk_model(horizon, shock_type)
    
    temp_df <- data.frame(
      t = sim_results$t,
      value = sim_results[[variable]],
      parameter_value = as.character(value)
    )
    results_df <- rbind(results_df, temp_df)
    
    alpha_1 <<- original_params$alpha_1
    alpha_2 <<- original_params$alpha_2
    alpha_3 <<- original_params$alpha_3
    beta_1 <<- original_params$beta_1
    beta_2 <<- original_params$beta_2
    beta_3 <<- original_params$beta_3
    gamma_1 <<- original_params$gamma_1
    gamma_2 <<- original_params$gamma_2
    gamma_3 <<- original_params$gamma_3
  }
  
  var_label <- switch(variable,
                      "y_gap" = "Output gap",
                      "pi" = "Inflation",
                      "r" = "Interest rate")
  
  shock_label <- switch(shock_type,
                        "demand" = "Demand shock",
                        "supply" = "Supply shock",
                        "monetary" = "Monetary shock")
  
  p <- ggplot(results_df, aes(x = t, y = value, color = parameter_value, 
                              group = parameter_value)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Sensitive of", var_label, "to a parameter", parameter_name,
                       "-", shock_label),
         x = "Period", y = var_label) +
    scale_color_discrete(name = parameter_name) +
    theme_minimal()
  
  return(p)
}

#### IRF plots #################################################################
demand_irf <- plot_irf("demand")
supply_irf <- plot_irf("supply")
monetary_irf <- plot_irf("monetary")

print(demand_irf)
print(supply_irf)
print(monetary_irf)

#### Alpha_3 sensitivity analysis ##############################################
plot_irf_separate("demand")
plot_irf_separate("supply")
plot_irf_separate("monetary")

alpha3_sensitivity_y <- sensitivity_analysis("alpha_3", c(0.05, 0.15, 0.25),
                                             "monetary", "y_gap")
alpha3_sensitivity_pi <- sensitivity_analysis("alpha_3", c(0.05, 0.15, 0.25),
                                              "monetary", "pi")
alpha3_sensitivity_r <- sensitivity_analysis("alpha_3", c(0.05, 0.15, 0.25),
                                             "monetary", "r")

print(alpha3_sensitivity_y)
print(alpha3_sensitivity_pi)
print(alpha3_sensitivity_r)

#### Beta_3 sensitivity analysis ###############################################
beta3_sensitivity_y <- sensitivity_analysis("beta_3", c(0.1, 0.3, 0.5),
                                            "demand", "y_gap")
beta3_sensitivity_pi <- sensitivity_analysis("beta_3", c(0.1, 0.3, 0.5),
                                             "demand", "pi")
beta3_sensitivity_r <- sensitivity_analysis("beta_3", c(0.1, 0.3, 0.5),
                                            "demand", "r")

print(beta3_sensitivity_y)
print(beta3_sensitivity_pi)
print(beta3_sensitivity_r)