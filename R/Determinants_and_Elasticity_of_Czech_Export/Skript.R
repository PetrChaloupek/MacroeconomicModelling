################################################################################
################# DETERMINANTY A ELASTICITA ČESKÉHO EXPOERTU ###################
############################### Petr Chaloupek #################################
################################################################################

start_time <- Sys.time()
rm(list = ls())
cat("\014")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Příprava dat ##############################################################
# 1.1 Načtení knihoven #########################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(mFilter)
library(hpfilter)
library(urca)
library(stringr)
library(patchwork)
library(BVAR)
library(rstan)
library(knitr)
library(bayesplot)
library(ggridges)  
library(ggnewscale)
library(zoo)
library(RColorBrewer)
library(posterior)

# 1.2 Načtení dat ##############################################################

df <- read_excel("Data/df.xlsx", col_types = c("date", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric"))

df$Date <- as.Date(df$Date)

columns_to_round1 <- c("GDP_Ger", "GDP_Sk", "GDP_Pol", "GDP_Fr", "GDP_Au", 
                       "GDP_It", "P_Cz", "P_Ger", "P_Sk", "P_Pol", "P_Fr", 
                       "P_Au", "P_It")
columns_to_round2 <- c("XR", "ULC", "Import")

df[columns_to_round1] <- round(df[columns_to_round1], 2)
df[columns_to_round2] <- round(df[columns_to_round2], 3)

print(head(df))

# 1.3 Vytvoření nových proměnných pro zahraniční poptávku ######################
df$y <- (df$GDP_Ger * 0.518 + df$GDP_Sk  * 0.128 + df$GDP_Pol * 0.111 + 
           df$GDP_Fr  * 0.086 + df$GDP_Au  * 0.076 + df$GDP_It  * 0.081)

df$y <- round(df$y, 2)

# 1.4 Vytvoření nových proměnných pro zahraniční ceny ##########################

df$P_for <- (df$P_Ger * 0.518 + df$P_Sk  * 0.128 + df$P_Pol * 0.111 +
               df$P_Fr  * 0.086 + df$P_Au  * 0.076 + df$P_It  * 0.081)

df$P_for <- round(df$P_for, 2)

# 1.5 Vytvoření log proměnných #################################################

df$l_x <- log(df$Export)
df$l_y <- log(df$y)
df$l_e <- log(df$XR)
df$l_p_for <- log(df$P_for)
df$l_p_cz <- log(df$P_Cz)
df$l_ulc <- log(df$ULC)
df$l_m <- log(df$Import)

# 1.6 Vizualizace originálních časových řad ####################################

variables_with_titles <- c(
  "Export" = "Export (v mil. CZK)",
  "y" = "Zahraniční poptávka (v mil. EUR)",
  "XR" = "Směnný kurz (CZK/EUR)",
  "P_for" = "Zahraniční cenová hladina (index, 2005 = 100)",
  "P_Cz" = "Domácí cenová hladina (index, 2005 = 100)",
  "ULC" = "Jednotkové náklady práce (index, 2015 = 100)",
  "Import" = "Index importních cen (index, 2017 = 100)"
)
variables <- names(variables_with_titles)

df_long <- df %>%
  select(Date, all_of(variables)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Variable_Name", 
    values_to = "Value"
  ) %>%
  mutate(Variable_Title = recode(Variable_Name, !!!variables_with_titles))

df_long$Variable_Title <- factor(df_long$Variable_Title, 
                                 levels = variables_with_titles)

original_cara_plot <- ggplot(df_long, aes(x = Date, 
                                          y = Value, 
                                          color = Variable_Title)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Variable_Title, ncol = 3, scales = "free_y") + 
  scale_color_brewer(palette = "Set1") + 
  scale_x_date(
    date_breaks = "5 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 14) + 
  labs(
    #title = "Časové řady",
    x = "Datum",
    y = "Hodnota"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(original_cara_plot)

ggsave(
  filename = "Plots/original_cara.png", 
  plot = original_cara_plot, 
  width = 13, 
  height = 10, 
  dpi = 300
)

################################################################################
# 2. Výpočet gapu proměnných ###################################################

# 2.1. Výpočet gapu proměnných #################################################

variables <- c("l_x", "l_y", "l_e", "l_p_cz", "l_p_for", "l_ulc", "l_m")
lamb <- 1600

for (var in variables) {
  y_full <- df[[var]] 
  valid_indices <- which(!is.na(y_full) & !is.infinite(y_full))
  y_clean <- y_full[valid_indices] 
  y_matrix <- as.matrix(y_clean)
  hp_result <- hpfilter::hp1(y_matrix, lambda = lamb)
  trend <- rep(NA, nrow(df))
  gap <- rep(NA, nrow(df))
  trend[valid_indices] <- as.numeric(hp_result[[1]])
  gap[valid_indices] <- (y_clean - trend[valid_indices]) * 100
  df[[paste0(var, "_trnd")]] <- trend
  df[[paste0(var, "_gap")]] <- gap
  print(paste("Vypočítán jednostranný HP filtr pro:", var))
}

# 2.2 Úprava proměnných ########################################################

df$l_p_gap <- df$l_p_cz_gap - df$l_p_for_gap
df$l_p_trnd <- df$l_p_cz_trnd - df$l_p_for_trnd
df$l_p <- df$l_p_cz - df$l_p_for

# 2.3 Vizualizace gapu proměnných #############################################

variables <- c("l_x", "l_y", "l_e", "l_p", "l_ulc", "l_m")
variables_gap <- paste0(variables, "_gap")

variables_with_titles_gap <- c(
  "l_x_gap" = "Export",
  "l_y_gap" = "Zahraniční poptávka",
  "l_e_gap" = "Směnný kurz",
  "l_p_gap" = "Inflační diferenciál",
  "l_ulc_gap" = "Jednotkové náklady práce",
  "l_m_gap" = "Index importních cen"
)

df_long_gap <- df %>%
  select(Date, all_of(variables_gap)) %>%
  pivot_longer(
    cols = -Date, 
    names_to = "Variable_Name", 
    values_to = "Value"
  ) %>%
  mutate(Variable_Title = recode(Variable_Name, !!!variables_with_titles_gap))

df_long_gap$Variable_Title <- factor(df_long_gap$Variable_Title, 
                                     levels = variables_with_titles_gap)

gap_plot <- ggplot(df_long_gap, aes(x = Date, 
                                    y = Value, 
                                    color = Variable_Title)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(size = 1.2) +
  facet_wrap(~ Variable_Title, ncol = 3, scales = "free_y") + 
  scale_color_brewer(palette = "Set1") + 
  scale_x_date(
    date_breaks = "5 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 14) + 
  #labs(
    #title = "Odchylky proměnných od trendu") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
print(gap_plot)

ggsave(
  filename = "Plots/hp_gaps_plot.png", 
  plot = gap_plot, 
  width = 13,
  height = 8,
  dpi = 300
)

# 2.4 Test stacionarity (ADF test) #############################################

variables_gap <- c("l_x_gap", "l_y_gap", "l_e_gap", "l_p_gap", "l_ulc_gap",
                   "l_m_gap")
results_list <- list()

for (var in variables_gap) {
  series <- df[[var]]
  series_clean <- series[!is.na(series)]
  test_result <- ur.df(series_clean, type = "none", selectlags = "AIC")
  test_stat <- test_result@teststat[1]
  p_value_approx <- test_result@testreg$coefficients[1, 4]
  
  results_list[[var]] <- data.frame(
    Variable = var,
    Test_Statistic = test_stat,
    Lags_Used = test_result@lags,
    p_value_approx = p_value_approx
  )
}

adf_table_urca <- do.call(rbind, results_list)
rownames(adf_table_urca) <- NULL 

print(adf_table_urca)

write.table(
  adf_table_urca,
  file = "Tabs/adf_test.txt",
  row.names = FALSE,
  quote = FALSE,  
  sep = "\t"  
)

# 2.5. Vizualizace originální řadu, gapu a trendu ##############################

recessions_df <- data.frame(
  start = as.Date(c("2008-01-01", "2012-01-01", "2020-01-01", "2023-01-01")),
  end = as.Date(c("2009-12-31", "2013-12-31", "2020-12-31", "2023-12-31"))
)

title_map_r <- c(
  "l_x" = "Export",
  "l_y" = "Zahraniční poptávka",
  "l_e" = "Směnný kurz",
  "l_p" = "Inflační diferenciál",
  "l_ulc" = "Jednotkové náklady práce",
  "l_m" = "Index importních cen"
)

plot_colors <- c(
  "Log" = "#1f77b4",             
  "Gap" = "#ff7f0e",
  "Trend" = "#2ca02c" 
)
col_titles <- names(plot_colors)

variables_base <- names(title_map_r)
all_plot_vars <- c(variables_base, 
                   paste0(variables_base, "_gap"), 
                   paste0(variables_base, "_trnd"))

df_long_all <- df %>%
  select(Date, all_of(all_plot_vars)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Column_Name",
    values_to = "Value"
  ) %>%
  mutate(
    Plot_Type = case_when(
      str_ends(Column_Name, "_gap") ~ col_titles[2],
      str_ends(Column_Name, "_trnd") ~ col_titles[3],
      TRUE ~ col_titles[1]
    ),
    Base_Var = str_remove(Column_Name, "_gap|_trnd"),
    Variable_Title = recode(Base_Var, !!!title_map_r)
  )

df_long_all$Variable_Title <- factor(df_long_all$Variable_Title, 
                                     levels = title_map_r)
df_long_all$Plot_Type <- factor(df_long_all$Plot_Type, levels = col_titles)
variables_in_order <- levels(df_long_all$Variable_Title)

plot_list <- list()

for (i in 1:length(variables_in_order)) {
  var_title <- variables_in_order[i]
  for (j in 1:length(col_titles)) {
    plot_type <- col_titles[j]
    plot_data <- df_long_all %>%
      filter(Variable_Title == var_title, Plot_Type == plot_type)
    p <- ggplot(plot_data, aes(x = Date, y = Value)) +
      geom_rect(
        data = recessions_df,
        aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill = "gray", alpha = 0.3, inherit.aes = FALSE
      ) +
      geom_line(color = plot_colors[plot_type], size = 1) +
      theme_minimal(base_size = 11) + 
      theme(
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12)
      ) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::comma) +
      xlab(NULL)
    if (plot_type == "Gap (% odchylky)") {
      p <- p + geom_hline(yintercept = 0, color = "black", linewidth = 0.5, 
                          linetype = "dashed")
    }
    if (i == 1) {
      p <- p + ggtitle(plot_type)
    }
    if (j == 1) {
      p <- p + ylab(var_title) + 
        theme(axis.title.y = element_text(face = "bold", size = 10, angle = 90))
    } else {
      p <- p + ylab(NULL)
    }
    if (i != 6) {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    plot_list[[paste(i, j)]] <- p
  }
}

final_plot <- wrap_plots(plot_list, ncol = 3)

print(final_plot)

ggsave(
  filename = "Plots/all_cara.png", 
  plot = final_plot, 
  width = 15,
  height = 20,
  dpi = 300
)

# 2.6 Popisné statistiky gapů ##################################################

variables_gap <- c("l_x_gap", "l_y_gap", "l_e_gap", "l_p_gap", "l_ulc_gap", 
                   "l_m_gap")
stats_list <- list()

print("--- Výpočet a přiřazení statistik ---")
for (var in variables_gap) {
  series_clean <- df[[var]][!is.na(df[[var]])]
  val_mean <- mean(series_clean)
  val_var <- var(series_clean)
  val_sd <- sd(series_clean)
  assign(paste0("mu_", var), val_mean)
  assign(paste0("sig_", var), val_var)
  assign(paste0("sd_", var), val_sd)
  stats_list[[var]] <- data.frame(
    Variable = var,
    Mean = val_mean,
    Variance = val_var,
    Std_Deviation = val_sd
  )
}

stats_table <- do.call(rbind, stats_list)
rownames(stats_table) <- NULL

print("Deskriptivní statistika (Gap proměnné):")
print(stats_table)

write.table(
  stats_table,
  file = "Tabs/desc_stats_gap.txt",
  row.names = FALSE,
  quote = FALSE,
  sep = "\t"
)

################################################################################
# 3. Korelogram proměnných #####################################################

main_var <- "l_x_gap"
regressor_vars <- c("l_y_gap", "l_e_gap", "l_p_gap", "l_ulc_gap", "l_m_gap")
lags_to_compute <- 0:6

ccf_results_list <- list()
main_series <- df[[main_var]]

for (reg_var in regressor_vars) {
  temp_results <- list()
  for (k in lags_to_compute) {
    lagged_regressor_series <- lag(df[[reg_var]], n = k)
    cor_val <- cor(main_series, lagged_regressor_series, 
                   use = "pairwise.complete.obs")
    temp_results[[as.character(k)]] <- data.frame(
      Variable = reg_var,
      Lag = k,
      Correlation = cor_val
    )
  }
  ccf_results_list[[reg_var]] <- do.call(rbind, temp_results)
}

ccf_df <- do.call(rbind, ccf_results_list)
rownames(ccf_df) <- NULL

title_map_z <- c(
  "l_y_gap" = "Zahraniční poptávka",
  "l_e_gap" = "Směnný kurz",
  "l_p_gap" = "Inflační diferenciál",
  "l_ulc_gap" = "Jednotkové náklady práce",
  "l_m_gap" = "Index importních cen"
)

ccf_df <- ccf_df %>%
  mutate(
    Variable_Title = recode(Variable, !!!title_map_z),
    Label = sprintf("% .2f", Correlation),
    Text_Color = ifelse(Correlation >= 0, "white", "black")
  )
ccf_df$Variable_Title <- factor(ccf_df$Variable_Title, levels = title_map_z)
ccf_df$Lag_Factor <- factor(paste0("t-", ccf_df$Lag), 
                            levels = paste0("t-", lags_to_compute))

heatmap_plot <- ggplot(ccf_df, aes(x = Lag_Factor, y = Variable_Title, 
                                   fill = Correlation)) +
  geom_tile(color = "#E0E0E0", linewidth = 0.5) +
  geom_text(aes(label = Label, color = Text_Color), 
            fontface = "bold",
            size = 3) +       
  scale_fill_gradient2(
    low = "#01b287",
    mid = "gray",
    high = "#854c9e",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Korelace",
  ) +
  
  scale_color_manual(values = c("white" = "white", "black" = "black"), 
                     guide = "none") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    #title = "Korelace s exportem (t až t−6)",
    x = "Zpoždění",
    y = ""
  )

print(heatmap_plot)

ggsave(
  filename = "Plots/korelace_heatmap.png", 
  plot = heatmap_plot, 
  width = 9.5, 
  height = 5.2, 
  dpi = 300
)

################################################################################
# 4. Rekurzivní odhad neomezeného modelu #######################################

vars <- c("l_y_gap", "l_p_gap", "l_e_gap", "l_ulc_gap", "l_m_gap", "l_x_gap")
model_data_ts <- ts(
  data = as.matrix(df[, vars]),
  start = c(2000, 1),
  frequency = 4
)
colnames(model_data_ts) <- vars

head(model_data_ts)

# 4.1 Příprava dat pro BVAR-X ##################################################

Y_names <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")
X_names <- c("l_y_gap","l_m_gap")                      

stopifnot(exists("df"))
D <- df[, c(Y_names, X_names)]
D <- D[complete.cases(D), , drop = FALSE]

Y_full <- as.matrix(D[, Y_names, drop = FALSE])
X_full <- as.matrix(D[, X_names, drop = FALSE])

T  <- nrow(Y_full) - 1
m  <- ncol(Y_full)
kx <- ncol(X_full)

Y    <- Y_full[2:(T+1), , drop = FALSE]   # Y_t
Ylag <- Y_full[1:T,     , drop = FALSE]   # Y_{t-1}
X    <- X_full[2:(T+1), , drop = FALSE]   # X_t

ix_x <- match("l_x_gap", Y_names)        

# 4.2 Nastavení priorů #########################################################

lambda_1 <- 0.1
lambda_2 <- 0.5
lambda_4 <- 1.0
sd_const <- 1.0
sd_theory <- 0.25

sd_own   <- lambda_1
sd_cross <- lambda_1 * lambda_2
sd_exo   <- lambda_4

A_sd <- matrix(sd_cross, m, m); diag(A_sd) <- sd_own
B_sd <- matrix(sd_exo,   m, kx)

idx_i_export <- match("l_x_gap", Y_names)   # i = 4 (rovnice pro export)
idx_j_price  <- match("l_p_gap", Y_names)   # j = 1 (vliv cen)
idx_j_ulc    <- match("l_ulc_gap", Y_names) # j = 3 (vliv nákladů)

# Priory v matici A_sd
A_sd[idx_i_export, idx_j_price] <- sd_theory
A_sd[idx_i_export, idx_j_ulc]   <- sd_theory

# 4.3 Stan model ###############################################################

suppressPackageStartupMessages(library(rstan))
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = max(1, parallel::detectCores() - 1))

stan_code <- "
data {
  int<lower=1> T; int<lower=1> m; int<lower=0> kx;
  matrix[T, m] Y; matrix[T, m] Ylag; matrix[T, kx] X;
  matrix[m, m] A_sd; matrix[m, kx] B_sd; vector[m] c_sd;
  real<lower=0> lkj_eta;
}
parameters {
  matrix[m, m] A;                 // Y_{t-1}
  matrix[m, kx] B;                // X_t
  vector[m] c;                    // konstanty
  vector<lower=0>[m] sigma;
  cholesky_factor_corr[m] L_Omega;
}
transformed parameters {
  matrix[m, m] L_Sigma = diag_pre_multiply(sigma, L_Omega);
}
model {
  to_vector(A) ~ normal(0, to_vector(A_sd));
  to_vector(B) ~ normal(0, to_vector(B_sd));
  c ~ normal(0, c_sd);
  sigma ~ cauchy(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(lkj_eta);

  for (t in 1:T) {
    vector[m] mu = c + A * to_vector(Ylag[t]') + B * to_vector(X[t]');
    Y[t]' ~ multi_normal_cholesky(mu, L_Sigma);
  }
}
generated quantities {
  matrix[m, m] Sigma = L_Sigma * L_Sigma';
}
"

set.seed(123)
sm <- stan_model(model_code = stan_code)

# 4.4 Rekurzivní odhad BVAR-X (expanding window) ###############################

t0 <- max(30, 2)
t_end_seq <- t0:T
nW <- length(t_end_seq)

chains  <- 2
iter    <- 2000
warmup  <- 1000
ctrl    <- list(adapt_delta = 0.90, max_treedepth = 12)

pb <- txtProgressBar(min = 0, max = nW, style = 3)

rec <- vector("list", nW)
autosave_file <- "bvarx_recursive_cache.rds"

muffle_built_under <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("built under R version", conditionMessage(w))) {
        try(invokeRestart("muffleWarning"), silent = TRUE)
      }
    }
  )
}

for (k in seq_along(t_end_seq)) {
  t_end <- t_end_seq[k]
  
  Y_t    <- Y[    1:t_end, , drop = FALSE]
  Ylag_t <- Ylag[ 1:t_end, , drop = FALSE]
  X_t    <- X[    1:t_end, , drop = FALSE]
  
  stan_data <- list(
    T = nrow(Y_t), m = m, kx = kx,
    Y = Y_t, Ylag = Ylag_t, X = X_t,
    A_sd = A_sd, B_sd = B_sd,
    c_sd = rep(sd_const, m),
    lkj_eta = 2
  )
  
  fit_t <- muffle_built_under(
    sampling(sm, data = stan_data,
             chains = chains, iter = iter, warmup = warmup,
             seed = 123 + t_end, refresh = 0, control = ctrl)
  )
  summary_fit <- rstan::summary(fit_t)$summary
  max_rhat <- max(summary_fit[, "Rhat"], na.rm = TRUE)
  min_neff <- min(summary_fit[, "n_eff"], na.rm = TRUE)
  
  sampler_params <- get_sampler_params(fit_t, inc_warmup = FALSE) 
  divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))

  post   <- rstan::extract(fit_t, pars = c("A","B","Sigma"))
  A_med  <- apply(post$A,       c(2,3), median)
  B_med  <- apply(post$B,       c(2,3), median)
  SigmaM <- apply(post$Sigma, c(2,3), median)
  
  rho <- max(Mod(eigen(A_med, only.values = TRUE)$values))
  
  A_x_p   <- A_med[ix_x, match("l_p_gap",   Y_names)]
  A_x_e   <- A_med[ix_x, match("l_e_gap",   Y_names)]
  A_x_ulc <- A_med[ix_x, match("l_ulc_gap", Y_names)]
  A_x_x   <- A_med[ix_x, match("l_x_gap",   Y_names)]
  
  B_x_y   <- B_med[ix_x, match("l_y_gap",   X_names)]
  B_x_m   <- B_med[ix_x, match("l_m_gap",   X_names)]
  
  A0_inv <- t(chol(SigmaM))
  b0_x_p   <- A0_inv[ix_x, match("l_p_gap",   Y_names)]
  b0_x_e   <- A0_inv[ix_x, match("l_e_gap",   Y_names)]
  b0_x_ulc <- A0_inv[ix_x, match("l_ulc_gap", Y_names)]
  b0_x_x   <- A0_inv[ix_x, match("l_x_gap",   Y_names)]
  
  rec[[k]] <- data.frame(
    t_end = t_end, rho = rho,
    max_rhat = max_rhat, 
    min_neff = min_neff, 
    divergences = divergences,
    
    A_x_p = A_x_p, A_x_e = A_x_e, A_x_ulc = A_x_ulc, A_x_x = A_x_x,
    B_x_y = B_x_y, B_x_m = B_x_m,
    b0_x_p = b0_x_p, b0_x_e = b0_x_e, b0_x_ulc = b0_x_ulc, b0_x_x = b0_x_x
  )
  if (k %% 5 == 0) saveRDS(rec[1:k], autosave_file)
  
  setTxtProgressBar(pb, k)
}
close(pb)

rec_results <- do.call(rbind, rec)

# 4.5 Náhled výsledků rekurzivního odhadu ######################################

print(head(rec_results, 3))
print(tail(rec_results, 3))

# 4.6 Odhad modelu na celém vzorku #############################################

final_stan_data <- list(
  T = T, m = m, kx = kx,
  Y = Y, Ylag = Ylag, X = X, 
  A_sd = A_sd, B_sd = B_sd,
  c_sd = rep(sd_const, m),
  lkj_eta = 2
)

fit_final <- muffle_built_under(
  sampling(sm, data = final_stan_data,
           chains = chains, iter = iter, warmup = warmup,
           seed = 123 + T, 
           control = ctrl)
)

print(fit_final, pars = "lp__")
check_hmc_diagnostics(fit_final)

cat("Neomezený model úspěšně odhadnut\n")

################################################################################
# 5 Prezentace výsledků neomzeneého modelu #####################################

# 5.1 Výpis výsledků neomezeného modelu ########################################

summary_obj <- rstan::summary(fit_final, 
                              pars = c("A", "B", "c"), 
                              probs = c(0.05, 0.5, 0.95))

summary_table_data <- summary_obj$summary

columns_to_keep <- c("mean", "sd", "5%", "50%", "95%", "n_eff", "Rhat")
final_table_data <- summary_table_data[, columns_to_keep]

colnames(final_table_data) <- c("Prumer", "SD", "5.kvantil", "Median", 
                                "95.kvantil", "n_eff", "Rhat")

simple_text_table <- kable(final_table_data, 
                           format = "simple", 
                           digits = c(2, 2, 2, 2, 2, 0, 2))

writeLines(simple_text_table, "Tabs/bvar_summary_table_neomezeny.txt")

post_sig <- rstan::extract(fit_final, pars = "Sigma")$Sigma
n_draws  <- dim(post_sig)[1]
m        <- dim(post_sig)[2]
P_draws <- array(NA_real_, dim = c(n_draws, m, m),
                 dimnames = list(NULL, Y_names, Y_names))
for (d in 1:n_draws) {
  P_draws[d,,] <- t(chol(post_sig[d,,]))
}
ix_x <- match("l_x_gap", Y_names)
stopifnot(!is.na(ix_x))
b0_x_mat <- P_draws[, ix_x, , drop = TRUE]
colnames(b0_x_mat) <- Y_names
b0_x_draws <- posterior::as_draws_matrix(b0_x_mat)
out <- data.frame(
  Determinant = Y_names,
  Prumer      = apply(b0_x_mat, 2, mean),
  Median      = apply(b0_x_mat, 2, median),
  SD          = apply(b0_x_mat, 2, stats::sd),
  stringsAsFactors = FALSE
)

write.table(out,
            file = "Tabs/cholesky_impact_on_l_x_gap_neomezeny.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

# 5.2 Posteriorní hustoty neomezeného modelu ###################################

paleta_fill_B <- c("l_m_gap" = "#9cc5b4", "l_y_gap" = "#e6cbc4")
paleta_color_B <- c("l_m_gap" = "#1c9e77", "l_y_gap" = "#d96005")
paleta_fill_A <- c("l_p_gap" = "#e6cbc4", "l_e_gap" = "#d5d3ec", 
                   "l_ulc_gap" = "#9cc5b4", "l_x_gap" = "#e9cbd5") 
paleta_color_A <- c("l_p_gap" = "#d96005", "l_e_gap" = "#746fb3", 
                    "l_ulc_gap" = "#1c9e77", "l_x_gap" = "#e72b8a") 
color_vline_zero <- "grey40"

theme_final_style <- function() {
  theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      axis.title.x = element_text(size = 14, face = "bold", 
                                  margin = margin(t = 10)), 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(face = "bold", size = 12, 
                                 vjust = 0.5, hjust = 1), 
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "none" 
    )
}

m <- 4
kx <- 2
ix_x <- 4
Y_names <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")
X_names <- c("l_y_gap","l_m_gap")

posterior_draws <- rstan::extract(fit_final)
post_A_x <- posterior_draws$A[, ix_x, ]
post_B_x <- posterior_draws$B[, ix_x, ]
pars_A_x <- paste0("A[", ix_x, ",", 1:m, "]")
pars_B_x <- paste0("B[", ix_x, ",", 1:kx, "]")
colnames(post_A_x) <- pars_A_x
colnames(post_B_x) <- pars_B_x

df_A_long <- as.data.frame(post_A_x) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  mutate(Type = "A")
df_B_long <- as.data.frame(post_B_x) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  mutate(Type = "B")

labels_A_short <- setNames(Y_names, pars_A_x)
labels_B_short <- setNames(X_names, pars_B_x)
all_labels_short <- c(labels_A_short, labels_B_short)

df_long <- bind_rows(df_A_long, df_B_long) %>%
  mutate(Parameter = recode_factor(Parameter, !!!all_labels_short))

create_ridge_plot <- function(data, fill_pal, color_pal, title_text) {
  ggplot(data, aes(y = Parameter, x = Value)) +
    geom_density_ridges(
      aes(fill = Parameter, color = Parameter),
      alpha = 0.8, linewidth = 1.2, rel_min_height = 0.01,
      scale = 2, quantile_lines = FALSE 
    ) +
    scale_fill_manual(values = fill_pal, guide = "none") +
    scale_color_manual(values = color_pal, guide = "none") +
    ggnewscale::new_scale_color() + 
    stat_density_ridges(
      aes(color = Parameter), fill = NA, quantile_lines = TRUE, 
      quantiles = c(0.05, 0.5, 0.95), linetype = "dashed", 
      linewidth = 1.0, scale = 2, rel_min_height = 0.01
    ) +
    scale_linetype_manual(values = c("dashed", "solid", "dashed"), 
                          guide = "none") + 
    scale_color_manual(values = color_pal, guide = "none") + 
    geom_vline(xintercept = 0, linetype = "dashed", color = color_vline_zero) +
    theme_final_style() +
    labs(
      title = title_text,
      subtitle = "Neomezený model",
      x = "Hodnota koeficientu",
      y = ""
    ) +
    coord_cartesian(clip = "off") 
}

plot_B_post <- df_long %>%
  filter(Type == "B") %>%
  mutate(Parameter = factor(Parameter, levels = c("l_m_gap", "l_y_gap"))) %>%
  create_ridge_plot(
    fill_pal = paleta_fill_B,
    color_pal = paleta_color_B,
    title_text = "Posteriorní hustoty koeficientů B"
  )
print(plot_B_post)

plot_A_post <- df_long %>%
  filter(Type == "A") %>%
  create_ridge_plot(
    fill_pal = paleta_fill_A,
    color_pal = paleta_color_A,
    title_text = "Posteriorní hustoty koeficientů A"
  )
print(plot_A_post)

ggsave(
  filename = "Plots/posterior_B_neomezeny.png",
  plot = plot_B_post,
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  filename = "Plots/posterior_A_neomezeny.png",
  plot = plot_A_post,
  width = 10,
  height = 8,
  dpi = 300
)

# 5.3 Graf koeficientů rekurzivního odhadu #####################################

recessions_df <- data.frame(
  start = as.Date(c("2008-01-01", "2012-01-01", "2020-01-01", "2023-01-01")),
  end = as.Date(c("2009-12-31", "2013-12-31", "2020-12-31", "2023-12-31"))
)

Y_names <- c("l_p_gap", "l_e_gap", "l_ulc_gap", "l_x_gap")
X_names <- c("l_y_gap", "l_m_gap")

D_with_dates <- df[, c("Date", Y_names, X_names)] 
D_with_dates_complete <- D_with_dates[complete.cases(D_with_dates), , 
                                      drop = FALSE]

full_dates <- D_with_dates_complete$Date
T_val <- length(full_dates) - 1 

date_map <- data.frame(
  t_end = 1:T_val, 
  Date = full_dates[2:(T_val + 1)] 
)

rec_results_with_date <- rec_results %>%
  left_join(date_map, by = "t_end") %>%
  filter(!is.na(Date)) 

coeff_titles_A <- c(
  "A_x_x"   = "Perzistence l_x_gap",
  "A_x_p"   = "Vliv l_p_gap",
  "A_x_e"   = "Vliv l_e_gap",
  "A_x_ulc" = "Vliv l_ulc_gap"
)
vars_A <- names(coeff_titles_A)

coeff_titles_B <- c(
  "B_x_y"   = "Vliv l_y_gap",
  "B_x_m"   = "Vliv l_m_gap"
)
vars_B <- names(coeff_titles_B)

coeff_titles_b0 <- c(
  "b0_x_x"  = "Okamžitý šok 'x' na 'x'",
  "b0_x_p"  = "Okamžitý šok 'p' na 'x'",
  "b0_x_e"  = "Okamžitý šok 'e' na 'x'",
  "b0_x_ulc"= "Okamžitý šok 'ulc' na 'x'"
)
vars_b0 <- names(coeff_titles_b0)

create_coeff_plot <- function(data, vars_to_plot, titles_map, 
                              n_col, plot_title) {
  
  rec_long <- data %>%
    select(Date, all_of(vars_to_plot)) %>%
    pivot_longer(
      cols = -Date,
      names_to = "Variable_Name",
      values_to = "Value"
    ) %>%
    mutate(Variable_Title = recode(Variable_Name, !!!titles_map))
  
  rec_long$Variable_Title <- factor(rec_long$Variable_Title,
                                    levels = titles_map)
  
  ggplot(rec_long, aes(x = Date, y = Value, color = Variable_Title)) +
    geom_rect(
      data = recessions_df,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey80", alpha = 0.3, inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(size = 1.1) +
    facet_wrap(~ Variable_Title, ncol = n_col, scales = "free_y") +
    scale_color_brewer(palette = "Set1") + 
    scale_x_date(
      date_breaks = "2 year", 
      date_labels = "%Y",
      expand = c(0.01, 0.01)
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = plot_title,
      subtitle = "Neomezený model",
      x = "Rok (konec odhadovaného okna)",
      y = "Hodnota koeficientu"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30"),
      legend.position = "none", 
      strip.text = element_text(face = "bold", size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_A <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_A,
  titles_map = coeff_titles_A,
  n_col = 2,
  plot_title = "Vývoj koeficientů - vliv zpoždění"
)
print(plot_A)

plot_B <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_B,
  titles_map = coeff_titles_B,
  n_col = 2,
  plot_title = "Vývoj koeficientů - vliv exogenních proměnných"
)
print(plot_B)

plot_b0 <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_b0,
  titles_map = coeff_titles_b0,
  n_col = 2,
  plot_title = "Vývoj koeficientů - okamžité dopady šoků"
)
print(plot_b0)

ggsave(
  filename = "Plots/coeffs_A_neomezeny.png",
  plot = plot_A,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  filename = "Plots/coeffs_B_neomezeny.png",
  plot = plot_B,
  width = 10,
  height = 5,
  dpi = 300
)
ggsave(
  filename = "Plots/coeffs_Cholesky_neomezeny.png",
  plot = plot_b0,
  width = 10,
  height = 8,
  dpi = 300
)

# 5.4 IRF ######################################################################

post_final <- rstan::extract(fit_final, pars = c("A", "Sigma"))
A_draws <- post_final$A
Sigma_draws <- post_final$Sigma
H <- 12
n_draws <- dim(A_draws)[1]
irf_draws_array <- array(NA, dim = c(H + 1, m, m, n_draws))

for (d in 1:n_draws) {
  A_d <- A_draws[d, , ]
  Sigma_d <- Sigma_draws[d, , ]
  P_d <- t(chol(Sigma_d))
  Theta_h_minus_1 <- P_d
  irf_draws_array[1, , , d] <- Theta_h_minus_1
  for (h in 1:H) {
    Theta_h <- A_d %*% Theta_h_minus_1
    irf_draws_array[h + 1, , , d] <- Theta_h
    Theta_h_minus_1 <- Theta_h
  }
}

irf_median <- apply(irf_draws_array, c(1, 2, 3), median)
irf_q16 <- apply(irf_draws_array, c(1, 2, 3), quantile, probs = 0.16, 
                 na.rm = TRUE)
irf_q84 <- apply(irf_draws_array, c(1, 2, 3), quantile, probs = 0.84, 
                 na.rm = TRUE)

dimnames(irf_median) <- list(horizon = 0:H, response = Y_names, shock = Y_names)
dimnames(irf_q16) <- dimnames(irf_median)
dimnames(irf_q84) <- dimnames(irf_median)

df_median <- as.data.frame.table(irf_median, responseName = "median")
df_q16 <- as.data.frame.table(irf_q16, responseName = "q16")
df_q84 <- as.data.frame.table(irf_q84, responseName = "q84")

irf_plot_data <- df_median %>%
  left_join(df_q16, by = c("horizon", "response", "shock")) %>%
  left_join(df_q84, by = c("horizon", "response", "shock"))
irf_plot_data$horizon <- as.numeric(as.character(irf_plot_data$horizon))

post_final_AB <- rstan::extract(fit_final, pars = c("A", "B"))
A_draws_x <- post_final_AB$A
B_draws_x <- post_final_AB$B
n_draws_x <- dim(A_draws_x)[1]
irf_x_array <- array(NA, dim = c(H + 1, m, kx, n_draws_x))
for (d in 1:n_draws_x) {
  A_d <- A_draws_x[d, , ]
  B_d <- B_draws_x[d, , ]
  irf_x_array[1, , , d] <- B_d
  Theta_h_minus_1 <- B_d
  for (h in 1:H) {
    Theta_h <- A_d %*% Theta_h_minus_1
    irf_x_array[h + 1, , , d] <- Theta_h
    Theta_h_minus_1 <- Theta_h
  }
}
irf_x_median <- apply(irf_x_array, c(1, 2, 3), median)
irf_x_q16 <- apply(irf_x_array, c(1, 2, 3), quantile, probs = 0.16, na.rm = TRUE)
irf_x_q84 <- apply(irf_x_array, c(1, 2, 3), quantile, probs = 0.84, na.rm = TRUE)
dimnames(irf_x_median) <- list(horizon = 0:H, response = Y_names, 
                               shock_X = X_names)
dimnames(irf_x_q16) <- dimnames(irf_x_median)
dimnames(irf_x_q84) <- dimnames(irf_x_median)

df_x_median <- as.data.frame.table(irf_x_median, responseName = "median")
df_x_q16 <- as.data.frame.table(irf_x_q16, responseName = "q16")
df_x_q84 <- as.data.frame.table(irf_x_q84, responseName = "q84")

irf_x_plot_data <- df_x_median %>%
  left_join(df_x_q16, by = c("horizon", "response", "shock_X")) %>%
  left_join(df_x_q84, by = c("horizon", "response", "shock_X"))
irf_x_plot_data$horizon <- as.numeric(as.character(irf_x_plot_data$horizon))

variables_with_titles_gap <- c(
  "l_x_gap" = "Export",
  "l_y_gap" = "Zahraniční poptávka",
  "l_e_gap" = "Směnný kurz",
  "l_p_gap" = "Inflační diferenciál",
  "l_ulc_gap" = "Jednotkové náklady práce",
  "l_m_gap" = "Index importních cen"
)
df_irf_combined <- bind_rows(
  irf_plot_data %>%
    filter(response == "l_x_gap") %>%
    rename(shock_variable = shock),
  irf_x_plot_data %>%
    filter(response == "l_x_gap") %>%
    rename(shock_variable = shock_X)
)
df_plot_final <- df_irf_combined %>%
  mutate(
    Shock_Title = recode(shock_variable, !!!variables_with_titles_gap),
    Shock_Title = factor(Shock_Title, levels = variables_with_titles_gap)
  ) %>%
  filter(!is.na(Shock_Title))
combined_irf_plot_h12 <- df_plot_final %>%
  filter(horizon <= 12) %>%
  ggplot(aes(x = horizon)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(
    aes(ymin = q16, ymax = q84, fill = Shock_Title), 
    alpha = 0.5
  ) +
  geom_line(
    aes(y = median, color = Shock_Title), 
    linewidth = 1.2
  ) +
  facet_wrap(~ Shock_Title, ncol = 3, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    #title = "Dopad šoků na l_x_gap",
    subtitle = "Neomezený model (Medián a 68% CI)",
    x = "Čtvrtletí",
    y = "Reakce (v %) odchylka od steady-state"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )
print(combined_irf_plot_h12)

ggsave(
  filename = "Plots/irf_neomezeny.png",
  plot = combined_irf_plot_h12,
  width = 13,
  height = 8,
  dpi = 300
)

# 5.5 Graf stability modelu (rho v čase) #######################################

time_axis_full <- as.Date(as.yearqtr(time(model_data_ts)[2:(T + 1)]))
plot_data_rho <- rec_results %>%
  mutate(date = time_axis_full[t_end])
p_rho_neomezeny <- ggplot(plot_data_rho, aes(x = date, y = rho)) +
  geom_line(color = "#01655E", linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    #title = "Vývoj stability modelu v čase",
    subtitle = "Neomezený model",
    x = "Poslední pozorování v okně",
    y = "Hodnota ρ"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p_rho_neomezeny)

ggsave(
  filename = "Plots/rho_neomezeny.png",
  plot = p_rho_neomezeny,
  width = 10,
  height = 6,
  dpi = 300
)

# 5.6 Historická šoková dekompozice ############################################

post_full <- rstan::extract(fit_final, pars = c("A", "B", "c", "Sigma"))
A_draws <- post_full$A
B_draws <- post_full$B
c_draws <- post_full$c
Sigma_draws <- post_full$Sigma
n_draws <- dim(A_draws)[1]
n_comp_shocks <- m
n_comp_exo <- kx
n_comp_base <- 1
n_components <- n_comp_base + n_comp_shocks + n_comp_exo
component_names <- c("Baseline_Det",
                     paste0("Shock_", Y_names), 
                     paste0("Exo_", X_names))

hsd_draws_array <- array(NA,
                         dim = c(n_draws, T, m, n_components),
                         dimnames = list(
                           Draw = 1:n_draws,
                           Time = 1:T,
                           Variable = Y_names,
                           Component = component_names
                         )
)
pb_hsd <- txtProgressBar(min = 0, max = n_draws, style = 3)
for (d in 1:n_draws) {
  A_d <- A_draws[d, , ]
  B_d <- B_draws[d, , ]
  c_d <- c_draws[d, ]
  Sigma_d <- Sigma_draws[d, , ]
  P_d <- t(chol(Sigma_d))
  P_inv_d <- solve(P_d)
  structural_shocks_d <- matrix(NA, T, m)
  irf_matrices_d <- array(NA, dim = c(T, m, m))
  det_baseline_path_d <- matrix(NA, T + 1, m)
  det_baseline_path_d[1, ] <- Y_full[1, ]
  exo_contrib_path_d <- array(NA, dim = c(T + 1, m, kx))
  exo_contrib_path_d[1, , ] <- 0
  Theta_h_minus_1 <- P_d
  irf_matrices_d[1, , ] <- P_d
  for (t in 1:T) {
    Y_t <- Y_full[t + 1, ]
    Y_t_minus_1 <- Y_full[t, ]
    X_t <- X[t, ]
    u_t <- Y_t - (c_d + A_d %*% Y_t_minus_1 + B_d %*% X_t)
    e_t <- P_inv_d %*% u_t
    structural_shocks_d[t, ] <- e_t
    det_baseline_path_d[t + 1, ] <- c_d + A_d %*% det_baseline_path_d[t, ]
    for (j in 1:kx) {
      exo_contrib_path_d[t + 1, , j] <- A_d %*% exo_contrib_path_d[t, , j] + 
        B_d[, j] * X[t, j]
    }
    if (t > 1) {
      Theta_h <- A_d %*% Theta_h_minus_1
      irf_matrices_d[t, , ] <- Theta_h
      Theta_h_minus_1 <- Theta_h
    }
  }
  hsd_draws_array[d, , , "Baseline_Det"] <- det_baseline_path_d[2:(T + 1), ]
  for (k in 1:kx) {
    comp_name_exo <- paste0("Exo_", X_names[k])
    hsd_draws_array[d, , , comp_name_exo] <- exo_contrib_path_d[2:(T + 1), , k]
  }
  for (t in 1:T) {
    for (j in 1:m) {
      contribution_j_t <- matrix(0, m, 1)
      for (h in 0:(t - 1)) {
        Theta_h <- irf_matrices_d[h + 1, , ]
        shock_j_t_minus_h <- structural_shocks_d[t - h, j]
        contribution_j_t <- contribution_j_t + Theta_h[, j] * shock_j_t_minus_h
      }
      comp_name_shock <- paste0("Shock_", Y_names[j])
      hsd_draws_array[d, t, , comp_name_shock] <- contribution_j_t
    }
  }
  setTxtProgressBar(pb_hsd, d)
}
close(pb_hsd)

hsd_median <- apply(hsd_draws_array, c(2, 3, 4), median, na.rm = TRUE)

hsd_plot_data_wide <- as.data.frame.table(hsd_median, responseName = "Value")
colnames(hsd_plot_data_wide) <- c("Time_Idx", "Variable", "Component", "Value")
hsd_plot_data_wide$Time_Idx <- as.numeric(hsd_plot_data_wide$Time_Idx)

Y_time_axis_date <- as.Date(as.yearqtr(time(model_data_ts)[2:(T + 1)]))
date_map <- data.frame(Time_Idx = 1:T, Date = Y_time_axis_date)

hsd_plot_data <- hsd_plot_data_wide %>%
  left_join(date_map, by = "Time_Idx")

observed_data_long <- as.data.frame(Y_full[2:(T + 1), ])
colnames(observed_data_long) <- Y_names
observed_data_long$Time_Idx <- 1:T
observed_data_long <- observed_data_long %>%
  pivot_longer(cols = -Time_Idx, names_to = "Variable", values_to = "Observed_Y")

hsd_plot_data <- hsd_plot_data %>%
  left_join(observed_data_long, by = c("Time_Idx", "Variable"))

var_to_plot <- "l_x_gap"
plot_data_hsd_final <- hsd_plot_data %>%
  filter(Variable == var_to_plot)

plot_data_hsd_final$Component <- factor(
  plot_data_hsd_final$Component, 
  levels = component_names
)

shock_labels <- paste("Šok:", variables_with_titles_gap[Y_names])
names(shock_labels) <- paste0("Shock_", Y_names)
exo_labels <- paste("Vliv:", variables_with_titles_gap[X_names])
names(exo_labels) <- paste0("Exo_", X_names)
comp_labels <- c(
  "Baseline_Det" = "Determ. baseline",
  shock_labels,
  exo_labels
)
colors_hsd_final <- c(
  "Baseline_Det"      = "grey80",
  "Shock_l_p_gap"     = "#E41A1C",
  "Shock_l_e_gap"     = "#377EB8",
  "Shock_l_ulc_gap"   = "#4DAF4A",
  "Shock_l_x_gap"     = "#984EA3",
  "Exo_l_y_gap"       = "#FF7F00",
  "Exo_l_m_gap"       = "#A65628" 
)

p_hsd_new <- ggplot(plot_data_hsd_final, aes(x = Date)) +
  geom_col(aes(y = Value, fill = Component), position = "stack", width = 80) +
  geom_line(aes(y = Observed_Y), color = "black", linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  scale_fill_manual(
    values = colors_hsd_final, 
    labels = comp_labels,
    name = "Příspěvek komponenty:",
    breaks = component_names 
  ) +
  scale_x_date(
    date_breaks = "2 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    #title = paste("Historická šoková dekompozice pro", 
    #              variables_with_titles_gap[var_to_plot]),
    subtitle = "Černá čára: Pozorovaná data. Sloupce: Příspěvky komponent.",
    x = "Datum",
    y = "Hodnota (odchylka od trendu)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
print(p_hsd_new)

ggsave(
  filename = "Plots/sokova_dekompozice_neomezeny.png",
  plot = p_hsd_new,
  width = 12,
  height = 8,
  dpi = 300
)

# 5.7 MCMC diagnostika - traceplots ############################################

post_draws_final <- as.array(fit_final)
pars_to_check <- c("lp__", 
                   "A[1,1]", "A[4,4]",  # Vlastní zpoždění l_p_gap a l_x_gap
                   "B[4,1]", "B[4,2]",  # Vliv l_y_gap a l_m_gap na l_x_gap
                   "sigma[1]", "sigma[4]") # Volatility l_p_gap a l_x_gap

nazev_p_gap <- variables_with_titles_gap["l_p_gap"]
nazev_x_gap <- variables_with_titles_gap["l_x_gap"]
nazev_y_gap <- variables_with_titles_gap["l_y_gap"]
nazev_m_gap <- variables_with_titles_gap["l_m_gap"]

trace_labels <- c(
  "lp__"     = "Log-posterior (model)",
  "A[1,1]"   = paste("AR(1)", nazev_p_gap),
  "A[4,4]"   = paste("AR(1)", nazev_x_gap),
  "B[4,1]"   = paste("Vliv:", nazev_y_gap),
  "B[4,2]"   = paste("Vliv:", nazev_m_gap),
  "sigma[1]" = paste("Sigma:", nazev_p_gap),
  "sigma[4]" = paste("Sigma:", nazev_x_gap)
)
chain_colors <- brewer.pal(3, "Set1")[1:2]
p_trace_custom <- mcmc_trace(
  post_draws_final, 
  pars = pars_to_check,
  facet_args = list(
    ncol = 3,
    labeller = labeller(.default = trace_labels, .multi_line = FALSE)
  )
) +
  scale_color_manual(values = chain_colors, name = "MCMC řetězec:") +
  labs(
    #title = "MCMC Trace Plots pro vybrané parametry",
    subtitle = "Neomezený model") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9), 
    panel.grid.minor = element_blank()
  )
print(p_trace_custom)

ggsave(
  filename = "Plots/mcmc_trace_neomezeny.png",
  plot = p_trace_custom,
  width = 12,
  height = 8,
  dpi = 300
)

################################################################################
# 6. Rekurzivní odhad omezeného modelu #########################################

vars <- c("l_y_gap", "l_p_gap", "l_e_gap", "l_ulc_gap", "l_m_gap", "l_x_gap")
model_data_ts <- ts(
  data = as.matrix(df[, vars]),
  start = c(2000, 1),
  frequency = 4
)
colnames(model_data_ts) <- vars

#head(model_data_ts)

# 6.1 Příprava dat pro BVAR-X ##################################################

Y_names <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")
X_names <- c("l_y_gap","l_m_gap")                      

stopifnot(exists("df"))

df_precovid <- df %>%
  filter(Date < as.Date("2020-01-01"))

D <- df_precovid[, c(Y_names, X_names)]
D <- D[complete.cases(D), , drop = FALSE]

Y_full <- as.matrix(D[, Y_names, drop = FALSE])
X_full <- as.matrix(D[, X_names, drop = FALSE])

T  <- nrow(Y_full) - 1
m  <- ncol(Y_full)
kx <- ncol(X_full)

Y    <- Y_full[2:(T+1), , drop = FALSE]   # Y_t
Ylag <- Y_full[1:T,     , drop = FALSE]   # Y_{t-1}
X    <- X_full[2:(T+1), , drop = FALSE]   # X_t

ix_x <- match("l_x_gap", Y_names)        

# 6.2 Nastavení priorů #########################################################

lambda_1 <- 0.1
lambda_2 <- 0.5
lambda_4 <- 1.0
sd_const <- 1.0
sd_theory <- 0.25

sd_own   <- lambda_1
sd_cross <- lambda_1 * lambda_2
sd_exo   <- lambda_4

A_sd <- matrix(sd_cross, m, m); diag(A_sd) <- sd_own
B_sd <- matrix(sd_exo,   m, kx)

idx_i_export <- match("l_x_gap", Y_names)
idx_j_price  <- match("l_p_gap", Y_names)
idx_j_ulc    <- match("l_ulc_gap", Y_names)

A_sd[idx_i_export, idx_j_price] <- sd_theory
A_sd[idx_i_export, idx_j_ulc]   <- sd_theory

# 6.3 Stan model ###############################################################

suppressPackageStartupMessages(library(rstan))
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = max(1, parallel::detectCores() - 1))

stan_code <- "
data {
  int<lower=1> T; int<lower=1> m; int<lower=0> kx;
  matrix[T, m] Y; matrix[T, m] Ylag; matrix[T, kx] X;
  matrix[m, m] A_sd; matrix[m, kx] B_sd; vector[m] c_sd;
  real<lower=0> lkj_eta;
}
parameters {
  matrix[m, m] A;                 // Y_{t-1}
  matrix[m, kx] B;                // X_t
  vector[m] c;                    // konstanty
  vector<lower=0>[m] sigma;
  cholesky_factor_corr[m] L_Omega;
}
transformed parameters {
  matrix[m, m] L_Sigma = diag_pre_multiply(sigma, L_Omega);
}
model {
  to_vector(A) ~ normal(0, to_vector(A_sd));
  to_vector(B) ~ normal(0, to_vector(B_sd));
  c ~ normal(0, c_sd);
  sigma ~ cauchy(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(lkj_eta);

  for (t in 1:T) {
    vector[m] mu = c + A * to_vector(Ylag[t]') + B * to_vector(X[t]');
    Y[t]' ~ multi_normal_cholesky(mu, L_Sigma);
  }
}
generated quantities {
  matrix[m, m] Sigma = L_Sigma * L_Sigma';
}
"

set.seed(123)
sm <- stan_model(model_code = stan_code)

# 6.4 Rekurzivní odhad BVAR-X (expanding window) ###############################

t0 <- max(30, 2)
t_end_seq <- t0:T
nW <- length(t_end_seq)

chains  <- 2
iter    <- 2000
warmup  <- 1000
ctrl    <- list(adapt_delta = 0.90, max_treedepth = 12)

pb <- txtProgressBar(min = 0, max = nW, style = 3)

rec_precovid <- vector("list", nW)
autosave_file_precovid <- "bvarx_recursive_cache_precovid.rds"

muffle_built_under <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("built under R version", conditionMessage(w))) {
        try(invokeRestart("muffleWarning"), silent = TRUE)
      }
    }
  )
}

for (k in seq_along(t_end_seq)) {
  t_end <- t_end_seq[k]
  
  Y_t    <- Y[    1:t_end, , drop = FALSE]
  Ylag_t <- Ylag[ 1:t_end, , drop = FALSE]
  X_t    <- X[    1:t_end, , drop = FALSE]
  
  stan_data <- list(
    T = nrow(Y_t), m = m, kx = kx,
    Y = Y_t, Ylag = Ylag_t, X = X_t,
    A_sd = A_sd, B_sd = B_sd,
    c_sd = rep(sd_const, m),
    lkj_eta = 2
  )
  
  fit_t <- muffle_built_under(
    sampling(sm, data = stan_data,
             chains = chains, iter = iter, warmup = warmup,
             seed = 123 + t_end, refresh = 0, control = ctrl)
  )
  
  summary_fit <- rstan::summary(fit_t)$summary
  max_rhat <- max(summary_fit[, "Rhat"], na.rm = TRUE)
  min_neff <- min(summary_fit[, "n_eff"], na.rm = TRUE)
  
  sampler_params <- get_sampler_params(fit_t, inc_warmup = FALSE) 
  divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  
  post   <- rstan::extract(fit_t, pars = c("A","B","Sigma"))
  A_med  <- apply(post$A,       c(2,3), median)
  B_med  <- apply(post$B,       c(2,3), median)
  SigmaM <- apply(post$Sigma, c(2,3), median)
  
  rho <- max(Mod(eigen(A_med, only.values = TRUE)$values))
  
  A_x_p   <- A_med[ix_x, match("l_p_gap",   Y_names)]
  A_x_e   <- A_med[ix_x, match("l_e_gap",   Y_names)]
  A_x_ulc <- A_med[ix_x, match("l_ulc_gap", Y_names)]
  A_x_x   <- A_med[ix_x, match("l_x_gap",   Y_names)]
  
  B_x_y   <- B_med[ix_x, match("l_y_gap",   X_names)]
  B_x_m   <- B_med[ix_x, match("l_m_gap",   X_names)]
  
  A0_inv <- t(chol(SigmaM))
  b0_x_p   <- A0_inv[ix_x, match("l_p_gap",   Y_names)]
  b0_x_e   <- A0_inv[ix_x, match("l_e_gap",   Y_names)]
  b0_x_ulc <- A0_inv[ix_x, match("l_ulc_gap", Y_names)]
  b0_x_x   <- A0_inv[ix_x, match("l_x_gap",   Y_names)]
  
  rec_precovid[[k]] <- data.frame(
    t_end = t_end, rho = rho,
    
    max_rhat = max_rhat, 
    min_neff = min_neff, 
    divergences = divergences,
    
    A_x_p = A_x_p, A_x_e = A_x_e, A_x_ulc = A_x_ulc, A_x_x = A_x_x,
    B_x_y = B_x_y, B_x_m = B_x_m,
    b0_x_p = b0_x_p, b0_x_e = b0_x_e, b0_x_ulc = b0_x_ulc, b0_x_x = b0_x_x
  )
  if (k %% 5 == 0) saveRDS(rec_precovid[1:k], autosave_file_precovid)
  
  setTxtProgressBar(pb, k)
}
close(pb)

rec_results_precovid <- do.call(rbind, rec_precovid)

# 6.5 Náhled výsledků rekurzivního odhadu ######################################

print(head(rec_results_precovid, 3))
print(tail(rec_results_precovid, 3))

# 6.6 Odhad modelu na celém vzorku #############################################

final_stan_data <- list(
  T = T, m = m, kx = kx,
  Y = Y, Ylag = Ylag, X = X, 
  A_sd = A_sd, B_sd = B_sd,
  c_sd = rep(sd_const, m),
  lkj_eta = 2
)

fit_final_precovid <- muffle_built_under(
  sampling(sm, data = final_stan_data,
           chains = chains, iter = iter, warmup = warmup,
           seed = 123 + T, 
           control = ctrl)
)

print(fit_final_precovid, pars = "lp__")
check_hmc_diagnostics(fit_final_precovid)

cat("Omezený model úspěšně odhadnut\n")

################################################################################
# 7 Prezentace výsledků omezeného modelu #######################################

# 7.1 Výpis výsledků omezeného modelu ##########################################

summary_obj_omezeny <- rstan::summary(fit_final_precovid, # <-- ZMĚNA
                                      pars = c("A", "B", "c"), 
                                      probs = c(0.05, 0.5, 0.95))

summary_table_data_omezeny <- summary_obj_omezeny$summary

columns_to_keep <- c("mean", "sd", "5%", "50%", "95%", "n_eff", "Rhat")
final_table_data_omezeny <- summary_table_data_omezeny[, columns_to_keep]

colnames(final_table_data_omezeny) <- c("Prumer", "SD", "5.kvantil", "Median", 
                                        "95.kvantil", "n_eff", "Rhat")

simple_text_table_omezeny <- kable(final_table_data_omezeny, 
                                   format = "simple", 
                                   digits = c(2, 2, 2, 2, 2, 0, 2))

writeLines(simple_text_table_omezeny, "Tabs/bvar_summary_table_omezeny.txt")

# 7.2 Posteriorní hustoty omezeného modelu ###################################

paleta_fill_B <- c("l_m_gap" = "#9cc5b4", "l_y_gap" = "#e6cbc4")
paleta_color_B <- c("l_m_gap" = "#1c9e77", "l_y_gap" = "#d96005")
paleta_fill_A <- c("l_p_gap" = "#e6cbc4", "l_e_gap" = "#d5d3ec", 
                   "l_ulc_gap" = "#9cc5b4", "l_x_gap" = "#e9cbd5") 
paleta_color_A <- c("l_p_gap" = "#d96005", "l_e_gap" = "#746fb3", 
                    "l_ulc_gap" = "#1c9e77", "l_x_gap" = "#e72b8a") 
color_vline_zero <- "grey40"

theme_final_style <- function() {
  theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      axis.title.x = element_text(size = 14, face = "bold", 
                                  margin = margin(t = 10)), 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(face = "bold", size = 12, vjust = 0.5, 
                                 hjust = 1), 
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "none" 
    )
}

posterior_draws <- rstan::extract(fit_final_precovid)
post_A_x <- posterior_draws$A[, ix_x, ]
post_B_x <- posterior_draws$B[, ix_x, ]
pars_A_x <- paste0("A[", ix_x, ",", 1:m, "]")
pars_B_x <- paste0("B[", ix_x, ",", 1:kx, "]")
colnames(post_A_x) <- pars_A_x
colnames(post_B_x) <- pars_B_x

df_A_long <- as.data.frame(post_A_x) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  mutate(Type = "A")
df_B_long <- as.data.frame(post_B_x) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  mutate(Type = "B")

labels_A_short <- setNames(Y_names, pars_A_x)
labels_B_short <- setNames(X_names, pars_B_x)
all_labels_short <- c(labels_A_short, labels_B_short)

df_long <- bind_rows(df_A_long, df_B_long) %>%
  mutate(Parameter = recode_factor(Parameter, !!!all_labels_short))

create_ridge_plot <- function(data, fill_pal, color_pal, title_text) {
  ggplot(data, aes(y = Parameter, x = Value)) +
    geom_density_ridges(
      aes(fill = Parameter, color = Parameter),
      alpha = 0.8, linewidth = 1.2, rel_min_height = 0.01,
      scale = 2, quantile_lines = FALSE 
    ) +
    scale_fill_manual(values = fill_pal, guide = "none") +
    scale_color_manual(values = color_pal, guide = "none") +
    ggnewscale::new_scale_color() + 
    stat_density_ridges(
      aes(color = Parameter), fill = NA, quantile_lines = TRUE, 
      quantiles = c(0.05, 0.5, 0.95), linetype = "dashed", 
      linewidth = 1.0, scale = 2, rel_min_height = 0.01
    ) +
    scale_linetype_manual(values = c("dashed", "solid", "dashed"), 
                          guide = "none") + 
    scale_color_manual(values = color_pal, guide = "none") + 
    geom_vline(xintercept = 0, linetype = "dashed", color = color_vline_zero) +
    theme_final_style() +
    labs(
      #title = title_text,
      subtitle = "Omezený model",
      x = "Hodnota koeficientu",
      y = ""
    ) +
    coord_cartesian(clip = "off") 
}

plot_B_post <- df_long %>%
  filter(Type == "B") %>%
  mutate(Parameter = factor(Parameter, levels = c("l_m_gap", "l_y_gap"))) %>%
  create_ridge_plot(
    fill_pal = paleta_fill_B,
    color_pal = paleta_color_B,
    title_text = "Posteriorní hustoty koeficientů B"
  )
print(plot_B_post)

plot_A_post <- df_long %>%
  filter(Type == "A") %>%
  create_ridge_plot(
    fill_pal = paleta_fill_A,
    color_pal = paleta_color_A,
    title_text = "Posteriorní hustoty koeficientů A"
  )
print(plot_A_post)

ggsave(
  filename = "Plots/posterior_B_omezeny.png",
  plot = plot_B_post,
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  filename = "Plots/posterior_A_omezeny.png",
  plot = plot_A_post,
  width = 10,
  height = 8,
  dpi = 300
)

# 7.3 Graf koeficientů rekurzivního odhadu #####################################

recessions_df <- data.frame(
  start = as.Date(c("2008-01-01", "2012-01-01", "2020-01-01", "2023-01-01")),
  end = as.Date(c("2009-12-31", "2013-12-31", "2020-12-31", "2023-12-31"))
)

D_with_dates <- df_precovid[, c("Date", Y_names, X_names)] 
D_with_dates_complete <- D_with_dates[complete.cases(D_with_dates), , 
                                      drop = FALSE]

full_dates <- D_with_dates_complete$Date
T_val <- length(full_dates) - 1 

date_map <- data.frame(
  t_end = 1:T_val, 
  Date = full_dates[2:(T_val + 1)] 
)

rec_results_with_date <- rec_results_precovid %>%
  left_join(date_map, by = "t_end") %>%
  filter(!is.na(Date)) 

coeff_titles_A <- c(
  "A_x_x"   = "Perzistence l_x_gap",
  "A_x_p"   = "Vliv l_p_gap",
  "A_x_e"   = "Vliv l_e_gap",
  "A_x_ulc" = "Vliv l_ulc_gap"
)
vars_A <- names(coeff_titles_A)

coeff_titles_B <- c(
  "B_x_y"   = "Vliv l_y_gap",
  "B_x_m"   = "Vliv l_m_gap"
)
vars_B <- names(coeff_titles_B)

coeff_titles_b0 <- c(
  "b0_x_x"  = "Okamžitý šok 'x' na 'x'",
  "b0_x_p"  = "Okamžitý šok 'p' na 'x'",
  "b0_x_e"  = "Okamžitý šok 'e' na 'x'",
  "b0_x_ulc"= "Okamžitý šok 'ulc' na 'x'"
)
vars_b0 <- names(coeff_titles_b0)

create_coeff_plot <- function(data, vars_to_plot, titles_map, n_col, 
                              plot_title) {
  
  rec_long <- data %>%
    select(Date, all_of(vars_to_plot)) %>%
    pivot_longer(
      cols = -Date,
      names_to = "Variable_Name",
      values_to = "Value"
    ) %>%
    mutate(Variable_Title = recode(Variable_Name, !!!titles_map))
  
  rec_long$Variable_Title <- factor(rec_long$Variable_Title,
                                    levels = titles_map)
  
  ggplot(rec_long, aes(x = Date, y = Value, color = Variable_Title)) +
    geom_rect(
      data = recessions_df,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey80", alpha = 0.3, inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(size = 1.1) +
    facet_wrap(~ Variable_Title, ncol = n_col, scales = "free_y") +
    scale_color_brewer(palette = "Set1") + 
    scale_x_date(
      date_breaks = "2 year", 
      date_labels = "%Y",
      expand = c(0.01, 0.01)
    ) +
    theme_minimal(base_size = 14) +
    labs(
      #title = plot_title,
      subtitle = "Omezený model",
      x = "Rok (konec odhadovaného okna)",
      y = "Hodnota koeficientu"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30"),
      legend.position = "none", 
      strip.text = element_text(face = "bold", size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_A <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_A,
  titles_map = coeff_titles_A,
  n_col = 2,
  plot_title = "Vývoj koeficientů - vliv zpoždění"
)
print(plot_A)

plot_B <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_B,
  titles_map = coeff_titles_B,
  n_col = 2,
  plot_title = "Vývoj koeficientů - vliv exogenních proměnných"
)
print(plot_B)

plot_b0 <- create_coeff_plot(
  data = rec_results_with_date,
  vars_to_plot = vars_b0,
  titles_map = coeff_titles_b0,
  n_col = 2,
  plot_title = "Vývoj koeficientů - okamžité dopady šoků"
)
print(plot_b0)

ggsave(
  filename = "Plots/coeffs_A_omezeny.png",
  plot = plot_A,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  filename = "Plots/coeffs_B_omezeny.png",
  plot = plot_B,
  width = 10,
  height = 5,
  dpi = 300
)
ggsave(
  filename = "Plots/coeffs_Cholesky_omezeny.png",
  plot = plot_b0,
  width = 10,
  height = 8,
  dpi = 300
)

# 7.4 IRF ######################################################################

H <- 12
post_final <- rstan::extract(fit_final_precovid, pars = c("A", "Sigma"))
A_draws <- post_final$A
Sigma_draws <- post_final$Sigma
n_draws <- dim(A_draws)[1]
irf_draws_array <- array(NA, dim = c(H + 1, m, m, n_draws))

for (d in 1:n_draws) {
  A_d <- A_draws[d, , ]
  Sigma_d <- Sigma_draws[d, , ]
  P_d <- t(chol(Sigma_d))
  Theta_h_minus_1 <- P_d
  irf_draws_array[1, , , d] <- Theta_h_minus_1
  for (h in 1:H) {
    Theta_h <- A_d %*% Theta_h_minus_1
    irf_draws_array[h + 1, , , d] <- Theta_h
    Theta_h_minus_1 <- Theta_h
  }
}
irf_median <- apply(irf_draws_array, c(1, 2, 3), median)
irf_q16 <- apply(irf_draws_array, c(1, 2, 3), quantile, probs = 0.16, 
                 na.rm = TRUE)
irf_q84 <- apply(irf_draws_array, c(1, 2, 3), quantile, probs = 0.84, 
                 na.rm = TRUE)
dimnames(irf_median) <- list(horizon = 0:H, response = Y_names, shock = Y_names)
dimnames(irf_q16) <- dimnames(irf_median)
dimnames(irf_q84) <- dimnames(irf_median)
df_median <- as.data.frame.table(irf_median, responseName = "median")
df_q16 <- as.data.frame.table(irf_q16, responseName = "q16")
df_q84 <- as.data.frame.table(irf_q84, responseName = "q84")
irf_plot_data <- df_median %>%
  left_join(df_q16, by = c("horizon", "response", "shock")) %>%
  left_join(df_q84, by = c("horizon", "response", "shock"))
irf_plot_data$horizon <- as.numeric(as.character(irf_plot_data$horizon))

post_final_AB <- rstan::extract(fit_final_precovid, pars = c("A", "B"))
A_draws_x <- post_final_AB$A
B_draws_x <- post_final_AB$B
n_draws_x <- dim(A_draws_x)[1]
irf_x_array <- array(NA, dim = c(H + 1, m, kx, n_draws_x))
for (d in 1:n_draws_x) {
  A_d <- A_draws_x[d, , ]
  B_d <- B_draws_x[d, , ]
  irf_x_array[1, , , d] <- B_d
  Theta_h_minus_1 <- B_d
  for (h in 1:H) {
    Theta_h <- A_d %*% Theta_h_minus_1
    irf_x_array[h + 1, , , d] <- Theta_h
    Theta_h_minus_1 <- Theta_h
  }
}
irf_x_median <- apply(irf_x_array, c(1, 2, 3), median)
irf_x_q16 <- apply(irf_x_array, c(1, 2, 3), quantile, probs = 0.16, 
                   na.rm = TRUE)
irf_x_q84 <- apply(irf_x_array, c(1, 2, 3), quantile, probs = 0.84, 
                   na.rm = TRUE)
dimnames(irf_x_median) <- list(horizon = 0:H, response = Y_names, 
                               shock_X = X_names)
dimnames(irf_x_q16) <- dimnames(irf_x_median)
dimnames(irf_x_q84) <- dimnames(irf_x_median)
df_x_median <- as.data.frame.table(irf_x_median, responseName = "median")
df_x_q16 <- as.data.frame.table(irf_x_q16, responseName = "q16")
df_x_q84 <- as.data.frame.table(irf_x_q84, responseName = "q84")
irf_x_plot_data <- df_x_median %>%
  left_join(df_x_q16, by = c("horizon", "response", "shock_X")) %>%
  left_join(df_x_q84, by = c("horizon", "response", "shock_X"))
irf_x_plot_data$horizon <- as.numeric(as.character(irf_x_plot_data$horizon))

variables_with_titles_gap <- c(
  "l_x_gap" = "Export",
  "l_y_gap" = "Zahraniční poptávka",
  "l_e_gap" = "Směnný kurz",
  "l_p_gap" = "Inflační diferenciál",
  "l_ulc_gap" = "Jednotkové náklady práce",
  "l_m_gap" = "Index importních cen"
)
df_irf_combined <- bind_rows(
  irf_plot_data %>%
    filter(response == "l_x_gap") %>%
    rename(shock_variable = shock),
  irf_x_plot_data %>%
    filter(response == "l_x_gap") %>%
    rename(shock_variable = shock_X)
)
df_plot_final <- df_irf_combined %>%
  mutate(
    Shock_Title = recode(shock_variable, !!!variables_with_titles_gap),
    Shock_Title = factor(Shock_Title, levels = variables_with_titles_gap)
  ) %>%
  filter(!is.na(Shock_Title))

combined_irf_plot_h12 <- df_plot_final %>%
  filter(horizon <= 12) %>%
  ggplot(aes(x = horizon)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(
    aes(ymin = q16, ymax = q84, fill = Shock_Title), 
    alpha = 0.5
  ) +
  geom_line(
    aes(y = median, color = Shock_Title), 
    linewidth = 1.2
  ) +
  facet_wrap(~ Shock_Title, ncol = 3, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    #title = "Dopad šoků na l_x_gap",
    subtitle = "Omezený model (Medián a 68% CI)",
    x = "Čtvrtletí",
    y = "Reakce (v %) odchylka od steady-state"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )
print(combined_irf_plot_h12)

ggsave(
  filename = "Plots/irf_omezeny.png",
  plot = combined_irf_plot_h12,
  width = 13,
  height = 8,
  dpi = 300
)

# 7.5 Graf stability modelu (rho v čase) #######################################

time_axis_full <- as.Date(as.yearqtr(time(model_data_ts)[2:(T + 1)]))
plot_data_rho <- rec_results_precovid %>%
  mutate(date = time_axis_full[t_end])
p_rho_omezeny <- ggplot(plot_data_rho, aes(x = date, y = rho)) +
  geom_line(color = "#01655E", linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    #title = "Vývoj stability modelu v čase",
    subtitle = "Omezený model", # <-- ZMĚNA
    x = "Poslední pozorování v okně",
    y = "Hodnota ρ"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p_rho_omezeny)

ggsave(
  filename = "Plots/rho_omezeny.png", # <-- ZMĚNA
  plot = p_rho_omezeny,
  width = 10,
  height = 6,
  dpi = 300
)

# 7.6 Historická šoková dekompozice ############################################

post_full <- rstan::extract(fit_final_precovid, pars = c("A", "B", "c", "Sigma"))
A_draws <- post_full$A
B_draws <- post_full$B
c_draws <- post_full$c
Sigma_draws <- post_full$Sigma
n_draws <- dim(A_draws)[1]
n_comp_shocks <- m
n_comp_exo <- kx
n_comp_base <- 1
n_components <- n_comp_base + n_comp_shocks + n_comp_exo
component_names <- c("Baseline_Det",
                     paste0("Shock_", Y_names), 
                     paste0("Exo_", X_names))

hsd_draws_array <- array(NA,
                         dim = c(n_draws, T, m, n_components),
                         dimnames = list(
                           Draw = 1:n_draws,
                           Time = 1:T,
                           Variable = Y_names,
                           Component = component_names
                         )
)
pb_hsd <- txtProgressBar(min = 0, max = n_draws, style = 3)
for (d in 1:n_draws) {
  A_d <- A_draws[d, , ]
  B_d <- B_draws[d, , ]
  c_d <- c_draws[d, ]
  Sigma_d <- Sigma_draws[d, , ]
  P_d <- t(chol(Sigma_d))
  P_inv_d <- solve(P_d)
  structural_shocks_d <- matrix(NA, T, m)
  irf_matrices_d <- array(NA, dim = c(T, m, m))
  det_baseline_path_d <- matrix(NA, T + 1, m)
  det_baseline_path_d[1, ] <- Y_full[1, ]
  exo_contrib_path_d <- array(NA, dim = c(T + 1, m, kx))
  exo_contrib_path_d[1, , ] <- 0
  Theta_h_minus_1 <- P_d
  irf_matrices_d[1, , ] <- P_d
  for (t in 1:T) {
    Y_t <- Y_full[t + 1, ]
    Y_t_minus_1 <- Y_full[t, ]
    X_t <- X[t, ]
    u_t <- Y_t - (c_d + A_d %*% Y_t_minus_1 + B_d %*% X_t)
    e_t <- P_inv_d %*% u_t
    structural_shocks_d[t, ] <- e_t
    det_baseline_path_d[t + 1, ] <- c_d + A_d %*% det_baseline_path_d[t, ]
    for (j in 1:kx) {
      exo_contrib_path_d[t + 1, , j] <- A_d %*% exo_contrib_path_d[t, , j] + 
        B_d[, j] * X[t, j]
    }
    if (t > 1) {
      Theta_h <- A_d %*% Theta_h_minus_1
      irf_matrices_d[t, , ] <- Theta_h
      Theta_h_minus_1 <- Theta_h
    }
  }
  hsd_draws_array[d, , , "Baseline_Det"] <- det_baseline_path_d[2:(T + 1), ]
  for (k in 1:kx) {
    comp_name_exo <- paste0("Exo_", X_names[k])
    hsd_draws_array[d, , , comp_name_exo] <- exo_contrib_path_d[2:(T + 1), , k]
  }
  for (t in 1:T) {
    for (j in 1:m) {
      contribution_j_t <- matrix(0, m, 1)
      for (h in 0:(t - 1)) {
        Theta_h <- irf_matrices_d[h + 1, , ]
        shock_j_t_minus_h <- structural_shocks_d[t - h, j]
        contribution_j_t <- contribution_j_t + Theta_h[, j] * shock_j_t_minus_h
      }
      comp_name_shock <- paste0("Shock_", Y_names[j])
      hsd_draws_array[d, t, , comp_name_shock] <- contribution_j_t
    }
  }
  setTxtProgressBar(pb_hsd, d)
}
close(pb_hsd)

hsd_median <- apply(hsd_draws_array, c(2, 3, 4), median, na.rm = TRUE)

hsd_plot_data_wide <- as.data.frame.table(hsd_median, responseName = "Value")
colnames(hsd_plot_data_wide) <- c("Time_Idx", "Variable", "Component", "Value")
hsd_plot_data_wide$Time_Idx <- as.numeric(hsd_plot_data_wide$Time_Idx)

Y_time_axis_date <- as.Date(as.yearqtr(time(model_data_ts)[2:(T + 1)]))
date_map <- data.frame(Time_Idx = 1:T, Date = Y_time_axis_date)

hsd_plot_data <- hsd_plot_data_wide %>%
  left_join(date_map, by = "Time_Idx")

observed_data_long <- as.data.frame(Y_full[2:(T + 1), ])
colnames(observed_data_long) <- Y_names
observed_data_long$Time_Idx <- 1:T
observed_data_long <- observed_data_long %>%
  pivot_longer(cols = -Time_Idx, names_to = "Variable", values_to = "Observed_Y")

hsd_plot_data <- hsd_plot_data %>%
  left_join(observed_data_long, by = c("Time_Idx", "Variable"))

var_to_plot <- "l_x_gap"
plot_data_hsd_final <- hsd_plot_data %>%
  filter(Variable == var_to_plot)

plot_data_hsd_final$Component <- factor(
  plot_data_hsd_final$Component, 
  levels = component_names
)

shock_labels <- paste("Šok:", variables_with_titles_gap[Y_names])
names(shock_labels) <- paste0("Shock_", Y_names)
exo_labels <- paste("Vliv:", variables_with_titles_gap[X_names])
names(exo_labels) <- paste0("Exo_", X_names)
comp_labels <- c(
  "Baseline_Det" = "Determ. baseline",
  shock_labels,
  exo_labels
)
colors_hsd_final <- c(
  "Baseline_Det"      = "grey80",
  "Shock_l_p_gap"     = "#E41A1C",
  "Shock_l_e_gap"     = "#377EB8",
  "Shock_l_ulc_gap"   = "#4DAF4A",
  "Shock_l_x_gap"     = "#984EA3",
  "Exo_l_y_gap"       = "#FF7F00",
  "Exo_l_m_gap"       = "#A65628" 
)

p_hsd_new <- ggplot(plot_data_hsd_final, aes(x = Date)) +
  geom_col(aes(y = Value, fill = Component), position = "stack", width = 80) +
  geom_line(aes(y = Observed_Y), color = "black", linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  scale_fill_manual(
    values = colors_hsd_final, 
    labels = comp_labels,
    name = "Příspěvek komponenty:",
    breaks = component_names 
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y", 
    expand = c(0, 0)
  ) +
  labs(
    #title = paste("Historická šoková dekompozice pro", 
    #              variables_with_titles_gap[var_to_plot]),
    subtitle = "Omezený model (Černá čára: Pozorovaná data)",
    x = "Datum",
    y = "Hodnota (odchylka od trendu)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
print(p_hsd_new)

ggsave(
  filename = "Plots/sokova_dekompozice_omezeny.png",
  plot = p_hsd_new,
  width = 12,
  height = 8,
  dpi = 300
)

# 7.7 MCMC diagnostika - traceplots ############################################

post_draws_final <- as.array(fit_final_precovid)
pars_to_check <- c("lp__", 
                   "A[1,1]", "A[4,4]", 
                   "B[4,1]", "B[4,2]", 
                   "sigma[1]", "sigma[4]") 

nazev_p_gap <- variables_with_titles_gap["l_p_gap"]
nazev_x_gap <- variables_with_titles_gap["l_x_gap"]
nazev_y_gap <- variables_with_titles_gap["l_y_gap"]
nazev_m_gap <- variables_with_titles_gap["l_m_gap"]

trace_labels <- c(
  "lp__"     = "Log-posterior (model)",
  "A[1,1]"   = paste("AR(1)", nazev_p_gap),
  "A[4,4]"   = paste("AR(1)", nazev_x_gap),
  "B[4,1]"   = paste("Vliv:", nazev_y_gap),
  "B[4,2]"   = paste("Vliv:", nazev_m_gap),
  "sigma[1]" = paste("Sigma:", nazev_p_gap),
  "sigma[4]" = paste("Sigma:", nazev_x_gap)
)
chain_colors <- brewer.pal(3, "Set1")[1:2]
p_trace_custom <- mcmc_trace(
  post_draws_final, 
  pars = pars_to_check,
  facet_args = list(
    ncol = 3,
    labeller = labeller(.default = trace_labels, .multi_line = FALSE)
  )
) +
  scale_color_manual(values = chain_colors, name = "MCMC řetězec:") +
  labs(
    #title = "MCMC Trace Plots pro vybrané parametry",
    subtitle = "Omezený model") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9), 
    panel.grid.minor = element_blank()
  )
print(p_trace_custom)

ggsave(
  filename = "Plots/mcmc_trace_omezeny.png",
  plot = p_trace_custom,
  width = 12,
  height = 8,
  dpi = 300
)

################################################################################
# 8 Konec skriptu ##############################################################

end_time <- Sys.time()
duration <- difftime(end_time, start_time)

duration_mins <- as.numeric(duration, units = "mins")

cat("\n################################################################\n")
cat("Celý skript úspěšně dokončen.\n")
cat("Čas dokončení:", format(end_time, "%H:%M:%S"), "\n")
cat("Celková doba trvání:", round(duration_mins, 2), "minut.\n")
cat("################################################################\n")

################################################################################
