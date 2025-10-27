################################################################################
################# DETERMINANTY A ELASTICITA ČESKÉHO EXPOERTU ###################
############################### Petr Chaloupek #################################
################################################################################

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

# Ruční výpočet pro P_for
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
  geom_line(size = 1.2) + # Silnější čára
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
    title = "Časové řady",
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

# 2. Výpočet gapu proměnných ###################################################



# 2.1. Výpočet gapu proměnných #################################################

variables <- c("l_x", "l_y", "l_e", "l_p_cz", "l_p_for", "l_ulc", "l_m")
lamb <- 1600 # Standardní lambda pro kvartální data

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
  print(paste("Vypočítán jednostranný HP filtr (hp1) pro:", var))
}

# 2.2 Úprava proměnných #

df$l_p_gap <- df$l_p_cz_gap - df$l_p_for_gap

# 2.2 Vizualizace gapu proměnných #############################################

variables <- c("l_x", "l_y", "l_e", "l_p", "l_ulc", "l_m")
variables_gap <- paste0(variables, "_gap")

variables_with_titles_gap <- c(
  "l_x_gap" = "Export (odchylka od trendu, %)",
  "l_y_gap" = "Zahr. poptávka (odchylka od trendu, %)",
  "l_e_gap" = "Směnný kurz (odchylka od trendu, %)",
  "l_p_gap" = "Inflační diferenciál (odchylka od trendu, %)",
  "l_ulc_gap" = "Růst ULC (odchylka od trendu, %)",
  "l_m_gap" = "Import (odchylka od trendu, %)"
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
  labs(
    title = "Odchylky proměnných od trendu (HP filtr)") +
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

# 2.3 Test stacionarity (ADF test) #############################################

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

# 2.6. Vizualizace originální řadu, gapu a trendu# #############################

recessions_df <- data.frame(
  start = as.Date(c("2008-01-01", "2012-01-01", "2020-01-01", "2023-01-01")),
  end = as.Date(c("2009-12-31", "2013-12-31", "2020-12-31", "2023-12-31"))
)

title_map_r <- c(
  "l_x" = "Export",
  "l_y" = "Zahraniční poptávka",
  "l_e" = "Směnný kurz",
  "l_p" = "Inflační diferenciál",
  "l_ulc" = "Změna ULC",
  "l_m" = "Importní ceny"
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

# 2.7 Popisné statistiky gapů ##################################################

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

# 2.8. Standardizace proměnných ################################################

variables_gap <- c("l_x_gap", "l_y_gap", "l_e_gap", "l_p_gap", "l_ulc_gap", 
                   "l_m_gap")

for (var in variables_gap) {
  mean_name <- paste0("mu_", var)
  sd_name <- paste0("sd_", var)
  val_mean <- get(mean_name)
  val_sd <- get(sd_name)
  z_score <- (df[[var]] - val_mean) / val_sd
  new_var_name <- paste0(var, "_z")
  df[[new_var_name]] <- z_score
}

head(df[, paste0(variables_gap, "_z")])

# 2.9 Popisné statistiky standardizovaných gapů ################################

variables_z <- c("l_x_gap_z", "l_y_gap_z", "l_e_gap_z", "l_p_gap_z", 
                   "l_ulc_gap_z", "l_m_gap_z")

stats_list_z <- list()

for (var in variables_z) {
  series_clean <- df[[var]][!is.na(df[[var]])]
  val_mean <- mean(series_clean)
  val_var <- var(series_clean)
  val_sd <- sd(series_clean)
  stats_list_z[[var]] <- data.frame(
    Variable = var,
    Mean = val_mean,
    Variance = val_var,
    Std_Deviation = val_sd
  )
}

stats_table_z <- do.call(rbind, stats_list_z)
rownames(stats_table_z) <- NULL

print("Deskriptivní statistika (Standardizované proměnné Z-scores):")
print(stats_table_z)

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
  "l_y_gap" = "Zahr. poptávka",
  "l_e_gap" = "Směnný kurz",
  "l_p_gap" = "Inflační diferenciál",
  "l_ulc_gap" = "Změna ULC",
  "l_m_gap" = "Importní ceny"
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
    mid = "#ff9f1c",
    high = "#854c9e",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Korelace" 
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
    title = "Korelace s exportem (t až t−6)",
    x = "Zpoždění",
    y = "Proměnná"
  )

print(heatmap_plot)

ggsave(
  filename = "Plots/korelace_heatmap.png", 
  plot = heatmap_plot, 
  width = 9.5, 
  height = 5.2, 
  dpi = 300
)


# 4 . Rekurzivní BVAR model ####################################################

vars <- c("l_y_gap", "l_p_gap", "l_e_gap", "l_ulc_gap", "l_m_gap", "l_x_gap")
model_data_ts <- ts(
  data = as.matrix(df[, vars]),
  start = c(2000, 1),
  frequency = 4
)
colnames(model_data_ts) <- vars

head(model_data_ts)

# 4.1 Rekurzivní odhad BVAR(1) s Minnesota priory + Cholesky ###################

vars_sel <- c("l_y_gap", "l_ulc_gap", "l_p_gap", "l_m_gap", "l_e_gap", "l_x_gap")
Y <- model_data_ts[, vars_sel]
m <- ncol(Y); Tn <- nrow(Y)

priors <- bv_priors(
  mn = bv_mn(
    lambda = bv_lambda(mode = 0.30, sd = 0.30, min = 1e-4, max = 5),
    alpha  = bv_alpha(mode = 2.00, sd = 0.25, min = 1.0, max = 3.0),
    psi    = bv_psi(mode = rep(1, m)),
    var = 1e7, b = 0
  )
)

t0 <- max(40, 1 + 4 * 1)

ix_y   <- which(colnames(Y) == "l_y_gap")
ix_ulc <- which(colnames(Y) == "l_ulc_gap")
ix_p   <- which(colnames(Y) == "l_p_gap")
ix_m   <- which(colnames(Y) == "l_m_gap")
ix_e   <- which(colnames(Y) == "l_e_gap")
ix_x   <- which(colnames(Y) == "l_x_gap")

rec_store <- vector("list", Tn - t0 + 1)
k <- 0

for (t_end in t0:Tn) {
  k <- k + 1
  Y_sub <- Y[1:t_end, ]
  set.seed(123)
  mod_t <- bvar(Y_sub, lags = 1, n_draw = 4000, n_burn = 2000, priors = priors)
  # (1) Koeficienty lagů
  B <- coef(mod_t, type = "quantile", probs = 0.5)
  lag_rows <- grep("lag1$", rownames(B))
  B1 <- as.matrix(B[lag_rows, ])
  # (2) Stabilita
  rho <- max(Mod(eigen(B1)$values))
  # (3) Zpožděné efekty (t-1)
  b_x_y   <- B1[ix_x, ix_y]
  b_x_ulc <- B1[ix_x, ix_ulc]
  b_x_p   <- B1[ix_x, ix_p]
  b_x_m   <- B1[ix_x, ix_m]
  b_x_e   <- B1[ix_x, ix_e]
  # (4) Současné efekty (t) – Cholesky
  Sigma_u <- vcov(mod_t, type = "quantile", probs = 0.5)
  A0_inv <- tryCatch(t(chol(Sigma_u)), error = function(e) NA)
  
  if (is.matrix(A0_inv)) {
    b0_x_y   <- A0_inv[ix_x, ix_y]
    b0_x_ulc <- A0_inv[ix_x, ix_ulc]
    b0_x_p   <- A0_inv[ix_x, ix_p]
    b0_x_m   <- A0_inv[ix_x, ix_m]
    b0_x_e   <- A0_inv[ix_x, ix_e]
  } else {
    b0_x_y <- b0_x_ulc <- b0_x_p <- b0_x_m <- b0_x_e <- NA_real_
  }
  # (5) Uložení výsledků
  rec_store[[k]] <- data.frame(
    t_end = t_end,
    rho = rho,
    b_x_y_L1   = b_x_y,
    b_x_ulc_L1 = b_x_ulc,
    b_x_p_L1   = b_x_p,
    b_x_m_L1   = b_x_m,
    b_x_e_L1   = b_x_e,
    b0_x_y   = b0_x_y,
    b0_x_ulc = b0_x_ulc,
    b0_x_p   = b0_x_p,
    b0_x_m   = b0_x_m,
    b0_x_e   = b0_x_e
  )
}

rec_results <- bind_rows(rec_store)

print(head(rec_results, 5))
print(tail(rec_results, 5))

# 4.2 Vizualizace BVAR modelu ##################################################

time_axis <- time(model_data_ts)

date_mapping <- data.frame(
  t_end = 1:length(time_axis),
  Date = zoo::as.Date.ts(time_axis)
)
rec_results_dated <- rec_results %>%
  left_join(date_mapping, by = "t_end")

plot_rho <- ggplot(rec_results_dated, aes(x = Date, y = rho)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Stabilita modelu (Max Eigenvalue)", x = "Datum", y = "Rho") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

coeffs_l1 <- rec_results_dated %>%
  select(Date, starts_with("b_x_") & ends_with("_L1")) %>%
  pivot_longer(cols = -Date, names_to = "Coefficient", values_to = "Value") %>%
  mutate(Coefficient = gsub("b_x_|_L1", "", Coefficient))
plot_l1 <- ggplot(coeffs_l1, aes(x = Date, y = Value, color = Coefficient)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ Coefficient, ncol = 3, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(title = "Vliv proměnných v t-1 na Export (l_x_gap) v t", x = "Datum", 
       y = "Hodnota koeficientu") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

coeffs_t0 <- rec_results_dated %>%
  select(Date, starts_with("b0_x_")) %>%
  pivot_longer(cols = -Date, names_to = "Coefficient", values_to = "Value") %>%
  mutate(Coefficient = gsub("b0_x_", "", Coefficient))
plot_t0 <- ggplot(coeffs_t0, aes(x = Date, y = Value, color = Coefficient)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ Coefficient, ncol = 3, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(title = "Současný vliv proměnných v t na Export (l_x_gap) v t (Cholesky)", 
       x = "Datum", y = "Hodnota koeficientu") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

print(plot_rho)
print(plot_l1)
print(plot_t0)

# 4.3 Diagnostika BVAR modelu ##################################################

print("Vykresluji Trace ploty pro poslední model...")
plot(mod_t, type = "trace")

print("Vykresluji Density ploty pro poslední model...")
plot(mod_t, type = "density")

# (Assuming 'mod_t' holds the last model)

# Get the residuals (median estimate)
resids <- residuals(mod_t, type = "quantile", probs = 0.5)

# Plot ACF for all residual series
print("Vykresluji ACF reziduí posledního modelu...")
acf(resids, main = "ACF reziduí (poslední model)")
# Ideally, correlations should quickly fall inside the blue lines for lags > 0.

print("Souhrn posledního modelu:")
summary(mod_t)

# 4.4 Vykreslení IRF ###########################################################

print("Počítám impulzní odezvy pro poslední model...")
irfs <- irf(
  mod_t,
  horizon = 16,
  identification = TRUE,
  # --- CORRECTION HERE ---
  conf_bands = c(0.16, 0.84) # Use conf_bands for 68% interval
  # --- END CORRECTION ---
)

# 2. Plot all IRFs
print("Vykresluji impulzní odezvy...")
plot(irfs)

# 3. Plot specific IRFs (Responses of Export)
print("Vykresluji reakce Exportu (l_x_gap) na šoky...")
plot(irfs,
     vars_response = "l_x_gap",
     vars_impulse = c("l_y_gap", "l_ulc_gap", "l_p_gap", "l_m_gap", "l_e_gap", "l_x_gap")
)

# Předpokládám, že balíčky jsou již načteny
# library(BVAR)
# library(dplyr)

# 4.5 Rekurzivní odhad BVAR-X(1) s Minnesota priory + Cholesky ################

vars_sel <- c("l_ulc_gap", "l_p_gap", "l_e_gap", "l_x_gap")
Y <- model_data_ts[, vars_sel]
m <- ncol(Y); Tn <- nrow(Y)

# <<< ZMĚNA 1: Definice exogenních proměnných (X)
# =========================================================================
# Zde si definujte názvy svých exogenních proměnných z 'model_data_ts'
# (Nahraďte názvy "vaše_exo_promenna_1", "vaše_exo_promenna_2" atd.)
vars_exo <- c("l_y_gap", "l_m_gap") 

# Vytvoření matice X
X <- model_data_ts[, vars_exo]

# Důležitá kontrola: Y a X musí mít stejný počet řádků
if (nrow(X) != Tn) {
  stop("Chyba: Počet pozorování (řádků) v Y a X se neshoduje.")
}
# =========================================================================


priors <- bv_priors(
  mn = bv_mn(
    lambda = bv_lambda(mode = 0.30, sd = 0.30, min = 1e-4, max = 5),
    alpha  = bv_alpha(mode = 2.00, sd = 0.25, min = 1.0, max = 3.0),
    psi    = bv_psi(mode = rep(1, m)),
    var = 1e7, b = 0
  )
)

# <<< POZNÁMKA K PRIORŮM:
# Výše uvedené 'priors' se vztahují pouze na endogenní část (Y).
# Balíček BVAR automaticky použije difuzní (non-informative) prior pro
# koeficienty u exogenních proměnných (X).
# Pokud byste chtěli nastavit prior i pro X, udělali byste to takto:
# priors <- bv_priors(mn = bv_mn(...), exo = bv_exo(var = 1.0, b = 0))
# Pro většinu aplikací je však výchozí difuzní prior v pořádku.


t0 <- max(40, 1 + 4 * 1) # Váš kód používá lags=1, takže 1+4*1 je v pořádku

ix_y   <- which(colnames(Y) == "l_y_gap")
ix_ulc <- which(colnames(Y) == "l_ulc_gap")
ix_p   <- which(colnames(Y) == "l_p_gap")
ix_m   <- which(colnames(Y) == "l_m_gap")
ix_e   <- which(colnames(Y) == "l_e_gap")
ix_x   <- which(colnames(Y) == "l_x_gap")

# ... (kód před cyklem je v pořádku) ...

rec_store <- vector("list", Tn - t0 + 1)
k <- 0

for (t_end in t0:Tn) {
  k <- k + 1
  Y_sub <- Y[1:t_end, ]
  X_sub <- X[1:t_end, , drop = FALSE]
  
  set.seed(123)
  
  # Odhad BVAR-X
  mod_t <- bvar(
    data = Y_sub, 
    lags = 1, 
    n_draw = 4000, 
    n_burn = 2000, 
    priors = priors,
    exogen = X_sub
  )
  
  # (1) Koeficienty lagů
  B <- coef(mod_t, type = "quantile", probs = 0.5)
  lag_rows <- grep("lag1$", rownames(B))
  
  
  # <<< TOTO JE KLÍČOVÁ OPRAVA (BLOK IF/ELSE) >>>
  # =========================================================================
  if (length(lag_rows) > 0) {
    B1 <- as.matrix(B[lag_rows, ])
    
    # (2) Stabilita
    rho <- max(Mod(eigen(B1)$values))
    
    # (3) Zpožděné efekty (t-1)
    b_x_y    <- B1[ix_x, ix_y]
    b_x_ulc  <- B1[ix_x, ix_ulc]
    b_x_p    <- B1[ix_x, ix_p]
    b_x_m    <- B1[ix_x, ix_m]
    b_x_e    <- B1[ix_x, ix_e]
    
  } else {
    # Pokud grep selhal (model se špatně odhadl), vyplníme NA
    # NA_real_ má délku 1, takže chyba v data.frame zmizí
    rho      <- NA_real_
    b_x_y    <- NA_real_
    b_x_ulc  <- NA_real_
    b_x_p    <- NA_real_
    b_x_m    <- NA_real_
    b_x_e    <- NA_real_
  }
  # =========================================================================
  
  
  # (4) Současné efekty (t) – Cholesky
  # Tato část je již správně ošetřena pomocí tryCatch a je v pořádku
  Sigma_u <- vcov(mod_t, type = "quantile", probs = 0.5)
  A0_inv <- tryCatch(t(chol(Sigma_u)), error = function(e) NA)
  
  if (is.matrix(A0_inv)) {
    b0_x_y   <- A0_inv[ix_x, ix_y]
    b0_x_ulc <- A0_inv[ix_x, ix_ulc]
    b0_x_p   <- A0_inv[ix_x, ix_p]
    b0_x_m   <- A0_inv[ix_x, ix_m]
    b0_x_e   <- A0_inv[ix_x, ix_e]
  } else {
    b0_x_y <- b0_x_ulc <- b0_x_p <- b0_x_m <- b0_x_e <- NA_real_
  }
  
  # (5) Uložení výsledků
  # Nyní již budou mít všechny proměnné zaručeně délku 1
  rec_store[[k]] <- data.frame(
    t_end = t_end,
    rho = rho,
    b_x_y_L1   = b_x_y,
    b_x_ulc_L1 = b_x_ulc,
    b_x_p_L1   = b_x_p,
    b_x_m_L1   = b_x_m,
    b_x_e_L1   = b_x_e,
    b0_x_y   = b0_x_y,
    b0_x_ulc = b0_x_ulc,
    b0_x_p   = b0_x_p,
    b0_x_m   = b0_x_m,
    b0_x_e   = b0_x_e
  )
} # Konec cyklu FOR

# Spojení výsledků
rec_results <- dplyr::bind_rows(rec_store)

print(head(rec_results, 5))
print(tail(rec_results, 5))

# 4.6 Model dle GPT ###########################################################
# ---- ZÁVISLOSTI ----
# install.packages("rstan", repos = "https://cloud.r-project.org")
library(rstan)

# ---- VYBER PROMĚNNÝCH ----
Y_names <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")  # endogenní
X_names <- c("l_y_gap","l_m_gap")                        # exogenní

stopifnot(exists("df"))
D <- na.omit(df[, c(Y_names, X_names, "Date")])

# ts indexy (lagování ručně)
Y <- as.matrix(D[, Y_names, drop = FALSE])
X <- as.matrix(D[, X_names, drop = FALSE])

# lag-1 pro Y:
Y_lag1 <- rbind(NA, Y[-nrow(Y), , drop = FALSE])

# zarovnej na společný vzorek bez NA
keep <- complete.cases(Y, X, Y_lag1)
Y_t   <- Y[keep, , drop = FALSE]
X_t   <- X[keep, , drop = FALSE]
Y_l1  <- Y_lag1[keep, , drop = FALSE]

Tn <- nrow(Y_t); m <- ncol(Y_t); kx <- ncol(X_t)

# ---- HYPERPARAMETRY (Minnesota-like) ----
# sd pro koeficienty u Y_{t-1}:
sd_own   <- 0.20   # „vlastní lag“ (diagonála A) – volnější
sd_cross <- 0.05   # „cizí lagy“ (mimo diagonálu) – silnější shrinkace
# sd pro exogenní X (B):
sd_exo   <- 0.10
# sd pro konstanty:
sd_const <- 1.0

# masky pro A (m x m)
own_mask   <- diag(m) == 1
cross_mask <- !own_mask

A_sd <- matrix(sd_cross, m, m)
A_sd[own_mask] <- sd_own

# B_sd (m x kx)
B_sd <- matrix(sd_exo, m, kx)

# ---- DATA PRO STAN ----
stan_data <- list(
  T = Tn, m = m, kx = kx,
  Y = Y_t, Ylag = Y_l1, X = X_t,
  A_sd = A_sd,     # prior sd pro A
  B_sd = B_sd,     # prior sd pro B
  c_sd = rep(sd_const, m), # prior sd pro konstanty
  lkj_eta = 2      # slabě informativní prior na korelace
)

# ---- STAN MODEL (dočasně v paměti) ----
stan_code <- "
data {
  int<lower=1> T;
  int<lower=1> m;
  int<lower=0> kx;
  matrix[T, m] Y;
  matrix[T, m] Ylag;
  matrix[T, kx] X;
  matrix[m, m] A_sd;
  matrix[m, kx] B_sd;
  vector[m] c_sd;
  real<lower=0> lkj_eta;
}
parameters {
  matrix[m, m] A;          // koef. u Y_{t-1}
  matrix[m, kx] B;         // koef. u X_t
  vector[m] c;             // konstanty
  vector<lower=0>[m] sigma;               // škály reziduí
  cholesky_factor_corr[m] L_Omega;        // korelační faktor
}
transformed parameters {
  matrix[m, m] L_Sigma;    // Cholesky kovarianční matice
  L_Sigma = diag_pre_multiply(sigma, L_Omega);
}
model {
  // Priory (Minnesota-like shrinkage)
  for (i in 1:m) {
    for (j in 1:m) {
      A[i, j] ~ normal(0, A_sd[i, j]);
    }
  }
  for (i in 1:m) {
    for (j in 1:kx) {
      B[i, j] ~ normal(0, B_sd[i, j]);
    }
  }
  c ~ normal(0, c_sd);

  sigma ~ cauchy(0, 0.5);                // slabý prior na škály
  L_Omega ~ lkj_corr_cholesky(lkj_eta);  // prior na korelace

  // Likelihood
  for (t in 1:T) {
    vector[m] mu = c + A * to_vector(Ylag[t]') + B * to_vector(X[t]');
    Y[t]' ~ multi_normal_cholesky(mu, L_Sigma);
  }
}
generated quantities {
  matrix[m, m] Sigma;
  Sigma = L_Sigma * L_Sigma';
}
"

# ---- FIT ----
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = max(1, parallel::detectCores()-1))

fit <- stan(model_code = stan_code,
            data  = stan_data,
            chains = 4, iter = 3000, warmup = 1500, seed = 123)

print(fit, pars = c("A","B","c"), probs = c(0.05, 0.5, 0.95))
print(fit, pars = c("Sigma"), probs = c(0.05, 0.5, 0.95))

# ---- Získání mediánových odhadů ----
post <- rstan::extract(fit)
A_med <- apply(post$A, c(2,3), median)  # (iter, m, m) -> (m, m)
B_med <- if (kx>0) apply(post$B, c(2,3), median) else matrix(0, m, 0)
Sigma_med <- apply(post$Sigma, c(2,3), median)

# Stabilita VAR(1) (jen endogenní lag blok A_med):
rho <- max(Mod(eigen(A_med)$values))
cat(sprintf("Spektrální poloměr ρ = %.3f %s\n", rho, ifelse(rho<1,"(stabilní)","(nestabilní)")))

# Současné (t=0) dopady v endogenním bloku (Cholesky):
A0_inv <- t(chol(Sigma_med))  # impact multipliers
print(A0_inv)

# Indexy
yn <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")
xn <- c("l_y_gap","l_m_gap")
ix_x <- match("l_x_gap", yn)

# Funkce pro posterior kvantily pro l_x_gap
qtab <- function(arr, idx_row, idx_cols, probs=c(0.05,0.5,0.95)) {
  # arr: draws x row x col
  out <- sapply(idx_cols, function(j) quantile(arr[,idx_row,j], probs = probs))
  t(out)
}

# Kvantily pro A (lagované endogenní → export)
A_q <- qtab(post$A, idx_row = ix_x, idx_cols = 1:4)  # sloupce 1:p,e,ulc,x(lag)
rownames(A_q) <- paste0(yn, ".lag1")

# Kvantily pro B (exogenní → export)
B_q <- qtab(post$B, idx_row = ix_x, idx_cols = 1:2)  # sloupce 1:y_gap, 2:m_gap
rownames(B_q) <- xn

A_q; B_q

irf_chol <- function(A, Sigma, h = 12, shock_var = "l_e_gap", yn = yn) {
  i <- match(shock_var, yn)
  L <- t(chol(Sigma))           # A0^{-1}
  P <- L                        # (t=0) dopady
  Phi <- array(0, dim=c(length(yn), length(yn), h+1))
  Phi[,,1] <- P
  for (k in 2:(h+1)) Phi[,,k] <- A %*% Phi[,,k-1]
  dimnames(Phi) <- list(yn, yn, 0:h)
  Phi
}
# Příklad: IRF exportu na šok v e
Phi <- irf_chol(A_med, Sigma_med, h=12, shock_var="l_e_gap", yn=yn)
irf_x_on_e <- Phi["l_x_gap","l_e_gap",]  # vektor t=0..12

irf_exo <- function(A, B, h=12, exo="l_y_gap", yn=yn, xn=xn, resp="l_x_gap") {
  k <- match(exo, xn); i <- match(resp, yn)
  v <- numeric(h+1)
  # t = 0
  v[1] <- B[i,k]
  # t >= 1
  M <- diag(nrow(A))
  for (s in 1:h) {
    M <- M %*% A
    v[s+1] <- (M %*% B)[i,k]
  }
  v
}
irf_x_on_yexo <- irf_exo(A_med, B_med, h=12, exo="l_y_gap", resp="l_x_gap", yn=yn, xn=xn)

# P(beta>0) pro exogenní dopady na export:
ix_x <- match("l_x_gap", yn)
p_pos_B <- colMeans(post$B[, ix_x, ] > 0)     # jména sloupců odpovídají xn
p_pos_A <- colMeans(post$A[, ix_x, ] > 0)     # 1..4 ~ l_p, l_e, l_ulc, l_x (lag1)
names(p_pos_A) <- paste0(yn, ".lag1")
p_pos_B; p_pos_A

tab_export <- rbind(
  cbind(var = paste0(yn, ".lag1"),
        q05 = round(A_q[,1], 3), q50 = round(A_q[,2], 3), q95 = round(A_q[,3], 3)),
  cbind(var = xn,
        q05 = round(B_q[,1], 3), q50 = round(B_q[,2], 3), q95 = round(B_q[,3], 3))
)
tab_export

# Endogenní: export na 1 s.d. šok v l_e_gap (t=0 efekt už máš v irf_x_on_e)
plot(0:12, irf_x_on_e, type="l", main="IRF: l_x_gap ← šok v l_e_gap", xlab="horizont", ylab="dopad")
abline(h=0, lty=2, col="gray")

# Exogenní: export na jednorázový šok v l_y_gap
plot(0:12, irf_x_on_yexo, type="l", main="IRF: l_x_gap ← (exo) šok v l_y_gap", xlab="horizont", ylab="dopad")
abline(h=0, lty=2, col="gray")

fevd_chol <- function(A, Sigma, H=12){
  m <- nrow(A); P <- t(chol(Sigma))
  Phi <- array(0, c(m,m,H+1)); Phi[,,1] <- P
  for(h in 2:(H+1)) Phi[,,h] <- A %*% Phi[,,h-1]
  # h-kroková var l_x_gap ~ suma_j sum_{s<=h} Phi[x,j,s]^2
  x <- match("l_x_gap", yn)
  num <- sapply(1:m, function(j) sum(Phi[x,j,]^2))
  den <- sum(num)
  num/den
}
round(fevd_chol(A_med, Sigma_med, H=12), 3)

# =============================================================================
#  POROVNÁNÍ BVAR(1) vs BVARX(1) NA SPOLEČNÉM VZORKU
# =============================================================================

# --- ZÁVISLOSTI ---
suppressPackageStartupMessages({
  library(BVAR)    # CRAN
  library(rstan)   # CRAN
  library(dplyr)   # CRAN
})

# --- VSTUPNÍ DATA ---
stopifnot(exists("df"))

# Názvy proměnných dle tvého skriptu
vars_sel <- c("l_y_gap", "l_ulc_gap", "l_p_gap", "l_m_gap", "l_e_gap", "l_x_gap") # pro BVAR
Y_names  <- c("l_p_gap","l_e_gap","l_ulc_gap","l_x_gap")                           # endog. pro BVARX
X_names  <- c("l_y_gap","l_m_gap")                                                 # exog. pro BVARX

# --- SPOLEČNÝ VZOREK PRO OBA MODELY (včetně lags) ---
D0 <- df[, c(vars_sel, "Date")] |> tidyr::drop_na()
Y_raw <- as.matrix(D0[, Y_names, drop = FALSE])
X_raw <- as.matrix(D0[, X_names, drop = FALSE])

# Lag-1 pro endogenní
Y_lag1 <- rbind(NA, Y_raw[-nrow(Y_raw), , drop = FALSE])

# Společný vzorek bez NA (zahrnuje Y, X i Y_{t-1})
keep <- complete.cases(Y_raw, X_raw, Y_lag1)
D  <- D0[keep, , drop = FALSE]
Yt <- as.matrix(D[, Y_names, drop = FALSE])
Xt <- as.matrix(D[, X_names, drop = FALSE])
Yl <- rbind(NA, Yt[-nrow(Yt), , drop = FALSE])  # lag-1 zarovnaný k Yt

# Pro BVAR pracujeme na stejném (intersekčním) vzorku
Y_bvar <- as.matrix(D[, vars_sel, drop = FALSE])

# Indexy
ix_x <- match("l_x_gap", colnames(Yt))  # ve 4×4 endog. bloku BVARX
ix_x_bvar <- match("l_x_gap", colnames(Y_bvar))  # v 6×6 BVAR

# =============================================================================
#  A) BVAR(1) (balík BVAR) na společném vzorku
# =============================================================================
set.seed(123)
m_bvar <- ncol(Y_bvar)

priors_bvar <- bv_priors(
  mn = bv_mn(
    lambda = bv_lambda(mode = 0.30, sd = 0.30, min = 1e-4, max = 5),
    alpha  = bv_alpha(mode = 2.00, sd = 0.25, min = 1.0, max = 3.0),
    psi    = bv_psi(mode = rep(1, m_bvar)),
    var = 1e7, b = 0
  )
)

mod_bvar <- bvar(
  Y_bvar,
  lags   = 1,
  n_draw = 4000,
  n_burn = 2000,
  priors = priors_bvar
)

# Koeficienty (median) a lag-1 blok
Bcoef <- coef(mod_bvar, type = "quantile", probs = 0.5)
lag_rows <- grep("lag1$", rownames(Bcoef))
B1_6x6 <- as.matrix(Bcoef[lag_rows, , drop = FALSE])  # (lags) x (equations), zde 6×6

# Stabilita (ρ) – spektrální poloměr B1
rho_bvar <- max(Mod(eigen(B1_6x6, only.values = TRUE)$values))

# In-sample 1Q-ahead predikce exportu (l_x_gap) z BVAR
Yb <- Y_bvar  # zkrácený název
yhat_bvar <- rep(NA_real_, nrow(Yb))
for (t in 2:nrow(Yb)) {
  mu <- B1_6x6 %*% as.numeric(Yb[t-1, ])
  yhat_bvar[t] <- mu[ix_x_bvar]
}
# RMSE (zahodíme první predikci NA)
rmse_bvar <- sqrt(mean((Yb[-1, ix_x_bvar] - yhat_bvar[-1])^2, na.rm = TRUE))

# =============================================================================
#  B) BVARX(1) (Stan) na stejném vzorku
# =============================================================================

# Hyperparametry (Minnesota-like)
sd_own   <- 0.20
sd_cross <- 0.05
sd_exo   <- 0.10
sd_const <- 1.0

m <- ncol(Yt); kx <- ncol(Xt)
A_sd <- matrix(sd_cross, m, m); diag(A_sd) <- sd_own
B_sd <- matrix(sd_exo,   m, kx)
# --- Zarovnání dat bez NA po vytvoření lagů ---
Yl <- rbind(NA, Yt[-nrow(Yt), , drop = FALSE])  # lag-1
keep2 <- complete.cases(Yt, Xt, Yl)
Yt <- Yt[keep2, , drop = FALSE]
Xt <- Xt[keep2, , drop = FALSE]
Yl <- Yl[keep2, , drop = FALSE]

# Ověření:
stopifnot(!any(is.na(Yt)), !any(is.na(Yl)), !any(is.na(Xt)))

stan_data <- list(
  T = nrow(Yt), m = m, kx = kx,
  Y = Yt, Ylag = Yl, X = Xt,
  A_sd = A_sd, B_sd = B_sd,
  c_sd = rep(sd_const, m),
  lkj_eta = 2
)

stan_code <- "
data {
  int<lower=1> T;
  int<lower=1> m;
  int<lower=0> kx;
  matrix[T, m] Y;
  matrix[T, m] Ylag;
  matrix[T, kx] X;
  matrix[m, m] A_sd;
  matrix[m, kx] B_sd;
  vector[m] c_sd;
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
  matrix[m, m] L_Sigma;
  L_Sigma = diag_pre_multiply(sigma, L_Omega);
}
model {
  // Minnesota-like priors
  for (i in 1:m) for (j in 1:m) A[i, j] ~ normal(0, A_sd[i, j]);
  for (i in 1:m) for (j in 1:kx) B[i, j] ~ normal(0, B_sd[i, j]);
  c ~ normal(0, c_sd);

  sigma ~ cauchy(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(lkj_eta);

  for (t in 2:T) {
    vector[m] mu = c + A * to_vector(Ylag[t]') + B * to_vector(X[t]');
    Y[t]' ~ multi_normal_cholesky(mu, L_Sigma);
  }
  // první řádek (t=1) ignorujeme (nemá lag) – implicitně slabě informativní
}
generated quantities {
  matrix[m, m] Sigma;
  Sigma = L_Sigma * L_Sigma';
}
"

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = max(1, parallel::detectCores()-1))

set.seed(123)
fit_bvarx <- stan(model_code = stan_code,
                  data = stan_data,
                  chains = 4, iter = 3000, warmup = 1500, seed = 123)

post <- rstan::extract(fit_bvarx)
A_med <- apply(post$A, c(2,3), median)
B_med <- apply(post$B, c(2,3), median)
Sigma_med <- apply(post$Sigma, c(2,3), median)
c_med <- apply(post$c, 2, median)

# Stabilita (ρ) – spektrální poloměr A_med
rho_bvarx <- max(Mod(eigen(A_med, only.values = TRUE)$values))

# In-sample 1Q-ahead predikce exportu (l_x_gap) z BVARX
yt_x <- rep(NA_real_, nrow(Yt))
for (t in 2:nrow(Yt)) {
  mu <- c_med + A_med %*% as.numeric(Yl[t, ]) + B_med %*% as.numeric(Xt[t, ])
  yt_x[t] <- mu[ix_x]
}
rmse_bvarx <- sqrt(mean((Yt[-1, ix_x] - yt_x[-1])^2, na.rm = TRUE))

# =============================================================================
#  C) SROVNÁNÍ A VÝSTUP
# =============================================================================

cat("\n=== Srovnání modelů na stejném vzorku (in-sample, 1Q ahead) ===\n")
cat(sprintf("Stabilita (ρ):   BVAR  = %.3f | BVARX = %.3f\n", rho_bvar, rho_bvarx))
cat(sprintf("RMSE (l_x_gap):  BVAR  = %.4f | BVARX = %.4f\n", rmse_bvar, rmse_bvarx))
cat(sprintf("=> %s\n",
            ifelse(rmse_bvarx < rmse_bvar, "BVARX predikuje lépe (nižší RMSE).",
                   "BVAR predikuje lépe nebo shodně.")))

# Pozn.: Bayes factor/LOO by bylo férové porovnávat jen mezi modely odhadnutými ve STEJNÉM enginu.
# Pokud budeš chtít BF/LOO, doporučuji zapsat i čistý BVAR(1) do stejného Stan kódu (tj. s kx=0).

summary(fit_bvarx, pars = c("A","B","c","sigma"))$summary[, c("Rhat","n_eff")]

hist(post$B[,4,1], main = "Posterior β(l_y_gap → l_x_gap)", xlab = "Hodnota koeficientu")
abline(v = median(post$B[,4,1]), col = "red", lwd = 2)

Y_fit <- Yl %*% t(A_med) + Xt %*% t(B_med) + matrix(c_med, nrow(Yt), length(c_med), byrow = TRUE)
res <- Yt - Y_fit
apply(res, 2, sd)






paleta_fill_B <- c("l_m_gap" = "#9cc5b4", "l_y_gap" = "#e6cbc4")
paleta_color_B <- c("l_m_gap" = "#1c9e77", "l_y_gap" = "#d96005")
paleta_fill_A <- c("l_p_gap" = "#e6cbc4", "l_e_gap" = "#d5d3ec", 
                   "l_ulc_gap" = "#9cc5b4", "l_x_gap" = "#e9cbd5") 
paleta_color_A <- c("l_p_gap" = "#d96005", "l_e_gap" = "#746fb3", 
                    "l_ulc_gap" = "#1c9e77", "l_x_gap" = "#e72b8a") 
color_vline_zero <- "grey40"

