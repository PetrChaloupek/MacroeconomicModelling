library(readxl)
library(fixest)
library(ggplot2)
library(dplyr)

data <- read_excel("Library/CloudStorage/OneDrive-MUNI/Škola/Mezinárodní politická ekonomie/df.xlsx", 
                 col_types = c("text", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))

data$log_GDP_pc <- log(data$GDP_pc)

data$DiD <- data$Treatment * data$Post

model_did <- lm(log_GDP_pc ~ Treatment + Post + DiD, 
              data = data)

summary(model_did)

model_fe <- feols(log_GDP_pc ~ DiD | Country + Year, data = data)
summary(model_fe)


# Skupiny a období
data$Group <- ifelse(data$Treatment == 1, "Marshall Plan", "No Plan")
data$Period <- ifelse(data$Year < 1948, "Pre", "Post")
data$GroupPeriod <- paste(data$Group, data$Period, sep = " - ")

# Graf s trendovými liniemi a legendou dole
ggplot(data, aes(x = Year, y = log_GDP_pc, color = GroupPeriod)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  geom_vline(xintercept = 1948, linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "Vývoj log HDP per capita podle skupiny a období",
    subtitle = "4 trajektorie s lineárními trendy – Difference-in-Differences styl",
    x = "Rok",
    y = "log HDP per capita",
    color = "Skupina a období"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
