set.seed(123)

n_healthy  <- 500
n_diseased <- 500

# CRP (mg/L) - highly skewed, log-normal
crp_healthy  <- rlnorm(n_healthy,  meanlog = 0.0, sdlog = 0.7)  # low CRP
crp_diseased <- rlnorm(n_diseased, meanlog = 2.0, sdlog = 0.7)  # much higher CRP

# Glucose (mmol/L) - roughly normal
gluc_healthy  <- rnorm(n_healthy,  mean = 5.0, sd = 0.7)
gluc_diseased <- rnorm(n_diseased, mean = 9.0, sd = 1.5)

# Avoid non-positive glucose values for logs
gluc_healthy  <- pmax(gluc_healthy,  0.1)
gluc_diseased <- pmax(gluc_diseased, 0.1)

# Combine into one data frame
library(dplyr)

df <- tibble(
  group = factor(c(rep("Healthy", n_healthy), rep("Diseased", n_diseased))),
  crp   = c(crp_healthy, crp_diseased),
  gluc  = c(gluc_healthy, gluc_diseased)
) %>%
  mutate(
    log_crp  = log(crp),
    log_gluc = log(gluc),
    disease  = if_else(group == "Diseased", 1, 0)  # outcome for ML
  )


library(ggplot2)

# To avoid the extreme tail dominating, cap x-axis at 99th percentile
crp_cap <- quantile(df$crp, 0.99)

ggplot(df, aes(x = crp, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  coord_cartesian(xlim = c(0, crp_cap)) +
  labs(
    title = "CRP distribution in levels",
    x = "CRP (mg/L)",
    y = "Density"
  )


ggplot(df, aes(x = log_crp, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  labs(
    title = "CRP distribution after log-transform",
    x = "log(CRP)",
    y = "Density"
  )


gluc_cap <- quantile(df$gluc, 0.99)

ggplot(df, aes(x = gluc, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
  coord_cartesian(xlim = c(0, gluc_cap)) +
  labs(
    title = "Glucose distribution in levels",
    x = "Glucose (mmol/L)",
    y = "Density"
  )

ggplot(df, aes(x = log_gluc, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
  labs(
    title = "Glucose distribution after log-transform",
    x = "log(Glucose)",
    y = "Density"
  )


# Raw CRP model
m_raw <- glm(disease ~ crp, data = df, family = binomial())

# Log-CRP model
m_log <- glm(disease ~ log_crp, data = df, family = binomial())

summary(m_raw)
summary(m_log)

####################
library(ggplot2)
library(dplyr)

df_small <- df %>% 
  select(crp) %>%
  mutate(
    scaled = scale(crp),
    log_transformed = log(crp)
  )

# Melt to long format
library(tidyr)
df_long <- df_small %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 40, alpha = 0.6) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "CRP: Raw vs Scaled vs Log-Transformed",
    x = "",
    y = "Frequency"
  )


#Scaling changes the units.
Log-transform changes the relationship.
