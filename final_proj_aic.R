---
title: "Stat 3302 Final Submission"
author: "Maddie Bailey, Jimmy Erkens, Piotr Ignatik, and Melina Raglin"
date: "`r Sys.Date()`"
output: pdf_document
---

# An analysis of mammalian sleep trends following msleep dataset 

## Introduction

### Libraries

```{r}
library(tidyverse)
library(nnet)
```

### Functions

```{r}
# no base natural log function, personally find it easier to work with
ln <- function(x)
{
  y <- log(x, base = exp(1))
  return(y)
}
# no p_value returned by nnet multinom function
p_value <- function(model){
  z <- summary(model)$coefficients/summary(model)$standard.errors
  
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  return(p)
  }
```

### Data Cleaning

```{r}
# must change diet to a factor variable
msleep <- msleep %>% filter(is.na(vore) == FALSE) %>% 
  # eats meat or doesn't eat meat, just for the sake of cleaner data
  mutate(meat = ifelse(vore == "herbi", FALSE, TRUE)) %>% 
  # create relative brain weight variable
  mutate(rel_brainwt = 100 * brainwt / bodywt) %>% 
  filter(is.na(rel_brainwt) == FALSE)
# whether or not they eat meat, herbivore baseline
meat_factor <- as.double(factor(msleep$meat, levels = c(FALSE, TRUE)))
# what diet are they? omnivore is the baseline
omni_factor <- as.double(factor(msleep$vore, levels = c("omni", "herbi", "carni", "insecti")))
```

## Hypotheses Tested

### There is a relationship between diet category and total amount of sleep

```{r}
# visualize the data
msleep %>% ggplot(aes(x = vore, y = sleep_total)) +
  geom_boxplot() +
  ggtitle("Diet and Total Sleep") +
  xlab("Diet Category") +
  ylab("Total sleep in hours")
# create regression
sleep_total_model <- multinom(omni_factor ~ sleep_total, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 0.2098 + 0.024 - 0.219
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_total_model$Hessian), 3)
alpha_matrix <- matrix(c(0, -1, 0, -1, 0, -1), nrow = 1, ncol = 6)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_total_model)
```
 
### There is a relationship between eating meat and total amount of sleep

```{r}
msleep %>% ggplot(aes(x = meat, y = sleep_total)) +
  geom_boxplot() +
  ggtitle("Meat and Total Sleep") +
  xlab("Eats meat") +
  ylab("Total sleep in hours")
# create regression
sleep_total_meat <- multinom(meat_factor ~ sleep_total, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for meat
alpha <- 0.2258
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_total_meat$Hessian), 3)
alpha_matrix <- matrix(c(0, 1), nrow = 1, ncol = 2)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_total_meat)
```

### There is a relationship between diet category and total amount of REM sleep

```{r}
# visualize the data
msleep %>% ggplot(aes(x = vore, y = sleep_rem)) +
  geom_boxplot() +
  ggtitle("Diet and REM Sleep") +
  xlab("Diet Category") +
  ylab("Total REM sleep in hours")
# create regression
sleep_rem_model <- multinom(omni_factor ~ sleep_rem, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 1.145 - 0.3013 - 0.8851
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_rem_model$Hessian), 3)
alpha_matrix <- matrix(c(0, -1, 0, -1, 0, -1), nrow = 1, ncol = 6)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_rem_model)
```

### There is a relationship between eating meat and total amount of REM sleep

```{r}
# visualize the data
msleep %>% ggplot(aes(x = meat, y = sleep_rem)) +
  geom_boxplot() +
  ggtitle("Meat and REM Sleep") +
  xlab("Eats meat") +
  ylab("Total REM sleep in hours")
# create regression
sleep_rem_meat <- multinom(meat_factor ~ sleep_rem, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 1.287
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_rem_meat$Hessian), 3)
alpha_matrix <- matrix(c(0, 1), nrow = 1, ncol = 2)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_rem_meat)
```

### There is a relationship between diet category and ln (Body Weight)

```{r}
# visualize the data
msleep %>% ggplot(aes(x = vore, y = ln(bodywt))) +
  geom_boxplot() +
  ggtitle("Diet and ln(Body Weight)") +
  xlab("Diet Category") +
  ylab("ln(Body Weight)")
# create regression
sleep_bodywt_model <- multinom(omni_factor ~ ln(bodywt), 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- -0.1964 - 0.1918 + 0.1570
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_bodywt_model$Hessian), 3)
alpha_matrix <- matrix(c(0, -1, 0, -1, 0, -1), nrow = 1, ncol = 6)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_bodywt_model)
```

### There is a relationship between diet category and relative brain weight

```{r}
# visualize the data
msleep %>% ggplot(aes(x = vore, y = rel_brainwt)) +
  geom_boxplot() +
  ggtitle("Diet and Relative Brain Weight") +
  xlab("Diet Category") +
  ylab("Relative Brain Weight")
# create regression
sleep_relbrain_model <- multinom(omni_factor ~ rel_brainwt, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 1.070 + 1.087 + 0.308
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_relbrain_model$Hessian), 3)
alpha_matrix <- matrix(c(0, -1, 0, -1, 0, -1), nrow = 1, ncol = 6)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_relbrain_model)
```

### There is a relationship between eating meat and relative brain weight

```{r}
# visualize the data
msleep %>% ggplot(aes(x = meat, y = rel_brainwt)) +
  geom_boxplot() +
  ggtitle("Meat and Relative Brain Weight") +
  xlab("Eats Meat") +
  ylab("Relative Brain Weight")
# create regression
sleep_relbrain_meat <- multinom(meat_factor ~ rel_brainwt, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 0.749
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_relbrain_meat$Hessian), 3)
alpha_matrix <- matrix(c(0, 1), nrow = 1, ncol = 2)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_relbrain_meat)
```

### There is a relationship between diet and sleep cycle

```{r}
# visualize the data
msleep %>% ggplot(aes(x = vore, y = sleep_cycle)) +
  geom_boxplot() +
  ggtitle("Diet and Relative Brain Weight") +
  xlab("Diet Category") +
  ylab("Sleep Cycle")
# create regression
sleep_cycle_omni <- multinom(omni_factor ~ sleep_cycle, 
                          data = filter(msleep, is.na(sleep_total) == FALSE), 
                          Hess = TRUE)
# test statistic for omnivores
alpha <- 1.58 + 1.77 + 18.90
exp(alpha)

# for confidence interval
covariance <- round(solve(sleep_cycle_omni$Hessian), 3)
alpha_matrix <- matrix(c(0, -1, 0, -1, 0, -1), nrow = 1, ncol = 6)
se_alpha <- sqrt(alpha_matrix %*% covariance %*% t(alpha_matrix))

LB <- alpha - (1.96 * se_alpha)
UB <- alpha + (1.96 * se_alpha)
exp(LB)
exp(UB)

# p values (which ones are significant)
p_value(sleep_cycle_omni)
```

### Assessing the strength of these models through AIC

```{r}
# derived 
sleep_factor_total_aic = 128.2543 
sleep_factor_bodyweight_aic = 134.8415 # highest
sleep_factor_brainweight_aic = 132.1439
sleep_factor_awake_aic = 132.1439 
sleep_factor_cycle_aic = 76.54189 # super low!!!
sleep_factor_rem_aic = 102.3385 # second lowest

aic_values = c(sleep_factor_total_aic, sleep_factor_bodyweight_aic, sleep_factor_brainweight_aic, 
                          sleep_factor_awake_aic, sleep_factor_cycle_aic, sleep_factor_rem_aic)
names <- c("factor~total", "factor~body", "factor~brain", 
"factor~awake", "factor~cycle", "factor~rem")

df <- as.data.frame(names) %>% mutate(
  aic_values = aic_values
) %>% as_tibble()

df %>% ggplot(aes(x = names)) +
  geom_bar(aes(weight = aic_values)) + xlab("Models") + ylab("AIC Values") +
  ggtitle("Comparing AIC Values Between Models")

# A model with Sleep Cycle and REM sleep

multinom_model <- summary(multinom(msleep_factor ~ sleep_cycle + sleep_rem, data = msleep),
                          Hess = TRUE)
multinom_model

sleep_factor_cycle_rem_aic = 73.85677  # super super low! 

# not relevant for hypothesis testing but based on AIC values this is our best model


# sleep cycle AND rem AND total sleep
multinom_model <- summary(multinom(msleep_factor ~ sleep_cycle + sleep_rem + sleep_total, data = msleep),
                          Hess = TRUE)
multinom_model

sleep_factor_cycle_rem_total_aic = 78.32453 #going back up again
```

### An interaction effect is needed to model the relation between brain weight, body weight, and diet category

```{r}
multinom_model <- multinom(omni_factor ~ ln(bodywt) * ln(brainwt), 
                          data = filter(msleep, is.na(bodywt) == FALSE),
                          Hess = TRUE)
summary(multinom_model)
```

Example function :
log(p2/p1) = -6.86 + 1.32(ln(brainwt)) - 1.42(ln(bodywt)) + 0.017(ln(bodywt):ln(brainwt))

```{r}
p_value(multinom_model)
```


### There is a relationship between conservation level and total amount of sleep

### There is a relationship between conservation level and ln(Body Weight)

### There is a relationship between domesticated status and relative brain weight

### There is a relationship between conservation level and relative brain weight
