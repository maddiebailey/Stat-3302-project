library(tidyverse)
library(nnet)

# make nat. log function
ln <- function(x)
{
  y <- log(x, base = exp(1))
  return(y)
}

# change vore to factor variable by amt of meat consumed
msleep <- msleep %>% filter(is.na(vore) == FALSE)
msleep_factor <- as.double(factor(msleep$vore, levels = c("herbi", "omni", "carni", "insecti")))
# multinomial regression to check if there is a signficant relationship between
# diet and other variables

# sleep_total
#msleep %>% ggplot(aes(x = vore, y = sleep_total)) +
#  geom_boxplot()

multinom_model <- summary(multinom(msleep_factor ~ sleep_total, 
                                   data = filter(msleep, is.na(sleep_total) == FALSE), 
                                   Hess = TRUE))
multinom_model


# confidence interval :

# body weight
#msleep %>% ggplot(aes(x = vore, y = bodywt)) +
#  geom_boxplot()

# not very helpful
#msleep %>% ggplot(aes(x = vore, y = ln(bodywt))) +
#  geom_boxplot()

multinom_model <- summary(multinom(msleep_factor ~ ln(bodywt), 
                                   data = filter(msleep, is.na(bodywt) == FALSE)),
                          Hess = TRUE)
multinom_model
# compute confidence interval :

# covariance matrix
V(x) <- round(solve(multinom_model$Hessian), 3)

# relative brain wt

msleep <- msleep %>% mutate(
  rel_brainwt = brainwt / bodywt
) %>% filter(is.na(rel_brainwt) == FALSE)

#msleep %>% ggplot(aes(x = vore, y = rel_brainwt)) +
#  geom_boxplot()

multinom_model <- summary(multinom(msleep_factor ~ rel_brainwt, 
                                   data = filter(msleep)),
                          Hess = TRUE)
multinom_model
# compute confidence interval :

# covariance matrix
V(x) <- round(solve(multinom_model$Hessian), 3)

# awake
multinom_model <- summary(multinom(msleep_factor ~ awake, 
                                   data = filter(msleep, is.na(awake == FALSE))),
                          Hess = TRUE)
multinom_model
# compute confidence interval for carnivores :

# covariance matrix
V(x) <-  round(solve(multinom_model$Hessian), 3)


# sleep_cycle
multinom_model <- summary(multinom(msleep_factor ~ sleep_cycle, data = msleep),
                          Hess = TRUE)
multinom_model
# compute confidence interval :

# covariance matrix
V(x) <- round(solve(multinom_model$Hessian), 3)

# sleep_rem 
multinom_model <- summary(multinom(msleep_factor ~ sleep_rem, data = msleep),
                          Hess = TRUE)
multinom_model
# compute confidence interval :

# covariance matrix
V(x) <- round(solve(multinom_model$Hessian), 3)



#AIC stuff 

sleep_factor_total_aic = 128.2543 
sleep_factor_bodyweight_aic = 134.8415 # highest
sleep_factor_brainweight_aic = 132.1439
sleep_factor_awake_aic = 132.1439 
sleep_factor_cycle_aic = 76.54189 # super low!!!
sleep_factor_rem_aic = 102.3385 # second lowest

## AIC PLOTS NUM 1

aic_values = c(sleep_factor_total_aic, sleep_factor_bodyweight_aic, sleep_factor_brainweight_aic, 
                          sleep_factor_awake_aic, sleep_factor_cycle_aic, sleep_factor_rem_aic)
names <- c("factor~total", "factor~body", "factor~brain", 
"factor~awake", "factor~cycle", "factor~rem")

df <- as.data.frame(names) %>% mutate(
  aic_values = aic_values
) %>% as_tibble()

df %>% ggplot(aes(x = names)) +
  geom_bar(aes(weight = aic_values)) + xlab("Models") + ylab("AIC Values")
# need to visualize this


# sleep cycle AND rem

multinom_model <- summary(multinom(msleep_factor ~ sleep_cycle + sleep_rem, data = msleep),
                          Hess = TRUE)
multinom_model

sleep_factor_cycle_rem_aic = 73.85677  # super super low! 


# sleep cycle AND rem AND total sleep
multinom_model <- summary(multinom(msleep_factor ~ sleep_cycle + sleep_rem + sleep_total, data = msleep),
                          Hess = TRUE)
multinom_model

sleep_factor_cycle_rem_total_aic = 78.32453 #going back up again


## AIC PLOTS NUM 2

aic_values_2 = c(sleep_factor_cycle_aic, sleep_factor_rem_aic, sleep_factor_cycle_rem_aic, sleep_factor_cycle_rem_total_aic)
names <- c("factor~cycle", "factor~rem", "factor~cycle+rem", "factor~cycle+rem+total")

df <- as.data.frame(names) %>% mutate(
  aic_values_2 = aic_values_2
) %>% as_tibble()

df %>% ggplot(aes(x = names)) +
  geom_bar(aes(weight = aic_values_2)) + xlab("Models") + ylab("AIC Values")


