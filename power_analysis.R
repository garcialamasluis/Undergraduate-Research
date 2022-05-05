#First attempt at power analysis
#Currently only a single test

library(dplyr)
#We will say our initial p = .5
#This implies that the logit(p), the beta associated with it is log(1)=0.
NUMBER_OF_CROSSES = 7 #This is the number you change to get a new power table
alpha = .0056
p = .5
n = 350*NUMBER_OF_CROSSES #350, based off of previous data avg number of kernels on each data point
Beta = log(p/(1-p))
sd = sqrt(dp_vc/(n*p*(1-p)))
cutoff = abs(qnorm(1-(alpha/2), mean = Beta, sd = 1))

transmission_rates <- seq(from = .35, to = .65, by = .005) #lowest lower confidence was .2806, and the highest upper confidence is .611

data <- data.frame(transmission_rates)
data$Beta = log(data$transmission_rates/(1-data$transmission_rates))
data$dp_vc = dp_vc
data$dp_sc = dp_sc
data$dp_seed = dp_seed
data$SD_vc = sqrt(dp_vc/(n*data$transmission_rates*(1-data$transmission_rates)))
data$SD_sc = sqrt(dp_sc/(n*data$transmission_rates*(1-data$transmission_rates)))
data$SD_seed = sqrt(dp_seed/(n*data$transmission_rates*(1-data$transmission_rates)))
data$mu_hat_vc = data$Beta/data$SD_vc
data$mu_hat_sc = data$Beta/data$SD_sc
data$mu_hat_seed = data$Beta/data$SD_seed
data$power_vc = (1-(pnorm(cutoff, mean = data$mu_hat_vc, sd = 1)-pnorm(-cutoff, mean = data$mu_hat_vc, sd = 1)))
data$power_sc = (1-(pnorm(cutoff, mean = data$mu_hat_sc, sd = 1)-pnorm(-cutoff, mean = data$mu_hat_sc, sd = 1)))
data$power_seed = (1-(pnorm(cutoff, mean = data$mu_hat_seed, sd = 1)-pnorm(-cutoff, mean = data$mu_hat_seed, sd = 1)))

write.table(data, "clipboard", sep = "\t")
#data$type2_error = 1-data$power
