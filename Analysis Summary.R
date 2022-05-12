#### R Code for Analysis Summary ####
# This code takes in a dataset, and then runs an analysis on the given dataset. It then copies the analysis data table, and you can paste it on excel.
# In the output, the rows are representing the outputs for a single allele. This includes the name, logit of the percentage,
# estimated transmission rate, lower and upper confidence interval, raw p value, adjusted p value, expression category, and number of crosses.

# The analysis done is a special case of generalized linear models, with a logit function. We took an extra binomial variation approach to the analysis.
# This is a quasibinomial logistic regression model
# More details provided on the analysis in "Analysis Summary" page on Onenote

#install.packages("tidyverse")
# If not installed, remove "#" and run the line above
library(tidyverse)



#### Loading Data ####

male_df <- read.table(file = "./data/raw_data/18192021_complete_data.tsv",
                              header = TRUE,
                              sep = '\t')
# This saves the data from the file desired. Here it's very simple, if you add a new tsv file that you want analyzed, just change
# the naming in the line after "file =" and in the quotations "Analysis Summary", put the location of your new file.

#####################


vc_high<- male_df[male_df$category == "vegetative_cell_high", ] 
sc_high <- male_df[male_df$category == "sperm_cell_high", ]
seedling_only <- male_df[male_df$category == "seedling_only", ]
#The lines above seperate the dataset into subsets based on the expression category.








#VEGETATIVE CELL:







#### Analysis ####

#Vegetative Cell:

# These variables are being removed as they are used within each categories code to generate output
# I just thought it would be cleaner to clear the data, and then create it again. Although, this is not necessary as the code will work either way.

#GLM
quasi_binomial_log_model <- glm(cbind(number_of_GFP_kernels, number_of_WT_kernels) ~ # using the ratio between kernels as the output
                              as.factor(allele), # allele as explanatory variable, factored and there are multiple crosses per allele
                            data = vc_high, # data from  subset, changes depending on expression category
                            family = quasibinomial(link = "logit")) # this is where the logit function is specificed, and quasibinomial approach
print(quasi_binomial_log_model)
#This analysis is done automatically by rstudio, however the analysis uses a maximum likelihood approach which assumes that the data is independant.
#We are not sure if it's independant, so it's best to assume that it's not and therefore a different approach was taken to calculate the p values.
#The alternate approach is to use standard deviation in place of standard error. The standard deviation is calculated by the variance.
#It uses the formula of the variance of 2 random variables
#Var(X + Y) = Var(X) + Var(Y) + 2Cov(X,Y)
#This is easily calcualted using the variance covariance matrix below.

S = vcov(quasi_binomial_log_model)#variance covariance matrix


raw_p = coef(summary(quasi_binomial_log_model))[1,4] #raw p value of intercept
beta_list = coef(summary(quasi_binomial_log_model))[1,1] #intercept coeffecient value
estimated_coeffecient = (exp(beta_list)/(1+exp(beta_list))) #estimated percentage between kernels based on beta
low_int = exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])) #Getting the lower confidence interval of the estimate
up_int = exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2]))
#Adding first value, beta, raw p, and estimated coeffecient and using those values to calculated confidence interval
#
#Collecting the first allele (intercept) 

#This for loop below collects the glm info but calculated raw p using alternate method talked about above

for (i in 2:nrow(coef(summary(quasi_binomial_log_model)))){ #starting at 2 because we already collected the firs tone...
  beta = coef(summary(quasi_binomial_log_model))[1,1] + coef(summary(quasi_binomial_log_model))[i,1] #Intercept + Allele coeffecient
  sigma = sqrt(sum(S[c(1,i),c(1,i)])) #sigma from variance matrix
  raw_p = c(raw_p, 2*(1-pnorm(abs(beta/sigma)))) #raw p calculated using z normal distribution
  beta_list = c(beta_list, beta)
  estimated_coeffecient = c(estimated_coeffecient, (exp(beta)/(1+exp(beta))))
  low_int = c(low_int, exp(beta-qnorm(.975)*sigma)/(1+exp(beta-qnorm(.975)*sigma)))
  up_int = c(up_int, exp(beta+qnorm(.975)*sigma)/(1+exp(beta+qnorm(.975)*sigma)))
  
}

adjusted_p = p.adjust(raw_p, method = "BH") #Being more conservative with our p values due to uncertainty and using BH method to do so, built in R

#finished getting raw p values from the model
#now we need to organize the data correctly

Alleles = sort(unique(vc_high$allele)) #creating unique allele vector

#reorganizing the data to be in the same order to eachother

names(raw_p) = sort(unique(vc_high$allele)) #names is name or row, unique only picks unique value from dataset
names(beta_list) = sort(unique(vc_high$allele))
names(estimated_coeffecient) = sort(unique(vc_high$allele))
names(Alleles) = sort(unique(vc_high$allele))
names(adjusted_p) = sort(unique(vc_high$allele))
names(low_int) = sort(unique(vc_high$allele))
names(up_int) = sort(unique(vc_high$allele))

#This helps sort the data correctly in the table with respect to the allele names



analysis_summary_vc = data.frame(allele = Alleles, logit_of_percentage = beta_list, estimated_tranmission_percentage = estimated_coeffecient, lower_confidence_interval = low_int, upper_confidence_interval = up_int, raw_p_value = raw_p, adj_p_value = adjusted_p)
#Combining all the organized vectors into one large data table for vegetative cell analysis output.
#This is done for each expression category and is combines at the very end to have all the analysis output
#Finished creating output for 

analysis_summary_vc$expression_category = "Vegetative Cell"














#SPERM CELL










#### Analysis ####

#Sperm Cell:

# These variables are being removed as they are used within each categories code to generate output
# I just thought it would be cleaner to clear the data, and then create it again. Although, this is not necessary as the code will work either way.

#GLM
quasi_binomial_log_model <- glm(cbind(number_of_GFP_kernels, number_of_WT_kernels) ~ # using the ratio between kernels as the output
                                  as.factor(allele), # allele as explanatory variable, factored and there are multiple crosses per allele
                                data = sc_high, # data from  subset, changes depending on expression category
                                family = quasibinomial(link = "logit")) # this is where the logit function is specificed, and quasibinomial approach
print(quasi_binomial_log_model)
#This analysis is done automatically by rstudio, however the analysis uses a maximum likelihood approach which assumes that the data is independant.
#We are not sure if it's independant, so it's best to assume that it's not and therefore a different approach was taken to calculate the p values.
#The alternate approach is to use standard deviation in place of standard error. The standard deviation is calculated by the variance.
#It uses the formula of the variance of 2 random variables
#Var(X + Y) = Var(X) + Var(Y) + 2Cov(X,Y)
#This is easily calcualted using the variance covariance matrix below.

S = vcov(quasi_binomial_log_model)#variance covariance matrix


raw_p = coef(summary(quasi_binomial_log_model))[1,4] #raw p value of intercept
beta_list = coef(summary(quasi_binomial_log_model))[1,1] #intercept coeffecient value
estimated_coeffecient = (exp(beta_list)/(1+exp(beta_list))) #estimated percentage between kernels based on beta
low_int = exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])) #Getting the lower confidence interval of the estimate
up_int = exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2]))
#Adding first value, beta, raw p, and estimated coeffecient and using those values to calculated confidence interval
#
#Collecting the first allele (intercept) 

#This for loop below collects the glm info but calculated raw p using alternate method talked about above

for (i in 2:nrow(coef(summary(quasi_binomial_log_model)))){ #starting at 2 because we already collected the firs tone...
  beta = coef(summary(quasi_binomial_log_model))[1,1] + coef(summary(quasi_binomial_log_model))[i,1] #Intercept + Allele coeffecient
  sigma = sqrt(sum(S[c(1,i),c(1,i)])) #sigma from variance matrix
  raw_p = c(raw_p, 2*(1-pnorm(abs(beta/sigma)))) #raw p calculated using z normal distribution
  beta_list = c(beta_list, beta)
  estimated_coeffecient = c(estimated_coeffecient, (exp(beta)/(1+exp(beta))))
  low_int = c(low_int, exp(beta-qnorm(.975)*sigma)/(1+exp(beta-qnorm(.975)*sigma)))
  up_int = c(up_int, exp(beta+qnorm(.975)*sigma)/(1+exp(beta+qnorm(.975)*sigma)))
  
}

adjusted_p = p.adjust(raw_p, method = "BH") #Being more conservative with our p values due to uncertainty and using BH method to do so, built in R

#finished getting raw p values from the model
#now we need to organize the data correctly

Alleles = sort(unique(sc_high$allele)) #creating unique allele vector

#reorganizing the data to be in the same order to eachother

names(raw_p) = sort(unique(sc_high$allele)) #names is name or row, unique only picks unique value from dataset
names(beta_list) = sort(unique(sc_high$allele))
names(estimated_coeffecient) = sort(unique(sc_high$allele))
names(Alleles) = sort(unique(sc_high$allele))
names(adjusted_p) = sort(unique(sc_high$allele))
names(low_int) = sort(unique(sc_high$allele))
names(up_int) = sort(unique(sc_high$allele))

#This helps sort the data correctly in the table with respect to the allele names



analysis_summary_sc = data.frame(allele = Alleles, logit_of_percentage = beta_list, estimated_tranmission_percentage = estimated_coeffecient, lower_confidence_interval = low_int, upper_confidence_interval = up_int, raw_p_value = raw_p, adj_p_value = adjusted_p)
#Combining all the organized vectors into one large data table for vegetative cell analysis output.
#This is done for each expression category and is combines at the very end to have all the analysis output
#Finished creating output for 

analysis_summary_sc$expression_category = "Sperm Cell"









#SEEDLING ONLY:



#### Analysis ####

#Sperm Cell:

# These variables are being removed as they are used within each categories code to generate output
# I just thought it would be cleaner to clear the data, and then create it again. Although, this is not necessary as the code will work either way.

#GLM
quasi_binomial_log_model <- glm(cbind(number_of_GFP_kernels, number_of_WT_kernels) ~ # using the ratio between kernels as the output
                                  as.factor(allele), # allele as explanatory variable, factored and there are multiple crosses per allele
                                data = seedling_only, # data from  subset, changes depending on expression category
                                family = quasibinomial(link = "logit")) # this is where the logit function is specificed, and quasibinomial approach
print(quasi_binomial_log_model)
#This analysis is done automatically by rstudio, however the analysis uses a maximum likelihood approach which assumes that the data is independant.
#We are not sure if it's independant, so it's best to assume that it's not and therefore a different approach was taken to calculate the p values.
#The alternate approach is to use standard deviation in place of standard error. The standard deviation is calculated by the variance.
#It uses the formula of the variance of 2 random variables
#Var(X + Y) = Var(X) + Var(Y) + 2Cov(X,Y)
#This is easily calcualted using the variance covariance matrix below.

S = vcov(quasi_binomial_log_model)#variance covariance matrix


raw_p = coef(summary(quasi_binomial_log_model))[1,4] #raw p value of intercept
beta_list = coef(summary(quasi_binomial_log_model))[1,1] #intercept coeffecient value
estimated_coeffecient = (exp(beta_list)/(1+exp(beta_list))) #estimated percentage between kernels based on beta
low_int = exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list - qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])) #Getting the lower confidence interval of the estimate
up_int = exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2])/(1+exp(beta_list + qnorm(.975)*coef(summary(quasi_binomial_log_model))[1,2]))
#Adding first value, beta, raw p, and estimated coeffecient and using those values to calculated confidence interval
#
#Collecting the first allele (intercept) 

#This for loop below collects the glm info but calculated raw p using alternate method talked about above

for (i in 2:nrow(coef(summary(quasi_binomial_log_model)))){ #starting at 2 because we already collected the firs tone...
  beta = coef(summary(quasi_binomial_log_model))[1,1] + coef(summary(quasi_binomial_log_model))[i,1] #Intercept + Allele coeffecient
  sigma = sqrt(sum(S[c(1,i),c(1,i)])) #sigma from variance matrix
  raw_p = c(raw_p, 2*(1-pnorm(abs(beta/sigma)))) #raw p calculated using z normal distribution
  beta_list = c(beta_list, beta)
  estimated_coeffecient = c(estimated_coeffecient, (exp(beta)/(1+exp(beta))))
  low_int = c(low_int, exp(beta-qnorm(.975)*sigma)/(1+exp(beta-qnorm(.975)*sigma)))
  up_int = c(up_int, exp(beta+qnorm(.975)*sigma)/(1+exp(beta+qnorm(.975)*sigma)))
  
}

adjusted_p = p.adjust(raw_p, method = "BH") #Being more conservative with our p values due to uncertainty and using BH method to do so, built in R

#finished getting raw p values from the model
#now we need to organize the data correctly

Alleles = sort(unique(seedling_only$allele)) #creating unique allele vector

#reorganizing the data to be in the same order to eachother

names(raw_p) = sort(unique(seedling_only$allele)) #names is name or row, unique only picks unique value from dataset
names(beta_list) = sort(unique(seedling_only$allele))
names(estimated_coeffecient) = sort(unique(seedling_only$allele))
names(Alleles) = sort(unique(seedling_only$allele))
names(adjusted_p) = sort(unique(seedling_only$allele))
names(low_int) = sort(unique(seedling_only$allele))
names(up_int) = sort(unique(seedling_only$allele))

#This helps sort the data correctly in the table with respect to the allele names



analysis_summary_seed = data.frame(allele = Alleles, logit_of_percentage = beta_list, estimated_tranmission_percentage = estimated_coeffecient, lower_confidence_interval = low_int, upper_confidence_interval = up_int, raw_p_value = raw_p, adj_p_value = adjusted_p)
#Combining all the organized vectors into one large data table for vegetative cell analysis output.
#This is done for each expression category and is combines at the very end to have all the analysis output
#Finished creating output for 



analysis_summary_seed$expression_category = "Seedling Only"










#combining the seperate analysis for each expression categories and combining




analysis_summary = rbind(analysis_summary_vc, analysis_summary_sc)
analysis_summary = rbind(analysis_summary, analysis_summary_seed)

#copying to clipboard so data frame can be pasted elsewhere like excel

write.table(analysis_summary, "clipboard", sep = "\t") #Copies the table to your computer, you can now paste it anywhere.
