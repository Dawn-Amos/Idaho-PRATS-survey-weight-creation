
#### loading packages and datasets ############################################

library(tidyverse)
library(survey) 
library(weights) 
library(janitor)

rm(list = ls())

#final merged dataset
dataset = read.csv("PRATS2024_final_dataset.csv")
#selected sample (wave 1 and 2)
selected_w1 = read.csv("sample2024_w1.csv") 
selected_w2 = read.csv("sample2024_w2.csv")
#sampling frame (wave 1 and 2)
frame_w1 = read.csv("frame2024_w1.csv")
frame_w2 = read.csv("frame2024_w2.csv")

#### preparing datasets (frame, sample, final dataset) #########################

#inserting frame wave
frame_w1$wave = 1
frame_w2$wave = 2

#creating one merged dataframe for the frame and sample
full.frame = rbind(frame_w1, frame_w2) %>% 
  select(yrsfn, sfnyr, wave, distnm, county, mage, u_mhisp)
full.sample = rbind(selected_w1, selected_w2) %>% 
  select(respID, yrsfn, sfnyr, wave, distnm, county, mage, u_mhisp, hisp)

#creating a categorical variable for age  
#will be used in post-stratification weights
cat_age_var = function(df, age_var) {
  df %>% 
    mutate(
      age.cat = case_when( 
        {{age_var}} <= 29 ~ "18 to 29", 
        {{age_var}} >= 30 ~ "30+"))
}
dataset = cat_age_var(dataset, momage)
full.frame = cat_age_var(full.frame, mage)
full.sample = cat_age_var(full.sample, mage)

#### quality check #############################################################

#double checking there aren't mismatched ethnicity values.
sum(dataset[dataset$u_mhisp == 0 & dataset$hisp != 0, ])
sum(dataset[dataset$u_mhisp == 1 & dataset$hisp != 1, ]) 
#checking for missing values in variables needed for weighting
sum(is.na(dataset$PRATSID))
sum(is.na(dataset$respID))
sum(is.na(dataset$mage))
sum(is.na(dataset$hisp), dataset$hisp == 9)

#### base weight ############################################################### 
#Based on selection probability and non-response. 
  #NOTE 1: See PRATS methodology doc for more information about sampling methodology.
  #NOTE 2: For ethnicity, 0 = non-Hispanic and 1 = Hispanic

dataset = dataset %>% 
  mutate(
    #creating N for finite population correction (fpc)
    N = case_when(  
      u_mhisp == 0 & wave == 1 ~ sum(full.frame$u_mhisp == 0 & full.frame$wave == 1), 
      u_mhisp == 1 & wave == 1 ~ sum(full.frame$u_mhisp == 1 & full.frame$wave == 1), 
      u_mhisp == 0 & wave == 2 ~ sum(full.frame$u_mhisp == 0 & full.frame$wave == 2), 
      u_mhisp == 1 & wave == 2 ~ sum(full.frame$u_mhisp == 1 & full.frame$wave == 2)), 
    #creating strata
    strata = case_when( 
      u_mhisp == 0 & wave == 1 ~ "W1, Non-Hispanic", 
      u_mhisp == 1 & wave == 1 ~ "W1, Hispanic", 
      u_mhisp == 0 & wave == 2 ~ "W2, Non-Hispanic", 
      u_mhisp == 1 & wave == 2 ~ "W2, Hispanic"))
#checking work
dataset %>% 
  group_by(wave, u_mhisp, N, strata) %>% 
  summarize(count = n()) %>% 
  mutate(weight = N/count) %>% 
  print(n = Inf)

#creating base weight
base.design = svydesign(ids = ~1,
                        strata = ~strata, 
                        fpc = ~N,
                        data = dataset)
dataset$base.w = weights(base.design)

#review work 
  #NOTE: "weight_est" should match the "base.w" value
dataset %>% 
  group_by(N, strata, base.w) %>% 
  summarise(count = n()) %>% 
  mutate(weight_est = N/count)

#### post-stratification - raking (iterative proportional fitting) #############

#NOTE: We will have 8 bins: 2 waves x 2 ethnicity groups x 2 age groups

#organizing respondents into their final 8 bins for weighing  
#this variable will be used in raking
dataset = dataset %>% 
  mutate( 
    age.hisp.wave = case_when( 
      #Wave 1
      u_mhisp == 0 & age.cat == "18 to 29" & wave == 1 ~ "W1, Non-Hispanic, 18 to 29", 
      u_mhisp == 0 & age.cat == "30+" & wave == 1 ~ "W1, Non-Hispanic, 30+", 
      u_mhisp == 1 & age.cat == "18 to 29" & wave == 1 ~ "W1, Hispanic, 18 to 29", 
      u_mhisp == 1 & age.cat == "30+" & wave == 1 ~ "W1, Hispanic, 30+", 
      #Wave 2
      u_mhisp == 0 & age.cat == "18 to 29" & wave == 2 ~ "W2, Non-Hispanic, 18 to 29", 
      u_mhisp == 0 & age.cat == "30+" & wave == 2 ~ "W2, Non-Hispanic, 30+", 
      u_mhisp == 1 & age.cat == "18 to 29" & wave == 2 ~ "W2, Hispanic, 18 to 29", 
      u_mhisp == 1 & age.cat == "30+" & wave == 2 ~ "W2, Hispanic, 30+"))
#checking work
dataset %>% 
  group_by(wave, u_mhisp, age.cat, age.hisp.wave) %>% 
  summarize(count = n()) %>% 
  print(n = Inf)
  
#defining population margins from the sampling frame 
pop.age.hisp.wave = data.frame(
  age.hisp.wave = c("W1, Hispanic, 18 to 29", "W1, Hispanic, 30+", 
                    "W1, Non-Hispanic, 18 to 29", "W1, Non-Hispanic, 30+",
                    "W2, Hispanic, 18 to 29", "W2, Hispanic, 30+",
                    "W2, Non-Hispanic, 18 to 29", "W2, Non-Hispanic, 30+"), 
  Freq = c(sum(full.frame$wave == "1" & full.frame$u_mhisp == 1 & full.frame$age.cat == "18 to 29"), 
           sum(full.frame$wave == "1" & full.frame$u_mhisp == 1 & full.frame$age.cat == "30+"), 
           sum(full.frame$wave == "1" & full.frame$u_mhisp == 0 & full.frame$age.cat == "18 to 29"), 
           sum(full.frame$wave == "1" & full.frame$u_mhisp == 0 & full.frame$age.cat == "30+"), 
           sum(full.frame$wave == "2" & full.frame$u_mhisp == 1 & full.frame$age.cat == "18 to 29"), 
           sum(full.frame$wave == "2" & full.frame$u_mhisp == 1 & full.frame$age.cat == "30+"), 
           sum(full.frame$wave == "2" & full.frame$u_mhisp == 0 & full.frame$age.cat == "18 to 29"), 
           sum(full.frame$wave == "2" & full.frame$u_mhisp == 0 & full.frame$age.cat == "30+")))

#NOTE: Because we have less bins this year, I am choosing to rake over all 8 bins.
#This allows me to correct representation issues within the interaction of the three variables. 
#Thus, if young Hispanic women in Wave 1 specifically weren't accurately represented, I can fix that. 

#raking (iterative proportional fitting)
raked.design = rake(
  design = base.design, 
    #pulling age.hisp.wave variable from dataset
  sample.margins = list(~age.hisp.wave),
  population.margins = list(pop.age.hisp.wave)) 
dataset$raked.w = weights(raked.design)

#### reviewing weights ######################################################### 

dataset %>% 
  group_by(strata, age.cat, base.w, raked.w) %>% 
  summarize(count = n()) %>% 
  print(n = Inf)

#checking distribution for outliers
summary(dataset$raked.w)
hist(dataset$raked.w, breaks = 30, 
     main = "Raked Weights, 2024", xlab = "Weights", xlim = c(18, 40))

#checking coefficient variation  
sd(weights(raked.design))/mean(weights(raked.design))

#checking Kish's effective sample size
deff.kish = sum(weights(raked.design))^2/sum(weights(raked.design)^2)
nrow(dataset)-deff.kish
(eff.pct = (deff.kish / nrow(dataset)) * 100)

############## trimming weights ################################################
  
#setting cut-off point for weight trimming
upper.limit = 2*mean(dataset$raked.w)
upper.limit < max(dataset$raked.w)

#The max value of the raked weights is less than 2 standard deviations from the mean 
#Thus, I won't be trimming the raked weights.  

############## cleaning up final dataset ####################################### 

final.dataset = dataset %>%
  select(-c(age.hisp.wave, base.w, respID)) %>% 
  rename(weight = raked.w) %>% 
  relocate(wave:u_fhisp, .after = "Q62B") %>% 
  relocate(c(weight, strata), .after = "PRATSID") %>% 
  relocate(c(age.cat, N), .after = "wave")

############## writing final csv file ##########################################

write.csv(final.dataset, "PRATS2024, 10.28.2026 full merge.csv", row.names = F)

############ summary of survey weights and counts ##############################

#summary of final dataset (responses)
counts_responses = final.dataset  %>% 
  group_by(wave, u_mhisp, age.cat, weight) %>% 
  summarize(sample.count = n()) %>% 
  mutate(sample.proportion = (sample.count/nrow(final.dataset))*100, 
         wave = case_when( 
           wave == 1 ~ "Wave 1", 
           wave == 2 ~ "Wave 2"), 
         u_mhisp = case_when( 
           u_mhisp == 0 ~ "Non-Hispanic", 
           u_mhisp == 1 ~ "Hispanic")) %>% 
  ungroup() %>% 
  relocate(wave) %>% 
  print(n = Inf)

#summary of sampling frame
counts_frame = full.frame  %>% 
  group_by(wave, u_mhisp, age.cat) %>% 
  summarize(population.count = n()) %>% 
  mutate(population.proportion = (population.count/nrow(full.frame))*100, 
         wave = case_when( 
           wave == 1 ~ "Wave 1", 
           wave == 2 ~ "Wave 2"),
         u_mhisp = case_when( 
           u_mhisp == 0 ~ "Non-Hispanic", 
           u_mhisp == 1 ~ "Hispanic")) %>% 
  print(n = Inf)

counts_table = counts_responses %>% 
  left_join(counts_frame, by = c("wave", "u_mhisp", "age.cat")) %>% 
  select(wave, u_mhisp, age.cat, sample.count, population.count, 
         sample.proportion, population.proportion, weight)

write.csv(counts_table, "Weight by group PRATS 2024, 10.28.2026.csv", row.names = F)
