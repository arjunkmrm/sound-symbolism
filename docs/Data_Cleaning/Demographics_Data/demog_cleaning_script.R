library(tidyverse)
demog_ss <- read.csv("demog_ss.csv")
head(demog_ss)

age_ss <- data.frame(age = demog_ss$Q2, id = demog_ss$Q8, gender = demog_ss$Q1, headphone = demog_ss$Q7, ethnicity = demog_ss$Q3)
nrow(age_ss)
age_ss <- age_ss %>% slice(3 : nrow(age_ss))
head(age_ss)
age_ss <- age_ss %>% distinct(.keep_all = TRUE)
nrow(age_ss)

age_ss$age <- as.numeric(age_ss$age)
mean(age_ss$age)
age_ss <- age_ss %>% filter(!age %in% c(37))
median(age_ss$age)
sd(age_ss$age)
max(age_ss$age)
min(age_ss$age)

head(age_ss)
nrow(age_ss)

write.csv(age_ss, "demog_ss.csv")

plot(density(age_ss$age))
nrow(age_ss)
