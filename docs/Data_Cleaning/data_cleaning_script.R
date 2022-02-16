# Load libraries
library(tidyverse)
library(lme4)
library(broom)

# Read data
data_perm <- data.frame()
for(i in 1:11){
  csv_file <- paste("page", i, ".csv", sep = "")
  data_temp <- read.csv(csv_file)
  data_perm <- rbind(data_temp, data_perm)
}

#check number of participants
nrow(data_perm)/32
head(data_perm)
names(data_perm)

# Data cleaning

#select required columns
symbol_rt <- data_perm %>% select(rt, condition, match, stimulus, headphone = hp_test)
symbol_rt$subj <- rep(1:102, each = 32) #assign participant id

head(symbol_rt)
symbol_rt <- symbol_rt %>% mutate(headphone = ifelse(headphone == 1, "fail", "pass"))
head(symbol_rt)

#write.csv(symbol_rt, "sound_symbol.csv")

#check summary
symbol_rt %>% group_by(match) %>% summarise(mean_rt = mean(rt), sd_rt = sd(rt))
quantile(symbol_rt$rt, seq(0, 1, 0.10))
#filter rt
symbol_rt <- symbol_rt %>% filter(rt > 350) %>% filter(rt < 6024)

#scale
symbol_rt = symbol_rt %>% group_by(subj) %>% 
  mutate(z_latency = as.vector(scale(rt)))
head(symbol_rt)

#check summary
symbol_rt %>% group_by(match) %>% summarise(mean_rt = mean(rt), sd_rt = sd(rt))

#add log latency
symbol_rt$loglat <- log(symbol_rt$rt)
symbol_rt %>% group_by(match) %>% summarise(mean_rt = mean(rt), sd_rt = sd(rt))
 
#write intermediate data to file - not necessary
write.csv(symbol_rt, "symbol.csv")
plot(density(symbol_rt$rt))
nrow(symbol_rt)

#create wide format data
symbol_ind = symbol_rt %>% group_by(subj, match) %>% summarise(mz_lat = mean(z_latency))
symbol_wide = spread(symbol_ind, key = match, value = mz_lat)
symbol_wide = na.omit(symbol_wide)
write.csv(symbol_wide, "symbol_wide.csv") #convert to wide format

#check summary
symbol_rt %>% group_by(subj) %>% filter(match == 'incongruent') %>% summarise(count = n()) 
symbol_rt %>% group_by(subj) %>% filter(match == 'congruent') %>% summarise(count = n()) 

#final data - reaction time averages across participants
symbol_irt = symbol_rt %>% group_by(subj, match) %>% summarise(mlat = mean(rt))
symbol_wrt = spread(symbol_irt, key = match, value = mlat)
symbol_wrt = na.omit(symbol_wrt)
write.csv(symbol_wrt, "data_reaction_times.csv")

#final data - count averages among participants
symbol_count = symbol_rt %>% group_by(subj, match) %>% count()
symbol_count = spread(symbol_count, key = match, value = n)
symbol_count = na.omit(symbol_count)
write.csv(symbol_count, "data_counts.csv")

# Plots
ggplot(symbol_rt, aes(match, z_latency, color = match)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, alpha = 0.8) +
  theme_minimal()
ggsave("bar_plot.png")






