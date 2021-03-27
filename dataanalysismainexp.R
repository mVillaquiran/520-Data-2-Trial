# Load libraries - remember both tidyverse and BayesFactor will be needed.

library(tidyverse)
library(BayesFactor)

# Read data - you may have more than one data file. 
# Remember to read them all, and that they will need to be stored in different data frames.

positivedata <- read_csv("Main Experiment (Positive) Final.csv")
negativedata <- read_csv("Main Experiment (Negative) Final.csv")

# Combine data files - hint: the command that does this is bind_rows

alldata <- bind_rows(positivedata, negativedata)

# Filter data - work out what columns you need to keep from your raw data files first. 
# Remember to check what you named a keyboard response object in your opensesame experiment. 
# This name will be in the columns you need. Let's say you didn't rename the keyboard object as
# left it at the default of new_keyboard_response. correct_new_keyboard_response is the column 
# that indicates whether the participant was correct on each trial. response_time_new_keyboard_response
# is the reaction time to make a response. response_new_keyboard_response logs the key that was pressed
# on each trial. Do not forget that you'll need to also keep columns relevant to your study - 
# the columns that code you within and between subjects factors for example. 
# Then filter by the rows - which trials do you need to keep and which can be removed 
# (e.g. remove practice or study trials, and keep the test trials.)

alldata_subset  <- alldata %>% select(subject_nr, probe, response_probe_response, condition, response_age_first_response, response_age_second_response, response_gender_response)
tidydata <- alldata_subset %>% 
  set_names(c("subj", "probe", "proberesponse", "priming", "agea", "ageb", "gender"))
datafiltered <- tidydata %>% filter(probe != "0")

# Summarise data. Here you'll need to group the data and use the summarise command to
# get the means/medians/counts that form your dependent variable. Before running the ANOVA,
# don't forget we need to specify which columns are factors. For our purposes, everything other 
# than the DV is a factor.ctrl <- wordctrlCIsum %>% pivot_wider(names_from = congru, values_from = rt)

probe.response <- c("n" = "none", "o" = "future", "w" = "past", "x" = "unsure")
mappeddata <-  datafiltered %>% mutate(mw_direction = recode(proberesponse, !!!probe.response))
conditiontype <- c("p" = "Positive", "n" = "Negative")
mappeddataa <-  mappeddata %>% mutate(priming_condition = recode(priming, !!!conditiontype))
datafinal <- mappeddataa %>% group_by(subj, mw_direction, priming_condition, agea, ageb, gender) %>% count(mw_direction)
datafinal %>% rename(occurances = `n`)
finaldata <- datafinal %>% rename(occurances = `n`)
testdata <- finaldata %>% pivot_wider(names_from = mw_direction, values_from = occurances)
testdata[is.na(testdata)] <- 0

testdata2 <- testdata %>% pivot_longer(c(past, future, unsure, none), names_to = "mw_direction", values_to = "occurances")


finaldatafltrd <- testdata2 %>% filter(mw_direction != "unsure")
finaldatafltrdtwo <- finaldatafltrd %>% filter(mw_direction != "none")
prpfrq <- finaldatafltrdtwo %>% mutate(sum=occurances/12)
draftdata <- prpfrq  %>% mutate(age=as.numeric(paste0(agea, ageb)))

draftnew$subj <- factor(draftnew$subj)
draftnew$mw_direction <- factor(draftnew$mw_direction)
draftnew$priming_condition <- factor(draftnew$priming_condition)
draftnew$age <- factor(draftnew$age)
draftnew$gender <- factor(draftnew$gender)
bf <- anovaBF(formula = sum ~ mw_direction * priming_condition + subj, data = data_frame(draftnew), whichRandom = "subj")
bf
bf[4]/bf[3]

# Make graph. The understanding interactions worksheet will give you guidance here.
# Remember to first create a data frame with the means for both levels in both conditions
# (thus 4 means should be present). Use this data frame to create a graph of your results using ggplot.

plotdata <- draftnew %>% group_by(mw_direction, priming_condition) %>% summarise(sum = mean(sum)) 
graph <- plotdata %>% ggplot(aes(x = mw_direction, y = sum, group = priming_condition)) + 
  geom_line(aes(colour = priming_condition)) + theme_bw() + ylab("Mind Wandering Occurances") + 
  xlab("Mind Wandering Direction")
# draftnew %>% group_by(priming_condition) %>% count(mw_direction)
graph <- plotdata %>% ggplot(aes(x = mw_direction, y = sum, group = priming_condition)) + 
  geom_line(aes(colour = priming_condition)) + theme_bw() + ylab("Mind Wandering Occurances") + 
  xlab("Mind Wandering Direction")
graph
# draftnew %>% group_by(priming_condition) %>% count(mw_direction)
draftdata %>%
  write_csv('sweets_recoded_withsummary.csv')
