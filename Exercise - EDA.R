# Exercise: EDA from Doing Data Science by Cathy O'Neil and Rachel Schutt
# Author: Noah Landesberg

# load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# load data
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

##########################################################################
# 1. Create a new variable, age_group, that categorizes users as "<18",  #
#   "18-24", "25-34", "35-44", "45-54", "55-64", "65+"                   #
##########################################################################
data1$age_group = cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
                      labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"))

data1$Gender = factor(data1$Gender, labels = c("Female", "Male"))
data1$Signed_In = factor(data1$Signed_In, levels = c(0, 1), labels = c("Y", "N"))

#########################
# 2. For a single day:  #
#########################
  #######################################################################################################
  # Plot the distributions of number of impressions and click-through-rate for these six age categories #
  #######################################################################################################

  # impressions plot
  ggplot(data1, aes(Impressions)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~ age_group)

  # CTR distribution (all)
  data1 %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    ggplot(aes(CTR, fill = age_group)) +
    geom_density(binwidth = 0.01)

  # CTR (grouped by age group)
  data1 %>%
    group_by(age_group) %>%
    summarize(CTR = sum(Clicks) / sum(Impressions)) %>%
    ggplot(aes(age_group, CTR, fill = age_group)) +
      geom_bar(stat = 'identity')

  ######################################################################################
  # Define a new variable to segment or categorize users based on their click behavior #
  ######################################################################################

  # creating variable "engaged" for users with CTR > 2%; removing users w/ clicks and no impressions
  data1 <- data1 %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    mutate(engaged = if_else(CTR > 0.02, "Y", "N")) %>%
    filter(is.na(CTR) == FALSE)

  ###################################################################################################
  # Explore the data and make visual and quantitative comparisons across user segments/demographics #
  ###################################################################################################

  # plot "engaged" users by sex
  by_gender <- data1 %>%
    group_by(Gender, engaged) %>%
    summarize(cnt_engaged = n()) %>%
    mutate(pcnt_engaged = cnt_engaged / sum(cnt_engaged)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(Gender, pcnt_engaged)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "") +
    scale_y_continuous(limits = c(0, 0.2))

  # plot "engaged" users by signed in status
  by_signedin <- data1 %>%
    group_by(Signed_In, engaged) %>%
    summarize(cnt_signedin = n()) %>%
    mutate(pcnt_signedin = cnt_signedin / sum(cnt_signedin)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(Signed_In, pcnt_signedin)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "") +
    scale_y_continuous(limits = c(0, 0.2))

  # plot "engaged" users by age group
  by_agegroup <- data1 %>%
    group_by(age_group, engaged) %>%
    summarize(cnt_agegroup = n()) %>%
    mutate(pcnt_agegroup = cnt_agegroup / sum(cnt_agegroup)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(age_group, pcnt_agegroup)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "") +
    scale_y_continuous(limits = c(0, 0.2))

  grid.arrange(by_gender, by_signedin, by_agegroup, ncol = 3)

  ##################################################################
  # Create metrics/measurements/statistics that summarize the data #
  ##################################################################
  summary(data1)


##########################################################################################
# 3. Extend the analysis across days. Visualize some metrics and distributions over time #
##########################################################################################

###################################################
# 4. Describe and interpret any patterns you find #
###################################################