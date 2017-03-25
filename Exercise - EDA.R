# Exercise: EDA from Doing Data Science by Cathy O'Neil and Rachel Schutt
# Author: Noah Landesberg

# Load libraries ----------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Function to get data for the requested number of days. Returns data frame.
getData <- function(day, type = ""){

  url_stub <- "http://stat.columbia.edu/~rachel/datasets/nyt"

  if(type != "combined" | day == 1) {
    initial_data <- read.csv(url(paste0(url_stub, day, ".csv"))) %>% mutate(day = as.numeric(day))
    return(initial_data)
  } else if (day > 1 & day <= 31) {
    initial_data <- read.csv(url(paste0(url_stub, "1.csv"))) %>% mutate(day = 1)

    for(i in 2:day) {
      url_full <- paste0(url_stub, i, ".csv")
      new_data <- assign(paste0("data", i), read.csv(url(url_full)))
      new_data <- mutate(new_data, day = as.numeric(i))
      initial_data <- union(initial_data, new_data)
    }

    return(initial_data)
  } else {
    return(paste("Bad input"))
  }
}

# Load data ----------------
df <- getData(1, type = "combined")

##########################################################################
# 1. Create a new variable, age_group, that categorizes users as "<18",  #
#   "18-24", "25-34", "35-44", "45-54", "55-64", "65+"                   #
##########################################################################
df$age_group = cut(df$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
                      labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
df$Gender = factor(df$Gender, labels = c("Female", "Male"))
df$Signed_In = factor(df$Signed_In, levels = c(0, 1), labels = c("Y", "N"))

#########################
# 2. For a single day:  #
#########################
  #######################################################################################################
  # Plot the distributions of number of impressions and click-through-rate for these six age categories #
  #######################################################################################################

  # Impressions plots.
  ggplot(df, aes(Impressions, colour = age_group)) +
    geom_freqpoly(binwidth = 1)

  ggplot(df, aes(Impressions, colour = Signed_In)) +
    geom_freqpoly(binwidth = 1)

  ggplot(df, aes(Impressions, colour = age_group)) +
    geom_freqpoly(binwidth = 1) +
    facet_wrap(~ Clicks)

  # CTR (by age group).
  df %>%
    group_by(age_group) %>%
    summarize(CTR = sum(Clicks) / sum(Impressions)) %>%
    filter(is.na(CTR) == FALSE) %>%
    ggplot(aes(age_group, CTR, fill = age_group)) +
      geom_bar(stat = 'identity') +
      scale_y_continuous(labels = percent)

  # CTR (by gender).
  df %>%
    group_by(Gender) %>%
    summarize(CTR = sum(Clicks) / sum(Impressions)) %>%
    filter(is.na(CTR) == FALSE) %>%
    ggplot(aes(Gender, CTR, fill = Gender)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = percent)

  # CTR (by signed in)
  df %>%
    group_by(Signed_In) %>%
    summarize(CTR = sum(Clicks) / sum(Impressions)) %>%
    filter(is.na(CTR) == FALSE) %>%
    ggplot(aes(Signed_In, CTR, fill = Signed_In)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = percent)

  # Most common CTR values
  df %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    filter(is.na(CTR) == FALSE) %>%
    ggplot(aes(CTR, fill = age_group)) +
    geom_histogram(binwidth = 0.01) +
    scale_x_continuous(labels = percent)

  ######################################################################################
  # Define a new variable to segment or categorize users based on their click behavior #
  ######################################################################################

  # creating variable "engaged" for users with CTR > 2%; removing users w/ clicks and no impressions
  df <- df %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    mutate(engaged = if_else(CTR > 0.02, "Y", "N")) %>%
    filter(is.na(CTR) == FALSE)

  ###################################################################################################
  # Explore the data and make visual and quantitative comparisons across user segments/demographics #
  ###################################################################################################

  # plot "engaged" users by sex
  by_gender <- df %>%
    group_by(Gender, engaged) %>%
    summarize(cnt_engaged = n()) %>%
    mutate(pcnt_engaged = cnt_engaged / sum(cnt_engaged)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(Gender, pcnt_engaged)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "")

  # plot "engaged" users by signed in status
  by_signedin <- df %>%
    group_by(Signed_In, engaged) %>%
    summarize(cnt_signedin = n()) %>%
    mutate(pcnt_signedin = cnt_signedin / sum(cnt_signedin)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(Signed_In, pcnt_signedin)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "")

  # plot "engaged" users by age group
  by_agegroup <- df %>%
    group_by(age_group, engaged) %>%
    summarize(cnt_agegroup = n()) %>%
    mutate(pcnt_agegroup = cnt_agegroup / sum(cnt_agegroup)) %>%
    filter(engaged == "Y") %>%
    ggplot(aes(age_group, pcnt_agegroup)) +
    geom_bar(stat = 'identity') +
    theme(legend.position = "")

  grid.arrange(by_gender, by_signedin, by_agegroup, ncol = 3)

  ##################################################################
  # Create metrics/measurements/statistics that summarize the data #
  ##################################################################

  # Overall summary.
  summary(df)

  # Gender breakdowns.
  pcnt_female <- (group_by(df, Gender) %>% summarise(count = n()) %>% mutate(pcnt = count/ sum(count)) %>% filter(Gender == "Female"))$pcnt
  pcnt_male <- (group_by(df, Gender) %>% summarise(count = n()) %>% mutate(pcnt = count/ sum(count)) %>% filter(Gender == "Male"))$pcnt
  paste0("Percent Female: ", round(100*pcnt_female, 2), '%')
  paste0("Percent Male: ", round(100*pcnt_male, 2), '%')

  # Signed In breakdowns.
  pcnt_signed_in <- (group_by(df, Signed_In) %>% summarise(count = n()) %>% mutate(pcnt = count/ sum(count)) %>% filter(Signed_In == "Y"))$pcnt
  pcnt_signed_out <- (group_by(df, Signed_In) %>% summarise(count = n()) %>% mutate(pcnt = count/ sum(count)) %>% filter(Signed_In == "N"))$pcnt
  paste0("Percent Signed In: ", round(100*pcnt_signed_in, 2), '%')
  paste0("Percent Signed Out: ", round(100*pcnt_signed_out, 2), '%')

##########################################################################################
# 3. Extend the analysis across days. Visualize some metrics and distributions over time #
##########################################################################################

  df_all <- getData(31, type = "combined")

  df_all$age_group = cut(df_all$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
                     labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  df_all$Gender = factor(df_all$Gender, labels = c("Female", "Male"))
  df_all$Signed_In = factor(df_all$Signed_In, levels = c(0, 1), labels = c("Y", "N"))
  df_all <- df_all %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    mutate(engaged = if_else(CTR > 0.02, "Y", "N")) %>%
    filter(is.na(CTR) == FALSE)

  # Plot clicks and impressions over time
  df_all %>%
    group_by(day) %>% summarise(total_impressions = sum(Impressions), total_clicks = sum(Clicks)) %>%
    ggplot(aes(x = day)) +
      geom_line(aes(y = total_clicks, color = "Clicks")) +
      geom_line(aes(y = total_impressions, color = "Impressions"))

  # Plot avg CTR by gender over time
  df_all %>%
    group_by(day, Gender) %>% summarize(avg_CTR = mean(CTR)) %>%
    ggplot(aes(x = day, y = avg_CTR, color = Gender)) +
      geom_line() +
      geom_vline(xintercept = c(6, 13, 20, 27), color = "grey")


  # Plot avg CTR by signed in stauts over time
  df_all %>%
    group_by(day, Signed_In) %>% summarize(avg_CTR = mean(CTR)) %>%
    ggplot(aes(x = day, y = avg_CTR, color = Signed_In)) +
    geom_line() +
    geom_vline(xintercept = c(6, 13, 20, 27), color = "grey")


  # Plot avg CTR by age group over time
  df_all %>%
    group_by(day, age_group) %>% summarize(avg_CTR = mean(CTR)) %>%
    ggplot(aes(x = day, y = avg_CTR, color = age_group)) +
      geom_line() +
      geom_vline(xintercept = c(6, 13, 20, 27), color = "grey")

###################################################
# 4. Describe and interpret any patterns you find #
###################################################

# Some general paterns:
#   - The young (< 18) and the elderly (> 55) are more engaged
#   - Engagment has a weekly trend, spiking on the 6th day of the month and then every week later.
#   - Signed in users are more like to engage with content
#   - Women are more likelt to engage with content