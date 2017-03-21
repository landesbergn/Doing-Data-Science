# Exercise: EDA from Doing Data Science by Cathy O'Neil and Rachel Schutt
# Author: Noah Landesberg

# load libraries
library(dplyr)
library(ggplot2)

# load data
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

# Create a new variable, age_group, that categorizes users as "<18",
# "18-24", "25-34", "35-44", "45-54", "55-64", "65+"

data1$age_group = cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
                      labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
# For a single day:
  # Plot the distributions of number of impressions and click-through-rate for these six age categories

    # impressions plot
    ggplot(data1, aes(Impressions)) +
      geom_histogram(binwidth = 1) +
      facet_wrap(~ age_group)

    # CTR overall plot
    data1 %>%
      group_by(age_group) %>%
      summarize(CTR = sum(Clicks) / sum(Impressions)) %>%
      ggplot(aes(age_group, CTR, fill = age_group)) +
        geom_bar(stat = 'identity')
    data1 <- ungroup(data1)

  # Define a new variable to segment or categorize users based on their click behavior
  data1 %>%
    mutate(CTR = round(Clicks/Impressions, 2)) %>%
    mutate(engaged = if_else(CTR > 0.02, "Y", "N")) %>%
    group_by(Gender, engaged) %>%
    summarize(count = n()) %>%
    ggplot(aes(factor(Gender, labels = c("Female", "Male")), count, fill = engaged)) +
      geom_bar(stat = 'identity')

  # Explore the data and make visual and quantitative comparisons across user segments/demographics

  # Create metrics/measurements/statistics that summarize the data

# Extend the analysis across days. Visualize some metrics and distributions over time

# Describe and interpret any patterns you find
