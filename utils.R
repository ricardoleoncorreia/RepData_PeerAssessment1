library(dplyr)
library(ggplot2)
library(chron)


csv.filename <- "activity.csv"


unzip_data <- function() {
    if (!file.exists(csv.filename)) unzip("activity.zip", exdir = "./")
}


load_data <- function() {
    if (!file.exists(csv.filename)) unzip_data()
    read.csv(csv.filename, stringsAsFactors = FALSE)
}


get_total_steps_per_day <- function(data) {
    data %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE))
}


get_mean_per_interval <- function(data) {
    data %>%
        group_by(interval) %>%
        summarize(mean_value = mean(steps, na.rm = TRUE))
}


impute_data_with_median <- function(data) {
    data %>%
        group_by(date) %>%
        mutate(median_value = median(steps, na.rm = TRUE)) %>%
        mutate(median_value = ifelse(is.na(median_value), 0, median_value)) %>%
        ungroup() %>%
        mutate(steps = ifelse(is.na(steps), median_value, steps)) %>%
        select(-c(median_value))
}


mean_by_day_type <- function(data) {
    data %>%
        mutate(type = is.weekend(date)) %>%
        mutate(type = as.factor(ifelse(type, "weekend", "weekday"))) %>%
        group_by(interval, type) %>%
        summarize(mean_value = mean(steps, na.rm = TRUE))
}
