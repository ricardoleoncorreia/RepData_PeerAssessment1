library(dplyr)
library(ggplot2)
library(chron)

csv.filename <- "activity.csv"
unzip_data <- function() {
    if (!file.exists(csv.filename)) unzip("activity.zip", exdir = "./")
}
load_data <- function() {
    if (!file.exists(csv.filename)) unzip_data()
    read.csv(csv.file, stringsAsFactors = FALSE)
}
data <- load_data()
head(data)


get_total_steps_per_day <- function(data) {
    data %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE))
}
data_q1 <- get_total_steps_per_day(data)
head(data_q1)
ggplot(data_q1, aes(x = total_steps)) +
    geom_histogram(bins = 50) +
    ggtitle("Frequency of total steps") +
    xlab("Total Steps") +
    ylab("Frequency")
mean(data_q1$total_steps, na.rm = TRUE)
median(data_q1$total_steps, na.rm = TRUE)


data_q2 <- data %>%
    group_by(interval) %>%
    summarize(mean_value = mean(steps, na.rm = TRUE))
ggplot(data_q2, aes(x=interval, y=mean_value)) +
    geom_line() +
    ggtitle("Average number of steps by interval") +
    xlab("Interval") +
    ylab("Number of steps")
(data_q2 %>% filter(mean_value == max(mean_value)))$interval


data_no_na <- data %>%
                group_by(date) %>%
                mutate(median_value = median(steps, na.rm = TRUE)) %>%
                mutate(median_value = ifelse(is.na(median_value), 0, median_value)) %>%
                ungroup() %>%
                mutate(steps = ifelse(is.na(steps), median_value, steps)) %>%
                select(-c(median_value))
data_q2 <- get_total_steps_per_day(data_no_na)
head(data_q2)
ggplot(data_q2, aes(x = total_steps)) +
        geom_histogram(bins = 50) +
        ggtitle("Frequency of total steps") +
        xlab("Total Steps") +
        ylab("Frequency")
mean(data_q2$total_steps, na.rm = TRUE)
median(data_q2$total_steps, na.rm = TRUE)


data_with_type <- data_no_na %>%
                    mutate(type = is.weekend(date)) %>%
                    mutate(type = as.factor(ifelse(type, "weekend", "weekday"))) %>%
                    group_by(interval, type) %>%
                    summarize(mean_value = mean(steps, na.rm = TRUE))
ggplot(data_with_type, aes(x=interval, y=mean_value)) +
        geom_line() +
        facet_grid(type ~ .) +
        ggtitle("Plot steps-interval by day type") +
        xlab("Interval") +
        ylab("Number of steps")