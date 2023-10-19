# Data Science Specialization - Johns Hopkins University - Coursera
# Reproducible Research - Peer-graded Assignment: Course Project 1
# Name: Isaac G Veras
# Date: 05 de outubro de 2023

R.version.string   # R 4.3.1
getwd(); cat("\n") # Current working directory
setwd("C:/Johns Hopkins - Data Science/Reproducible_Research/RR_Poject1")

# Package installation: ----------------------------------------------------------------------
	if (!require("pacman")) install.packages("pacman")
	pacman::p_load(pacman,     # Package Manager
				   knitr,      # Transform R Markdown documents into various output formats such as HTML, PDF, Word and others.
				   plyr,       # Data manipulation
				   data.table, # Manipulate, process and analyze large data sets
				   tidyverse   # Data organization
	)

# Loading and preprocessing the data ----------------------------------------------------------
	opts_chunk$set(fig_path = "./figure/")
	activity_monitoring_data <- read.csv("activity.csv",
										 header     = TRUE,
										 na.strings = "NA"
	)
	head(activity_monitoring_data)

## What is mean total number of steps taken per day? -------------------------------------------
	opts_chunk$set(fig_path = "./figure/")
	steps_per_day <- tapply(activity_monitoring_data$steps,
							activity_monitoring_data$date,
							sum,
							na.rm = TRUE
	)

# What is the average daily activity pattern? ---------------------------------------------------
mean_step_per_day <- mean(steps_per_day)
mean_step_per_day

	# Histogram ----------------------------
	opts_chunk$set(fig_path = "./figure/")
	qplot(steps_per_day,
		  xlab     = "Total Steps per day",
		  ylab     = "Frecuency",
		  binwidth = 500
	)

## Median Steps per day --------------------------------------------------------------------------
median_step_per_day <- median(steps_per_day)
median_step_per_day

	# Time Series --------------------------
	opts_chunk$set(fig_path = "./figure/")
	average_day_act_patt <- aggregate(x     = list(meanSteps = activity_monitoring_data$steps),
									  by    = list(interval  = activity_monitoring_data$interval),
									  FUN   = mean,
									  na.rm = TRUE
	)

	ggplot(data = average_day_act_patt, aes(x = interval,
											y = meanSteps)) +
		geom_line() +
		ggtitle("Average Number of Steps Per Day") +
		xlab("5-minute interval") +
		ylab("Average Number of steps")

## Calculation of 5-Minutes Interval --------------------------------------------------------------
opts_chunk$set(fig_path = "./figure/")
max_steps     <- which.max(average_day_act_patt$meanSteps)
most_of_steps <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", average_day_act_patt[max_steps, "interval"])
most_of_steps

# Imputing missing values --------------------------------------------------------------------------
missing_values <- length(which(is.na(activity_monitoring_data$steps)))
missing_values

## Make an Histogram of the number of total steps taken by day
opts_chunk$set(fig_path = "./figure/")
activity    <- data.table::fread(input = "activity.csv")
total_steps <- activity[, lapply(.SD, sum), .SDcols = "steps", by = .(date)]
total_steps[, .(MeanSteps   = mean(steps),
				MedianSteps = median(steps))]

	ggplot(total_steps, aes(x = steps)) +
			geom_histogram(fill     = "blue",
						   binwidth = 1000) +
			labs(title = "Daily Steps",
				 x     = "Steps",
				 y     = "Frequency"
			)
	print(ggplot)

# Are there differences in activity patterns between weekdays and weekends? ------------------------
opts_chunk$set(fig_path = "./figure/")
activity_monitoring_data      <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
activity_monitoring_data$date <- as.POSIXct(activity_monitoring_data$date)
data_fix                      <- activity_monitoring_data

for (i in unique(data_fix$interval)) {
	data_fix$steps[is.na(data_fix$steps) & data_fix$interval == i] <-
		round(mean(data_fix$steps[activity_monitoring_data$interval == i], na.rm = TRUE))
}

	data_fix$weekDay <- as.POSIXlt(activity_monitoring_data$date)$wday == 0 | as.POSIXlt(activity_monitoring_data$date)$wday == 6
	data_fix$weekDay <- factor(data_fix$weekDay,
							   levels = c(F, T),
							   labels = c("weekday", "weekend")
	)

steps_week_day <- tapply(data_fix$steps   [data_fix$weekDay == "weekday"],
						 data_fix$interval[data_fix$weekDay == "weekday"], mean)
steps_week_end <- tapply(data_fix$steps   [data_fix$weekDay == "weekend"],
						 data_fix$interval[data_fix$weekDay == "weekend"], mean)

	par(mfrow = c(2, 1))
	plot(steps_week_day,
		 type = "l",
		 main = "weekdays",
		 xlab = "the 5-minute interval",
		 ylab = "the average steps"
	)
	plot(steps_week_end,
		 type = "l",
		 main = "weekends",
		 xlab = "the 5-minute interval",
		 ylab = "the average steps"
	)