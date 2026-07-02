
# Install Packages


install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("lubridate")
install.packages("GGally")
install.packages("corrplot")
install.packages("plotly")
install.packages("psych")
install.packages("moments")
install.packages("car")


# Load Required Libraries


library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(GGally)
library(corrplot)
library(plotly)
library(psych)
library(moments)
library(car)


# Import Dataset


air_quality <- read.csv("C:/Users/deora/OneDrive/Desktop/R Project/global_air_quality_data_10000.csv")


# Clean Column Names


air_quality <- clean_names(air_quality)


# Display Dataset


head(air_quality)

tail(air_quality)


# Dataset Structure


str(air_quality)

dim(air_quality)

colnames(air_quality)

sapply(air_quality, class)


# Summary Statistics


summary(air_quality)


# Missing Values


colSums(is.na(air_quality))


# Duplicate Values


sum(duplicated(air_quality))


# Remove Duplicate Records


air_quality <- distinct(air_quality)


# Number of Rows & Columns


cat("Number of Rows :", nrow(air_quality), "\n")

cat("Number of Columns :", ncol(air_quality), "\n")


# Unique Cities & Countries


length(unique(air_quality$city))

length(unique(air_quality$country))

# Select Numeric Variables


numeric_data <- air_quality[, c("pm2_5",
                                "pm10",
                                "no2",
                                "so2",
                                "co",
                                "o3",
                                "temperature",
                                "humidity",
                                "wind_speed")]



cat("Summary Statistics\n")
summary(numeric_data)


# Mean


cat("\nMean Values\n")
sapply(numeric_data, mean)


# Median

cat("\nMedian Values\n")
sapply(numeric_data, median)


# Standard Deviation


cat("\nStandard Deviation\n")
sapply(numeric_data, sd)


# Variance


cat("\nVariance\n")
sapply(numeric_data, var)


# Minimum Values


cat("\nMinimum Values\n")
sapply(numeric_data, min)


# Maximum Values


cat("\nMaximum Values\n")
sapply(numeric_data, max)


# Correlation Matrix

cat("\nCorrelation Matrix\n")
correlation_matrix <- cor(numeric_data)
correlation_matrix


# BAR CHART - Top 10 Cities


library(ggplot2)
library(dplyr)

top_cities <- air_quality %>%
  count(city, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top_cities, aes(x = reorder(city, n), y = n)) +
  geom_col(fill = "#2E86DE") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0))
  labs(
    title = "Top 10 Cities by Number of Air Quality Records",
    x = "City",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


ggsave("Top_10_Cities_BarChart.png", width = 8, height = 5, dpi = 300)



# PIE CHART - Country Distribution

country_data <- air_quality %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(country_data,
       aes(x = "", y = n, fill = country)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(
    title = "Distribution of Air Quality Records by Country"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5,
                              size = 16)
  )


ggsave("Country_PieChart.png", width = 7, height = 7, dpi = 300)


# HISTOGRAM 

ggplot(air_quality,
       aes(x = pm2_5)) +
  geom_histogram(
    bins = 30,
    fill = "#27AE60",
    color = "black"
  ) +
  labs(
    title = "Distribution of PM2.5 Levels",
    x = "PM2.5",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )


ggsave("PM25_Histogram.png", width = 8, height = 5, dpi = 300)



# BOX PLOT 

ggplot(air_quality, aes(y = pm2_5)) +
  geom_boxplot(fill = "#F39C12",
               color = "black",
               outlier.color = "red",
               outlier.shape = 16) +
  labs(
    title = "Box Plot of PM2.5 Levels",
    x = "",
    y = "PM2.5"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


# SCATTER PLOT

ggplot(air_quality,
       aes(x = pm10,
           y = pm2_5)) +
  geom_point(
    color = "#2E86DE",
    alpha = 0.3,
    size = 1.2
  ) +
  labs(
    title = "Relationship Between PM10 and PM2.5",
    x = "PM10",
    y = "PM2.5"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )



#  LINE CHART

country_avg <- air_quality %>%
  group_by(country) %>%
  summarise(avg_pm25 = mean(pm2_5, na.rm = TRUE))

ggplot(country_avg,
       aes(x = reorder(country, avg_pm25),
           y = avg_pm25)) +
  geom_col(fill = "#8E44AD") +
  coord_flip() +
  labs(
    title = "Average PM2.5 Levels by Country",
    x = "Country",
    y = "Average PM2.5"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



# CORRELATION HEATMAP

cor_matrix <- cor(numeric_data)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.9,
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.8
)



# REGRESSION PLOT

ggplot(air_quality,
       aes(x = pm10,
           y = pm2_5)) +
  geom_point(
    color = "#34495E",
    alpha = 0.35,
    size = 1.2
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    linewidth = 1,
    se = TRUE
  ) +
  labs(
    title = "Linear Regression Between PM10 and PM2.5",
    x = "PM10",
    y = "PM2.5"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    axis.title = element_text(face = "bold")
  )




# Simple Linear Regression


simple_model <- lm(pm2_5 ~ pm10, data = air_quality)

summary(simple_model)


# Multiple Linear Regression


multiple_model <- lm(
  pm2_5 ~ pm10 +
    no2 +
    so2 +
    co +
    o3 +
    temperature +
    humidity +
    wind_speed,
  data = air_quality
)

summary(multiple_model)



# Model Comparison


AIC(simple_model)

AIC(multiple_model)



# Diagnostic Plots


par(mfrow = c(2,2))

plot(multiple_model)

par(mfrow = c(1,1))