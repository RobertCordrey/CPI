
rm(list = ls(all.names = TRUE))

library(dplyr)
library(lubridate)
library(ggplot2)
library(fredr)
library(scales)

# Get the current month and year
month <- format(Sys.Date(), " |> %B")
year <- format(Sys.Date(), "%Y")
todaysDate <- Sys.Date()
# todaysDate <- format(Sys.Date(), "%d-%b-%Y")

# Set FRED API key
fredr_set_key('a20f11e75b97d7c2b7120f0f9789ea30')

# Retrieve CPI data from FRED
medianCPI <- fredr(
    series_id = 'MEDCPIM158SFRBCLE',
    observation_start = as.Date('1971-01-01'),
    observation_end = as.Date(Sys.Date())
)

min_date <- format(min(medianCPI$date), '%B %Y')

medianCPI1 <- medianCPI |>
    na.omit() |>
    select(1, 3) %>%
    rename(Date = 1, Value = 2)

# class(medianCPI1$Date)

yearlymedianCPI <- medianCPI1 %>%
    group_by(year = year(Date)) %>%
    summarize(medianCPI = round(median(Value), 2))

# Define a custom label function to add a % sign
percent_label <- function(x) {
    paste0(x, "%")
}

yearlyMedPrice

class(todaysDate)

# class(yearlymedianSales1$year)

ggplot(medianCPI1, aes(Date, Value)) +
    geom_line(linewidth = 0.5) +
    geom_point(color = 'Red', size = 1) +
    scale_x_date(breaks = seq(min(year(medianCPI1$Date)),
                              max(year(medianCPI1$Date)),
                              by = "5 years")) +
    scale_y_continuous(limits = c(min(medianCPI1$Value,
                                      na.rm = TRUE),
                                  max(medianCPI1$Value,
                                      na.rm = TRUE)),
                       labels = label_dollar()) +
    theme_minimal() +
    ggtitle(paste0("Monthly CPI from ",
                   min(medianCPI1$Date),
                   " to ",
                   max(medianCPI1$Date))) +
    labs(caption = paste0("Blue Hen Analytics - Data from FRED on ",
                          format(todaysDate, '%b %d, %Y'))) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.90),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.90, 0.25)) +
    xlab("") +
    ylab("Price ($)")


# show the monthly variation for each year
ggplot(medianPrice, aes(x = Date, y = Value)) +
    geom_boxplot(aes(group = year(Date)), outlier.color = "red", outlier.size = 1) +
    scale_x_date(breaks = seq(min(medianPrice$Date),
                              max(medianPrice$Date),
                              by = "2 years"),
                 labels = date_format("%Y")) +  # Format to show only the year
    scale_y_continuous(labels = label_dollar()) +
    theme_minimal() +
    ggtitle(paste0("Variation in US Median Home Sold Prices (",
                   min(year(medianPrice$Date)), " - ",
                   max(year(medianPrice$Date)), ")")) +
    labs(x = "Year",
         y = "Price ($)",
         caption = paste0("Blue Hen Analytics - Data from FRED on ",
                          format(todaysDate, '%b %d, %Y'))) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.90),
          axis.text.x = element_text(angle = 45, hjust = 1))






# for every month, not the median value
# Fetch the data from FRED
monthly_cpi_data <- fredr(series_id = "CPIAUCSL",
                  observation_start = as.Date("1990-01-01"),
                  frequency = "m") |>
    select(date, value) |>
    mutate(value = as.numeric(value),
           lagged_value = dplyr::lag(value),
           inflation_rate = ifelse(is.na(lagged_value), 0, (value / lagged_value - 1) * 100))

monthly_cpi_data

# Print the first few rows to check the result
# print(head(cpi_data, 20))

cpiDataFiltered <- monthly_cpi_data %>%
    filter(date >= '2015-01-01')

ggplot(cpiDataFiltered, aes(x = date, y = inflation_rate)) +
    geom_line() +
    labs(title = "Monthly Inflation Rate in the US",
         x = "Date",
         y = "Inflation Rate (%)")


minDate <- as.POSIXct(min(cpiDataFiltered$date), tz = "UTC")
min_date <- format(as.Date(minDate), "%b %Y")
maxDate <- as.POSIXct(max(cpiDataFiltered$date), tz = "UTC")
max_date <- format(as.Date(maxDate), "%b %Y")

ggplot(data = cpiDataFiltered,
       mapping = aes(x = date, y = value)) +
    geom_line(color = "blue", linewidth = 0.6) +
    theme_minimal() +
    ggtitle(paste0("US Consumer Price Index from ", min_date, ' to ', max_date)) +
    labs(x = " ",
         y = "Index 1982-1984=100",
         caption = paste0("Blue Hen Analytics - Data from FRED dated ",
                          format(todaysDate, '%b %d, %Y')),
         subtitle = paste0("The prices continue to rise as of "
                           , format(todaysDate,
                                    '%b %d, %Y'))) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Center and bold title
          plot.caption = element_text(hjust = 1, size = 8, face = "italic"), # Adjust caption style
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(size = 12), # Bold axis titles , face = "bold"
          axis.text = element_text(size = 10), # Adjust axis text size
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_line(color = "gray80"), # Major grid lines in lighter gray
          panel.grid.minor = element_line(color = "gray90", linewidth = 0.5)) + # Minor grid lines even lighter
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b %Y",
                 expand = expansion(0)) +
    scale_y_continuous(limits = c(min(cpiDataFiltered$value,
                                      na.rm = TRUE),
                                  max(cpiDataFiltered$value,
                                      na.rm = TRUE)))# ,
#    labels = percent)

# Creating a bar plot
ggplot(cpiDataFiltered, aes(x = date,
                        y = inflation_rate,
                        fill = inflation_rate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste0("Monthly CPI Change (%) from 2015 to ",
         format(max(cpiDataFiltered$date), '%b %d, %Y')),
         caption = paste0("Blue Hen Analytics - Data from FRED on ", format(todaysDate, '%b %d, %Y')),
         x = "",
         y = "Monthly Change (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    scale_x_date(date_breaks = "3 month",
                 date_labels = "%b %Y")

# Calculate the minimum and maximum dates to set x-axis limits
minDate <- min(cpiDataFiltered$date)
minDate
maxDate <- max(cpiDataFiltered$date)
maxDate


# Creating a bar plot
ggplot(cpiDataFiltered, aes(x = date,
                            y = inflation_rate,
                            fill = inflation_rate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Monthly CPI from 2015 Onwards",
         x = "Date",
         y = "Monthly CPI") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1)) + # Rotate x-axis labels for better visibility
    scale_x_date(limits = c(minDate,
                            maxDate),
                 date_breaks = "3 month",
                 date_labels = "%b %Y")

# str(cpiDataFiltered)

# Creating a bar plot with adjusted x-axis
ggplot(cpiDataFiltered, aes(x = date, y = value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Monthly CPI from 2015 Onwards",
         x = "Date",
         y = "Monthly CPI") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
    expand_limits(x = c(min(cpiDataFiltered$date), max(cpiDataFiltered$date))) # Explicitly set x-axis limits

# to make sure that the x-axis only shows dates within the data frame
# Define the sequence of dates for breaks
date_breaks <- seq(from = min(cpiDataFiltered$date),
                   to = max(cpiDataFiltered$date),
                   by = "3 months")

# Plotting the bar plot with strictly defined x-axis breaks
ggplot(cpiDataFiltered, aes(x = date,
                            y = value)) +
    geom_bar(stat = "identity",
             fill = "steelblue") +
    labs(title = paste0("Monthly CPI from ",
                        format(min(date_breaks),
                               "%Y"),
                        " Onwards"),
         x = "Date",
         y = "Monthly CPI") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    scale_x_date(breaks = date_breaks,
                 labels = scales::date_format("%b %Y")) +
    coord_cartesian(xlim = c(min(cpiDataFiltered$date),
                             max(cpiDataFiltered$date)))


# *****


# Fetch the data from FRED
monthly_cpi_data <- fredr(series_id = "CPIAUCSL",
                          observation_start = as.Date("1990-01-01"),
                          frequency = "m") %>%
    select(date, value) %>%
    mutate(value = as.numeric(value),
           lagged_value = dplyr::lag(value, 12),
           inflation_rate = ifelse(is.na(lagged_value), NA, (value / lagged_value - 1) * 100))

# Remove the rows with NA values in inflation_rate
monthly_cpi_data <- monthly_cpi_data %>%
    filter(!is.na(inflation_rate))

cpiDataFiltered <- monthly_cpi_data %>%
    filter(date >= '2015-01-01')

# Creating a bar plot
ggplot(cpiDataFiltered, aes(x = date,
                            y = inflation_rate,
                            fill = inflation_rate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Monthly CPI from 2015 Onwards",
         x = "Date",
         y = "Monthly CPI") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1)) +
    scale_x_date(limits = c(minDate,
                            maxDate),
                 date_breaks = "3 month",
                 date_labels = "%b %Y")


# **************** works correctly to show that the color for a party

cpiDataFiltered <- monthly_cpi_data %>%
    filter(date >= '2010-01-01')

# Load the presidential term data
presidential_terms <- read_csv("G:/excel/US President Terms.csv")

# Convert dates to Date class and add a color column based on the party
presidential_terms <- presidential_terms %>%
    mutate(start_date = as.Date(`Took office`, format = "%m/%d/%Y"),
           end_date = as.Date(`Left office`, format = "%m/%d/%Y"),
           color = ifelse(Party == "Republican", "lightpink", "lightgreen"))

# Define the date range for the CPI data
minDate <- min(cpiDataFiltered$date, na.rm = TRUE)
maxDate <- max(cpiDataFiltered$date, na.rm = TRUE)

# Filter presidential terms to include those that overlap with the CPI data range
presidential_terms_filtered <- presidential_terms %>%
    filter(start_date <= maxDate & end_date >= minDate)

# Create the base bar plot
base_plot <- ggplot() +
    geom_bar(data = cpiDataFiltered, aes(x = date, y = inflation_rate), stat = "identity", fill = "steelblue") +
    labs(title = "Monthly CPI from 2015 Onwards", x = "Date", y = "Monthly CPI") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(limits = c(minDate, maxDate), date_breaks = "3 month", date_labels = "%b %Y")

# Print the base plot
print(base_plot)

# Adjust the dates in presidential_terms_filtered to be within minDate and maxDate
presidential_terms_filtered <- presidential_terms_filtered %>%
    mutate(start_date = pmax(start_date, minDate),
           end_date = pmin(end_date, maxDate))

# Add shaded areas for presidential terms
final_plot <- base_plot +
    geom_rect(data = presidential_terms_filtered,
              aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = Party),
              alpha = 0.2) +
    scale_fill_manual(values = c("Republican" = "lightpink", "Democratic" = "lightgreen"))

# Print the final plot
print(final_plot)