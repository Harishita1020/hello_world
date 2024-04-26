install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
install.packages("maps")
install.packages("ggplot2")
install.packages("plotly")
library("plotly")
library(maps)
library(dplyr)
library(ggplot2)

cleaned_unicef1 <- read.csv("cleaned_unicef1.csv")
cleaned_metadata <- read.csv("cleaned_metadata.csv")
total_data <- read.csv("total_data.csv")

data_join <- full_join(cleaned_metadata, cleaned_unicef1)

data_join <- full_join(cleaned_metadata, cleaned_unicef1, by = join_by(country, year))
data_join <- full_join(cleaned_metadata, cleaned_unicef1, by = c("country" , "year" ))
full_join(cleaned_metadata, cleaned_unicef1, by = join_by(country, year))
full_join(cleaned_metadata, cleaned_unicef1, by = c("country" , "year" ))


# Load necessary libraries
library(ggplot2)
library(maps)
library(dplyr)

# Load your CSV file
data <- read.csv("C:/Users/raosh/OneDrive/Desktop/AakashRao/total_data.csv")

# Get map data for the world
world_map <- map_data("world")
head(world_map)

#map
map_world <- map_data("world")

map_data_join <- full_join(data, map_world, by = c("country" = "region"))                       
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(title = "Global Distribution of Child Deprivation ",  
subtitle = "Percentage of children suffering at least two deprivations")







# bar graph
average_deprivation <- cleaned_unicef1 %>%
  group_by(country) %>%
  summarize(AverageDeprivation = mean(obs_value, na.rm = TRUE))
top_countries <- average_deprivation %>%
  arrange(desc(AverageDeprivation)) %>%
  top_n(10, AverageDeprivation) %>%
  mutate(country = factor(country, levels = rev(unique(country))))  # Ensure correct ordering in the plot

ggplot(top_countries, aes(x = country, y = AverageDeprivation, fill = country)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(  # Adjust y-axis scale to fit the data range properly
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_viridis_d(direction = -1, guide = "none") +  # Color scale
  labs(title = "A Closer Look at Inequality: Child Welfare by Country",
       subtitle = "Top 10 Countries by Average Percentage of Children Suffering from At Least Two Deprivations 2011-2018",
       x = "Country",
       y = "Average Percentage of Children in Deprivation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Improve legibility of country names
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"  # Removing the legend as it's redundant
  )








#scatter plot with a linear regression line

library(tidyverse)
library(ggplot2)


# Ensure the country column is in the same format
cleaned_metadata$country <- tolower(cleaned_metadata$country)
cleaned_unicef1$country <- tolower(cleaned_unicef1$country)

# Join the datasets
combined_data <- inner_join(cleaned_unicef1, cleaned_metadata, by = "country")

# Select and rename the necessary columns
combined_data <- combined_data %>%
  select(
    country,
    `GDP per capita` = `GDP per capita (constant 2015 US$)`,  # Renaming for easier reference
    `Percentage Children Suffering` = obs_value  # Simplified naming
  )

# Create the scatter plot with a linear regression line
ggplot(combined_data, aes(x = `GDP per capita`, y = `Percentage Children Suffering`, color = country)) +
  geom_point(alpha = 0.8) +  # Points with transparency
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Linear regression line with confidence interval
  labs(
    title = "Economic Prosperity and Its Impact on Child Well-being",
    subtitle = "Analyzing the correlation between GDP per capita and child deprivation rates",
    x = "GDP Per Capita (constant 2015 USD)",
    y = "Percentage of Children Experiencing Deprivations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  scale_color_viridis_d()  # Enhanced color scale

# Print the plot
print(ggplot)








#map plotting here



map_world <- map_data("world")
cleaned_metadata$country <- tolower(cleaned_metadata$country)
cleaned_unicef1$country <- tolower(cleaned_unicef1$country)
combined_data <- full_join(cleaned_metadata, cleaned_unicef1_1_, by = "country", relationship = "many-to-many")
map_cleaned_metadata <- full_join(map_world, combined_data, by = c("region" = "country"))
manual_percent_format <- function(x) {
  scales::percent(x / 100) 
}
ggplot(unicef1_joined, aes(x = long, y = lat, group = group, fill = Percentage children suffering at least two deprivation)) +
  geom_polygon(color = "white", linewidth = 0.5) +  # Ensures country borders are visible
  coord_fixed(1.3) +  # Keeps the map's aspect ratio correct
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Percentage Suffering\nAt Least Two Deprivations",
                      labels = scales::percent_format(scale = 1)) +  # Uses a blue gradient and formats labels as percentages
  labs(title = "Global Distribution of Child Deprivation",
       subtitle = "Percentage of children suffering at least two deprivations",
       caption = "Data source: UNICEF") +
  theme_minimal() +
  theme(legend.position = "right",  # Adjusts legend position
        plot.title = element_text(size = 16, face = "bold"),  # Enhances title aesthetics
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic"))




library(ggplot2)
library(dplyr)

# Read the data from the CSV file
data <- read.csv("total_data.csv")

# Inspecting the first few rows of the data to understand its structure
head(data)

# Assuming column H is named 'obs_value', column for longitude is 'long', and latitude is 'lat'
# If the names are different, you will need to adjust them in the code below

# Plotting the map
ggplot2(data, aes(x = long, y = lat, fill = obs_value)) +
  geom_polygon(color = "white", size = 0.5) +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Observation Value") +
  labs(title = "Map Visualization",
       subtitle = "Visualization based on obs_value from Column H",
       caption = "Data source: Provided CSV") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic"))



#scatter plot
filtered_data <- data_join %>%
  filter(
         country %in% c( "India", "Bangladesh", "Pakistan", "Indonesia", "Philippines")) %>%
  group_by(country)
scatter_plot <- ggplot(filtered_data, aes(x = GDP.per.capita..constant.2015.US.., y = obs_value, color = country, group = country)) +
  geom_point() +  # Adds points to the scatter plot, color-coded by country
  geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  # Adds a linear regression line per country
  labs(title = "Indicator over Years",
       x = "GDP.per.capita..constant.2015.US..",
       y = "Percentage children suffering at least two deprivation") +
  theme_minimal()

# Convert to plotly for an interactive plot
plotly_scatter_plot <- ggplotly(scatter_plot, tooltip = "text")

# If you're in an R environment that supports it, you can directly display the plot
print(plotly_scatter_plot)



#timeseries
filtered_data <- cleaned_unicef1 %>%
  filter(
    country %in% c( "India", "Bangladesh", "Pakistan", "Indonesia", "Philippines"))
  
  
time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, group = country, color = country)) +
  geom_line() +  # Draws lines for each group (country)
  geom_point(size = 1.5, shape = 21, fill = "white") +  # Adds points with a small white fill
  geom_smooth(se = FALSE, aes(color = country), method = "loess") +  # Adds a LOESS smoothed line
  facet_wrap(~ country, scales = "free_y") +  # Facets by country with free y scales
  labs(title = "Time Series of Observation Values for Selected Countries",
       x = "Year",
       y = "Observation Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Print the enhanced plot
print(time_series_plot)

# Convert to an interactive plotly plot
interactive_plot <- ggplotly(time_series_plot)
# Print the interactive plot
print(interactive_plot)

#scatter plot

filtered_data <- data_join %>%
  filter(
    country %in% c( "India", "Bangladesh", "Pakistan", "Indonesia", "Philippines")) %>%
  group_by(country)
# Create scatter plot with regression line
scatter_plot <- ggplot(filtered_data, aes(x = Military.expenditure....of.GDP., y = GDP.per.capita..constant.2015.US..)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line without confidence interval
  labs(x = "Military Expenditure as % of GDP", y = "GPD Per Capita", title = "Scatter Plot with Regression Line") +  # Labels
  theme_minimal()  # Optional: Customize plot theme

# Convert to plotly for an interactive plot
plotly_scatter_plot <- ggplotly(scatter_plot, tooltip = "text")

# If you're in an R environment that supports it, you can directly display the plot
plotly_scatter_plot

