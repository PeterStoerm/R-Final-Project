# Importing libaries
library(ggplot2)
library(leaflet)
library(cowplot)
library(sf)

# Importing data set
data <- read.csv('2019.csv')
head(data)


# Data cleaning (checking NA values)
str(data)
print('--------------------------------------------------------------------------------------------------')
sum = sum(is.na(data))
str = "There are these many NA values in the data set: "
result = paste(str, sum)
print(result)

# Data cleaning (Renaming columns )
data_1 <-data %>%
    rename(
        rank = Overall.rank,
        region = Country.or.region, # I made this lower case for map visualization
        score = Score,
        GDP_per_capita = GDP.per.capita,
        support = Social.support,
        life_expectancy = Healthy.life.expectancy,
        freedom =  Freedom.to.make.life.choices,
        generosity = Generosity ,
        coruption = Perceptions.of.corruption
    )
head(data_1)

# Data clenaing (Dropping rank column, its not needed)
data_final <- select(data_1, -rank)
head(data_final)
tail(data_final)

# Summary (using summary())
summary(data_final)

# Plot the distribution of variables using a histogram
score <- data_final$score
hist(score)

# Adding a new column to find the percent happier of each country compared the least happiest

percent_happy <- data_final %>% mutate(percent_happier = (score/min(score) * 100))
head(percent_happy)
tail(percent_happy)

# Provide findings in a plot or a table
p1 <- ggplot(data = data_final, aes(x = GDP_per_capita, y = score)) +
  geom_point()
p2 <-ggplot(data = data_final, aes(x = support, y = score)) +
  geom_point()
p3 <-ggplot(data = data_final, aes(x = life_expectancy, y = score)) +
  geom_point()
p4 <-ggplot(data = data_final, aes(x = freedom, y = score)) +
  geom_point()
p5 <-ggplot(data = data_final, aes(x = generosity, y = score)) +
  geom_point()
p6 <-ggplot(data = data_final, aes(x = coruption, y = score)) +
  geom_point()
plot_grid(p1, p2, p3, p4, p5, p6)

#Attempt to fix the world

mapdata <- map_data("world")
mapdata1 <- mapdata %>%    # Rename columns
  # Replace "United States of America" by USA in the region column
  mutate(
    region = ifelse(region == "USA", "United States", region)
    )     
mapdata_final <- left_join(mapdata1, data_final, by = "region")
map <- ggplot(mapdata_final, aes(x = long, y = lat, group=group)) + geom_polygon(aes(fill = score), color = "black")
map
