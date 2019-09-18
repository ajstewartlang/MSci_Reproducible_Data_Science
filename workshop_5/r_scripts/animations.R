# Animation with fictional data #### 
library(tidyverse)
library(gganimate)
library(ggimage)

set.seed(1234)
coffee_data <- seq(1:100)
time1 <- sort(sample(1:200, 100))
time2 <- sort(sample(1:200, 100))
coffee <- rep("coffee", 100)
tea <- rep("tea", 100)

data <- as.tibble(cbind(c(coffee, tea), c(time2, time2), c(coffee_data, coffee_data/2)))

colnames(data) <- c("Product", "Time", "n_sold") 

data$Product <- as.factor(data$Product)
data$Time <- as.integer(data$Time)
data$n_sold <- as.integer(data$n_sold)

# Just animated lines
data %>%
  ggplot(aes(x = n_sold, y = Time, group = Product, colour = Product)) +
  geom_point() +
  geom_line(size = 2) + 
  coord_flip() +
  transition_reveal(Time) +
  labs(title = "Coffee and tea purchases in a fictitious cafe", 
       x = "Minutes since opening",
       y = "Number sold") +
  theme(text = element_text(size = 15))

# With text labels
data %>%
  ggplot(aes(x = n_sold, y = Time, group = Product, colour = Product)) +
  geom_point() +
  geom_line(size = 2) + 
  coord_flip() +
  geom_text(aes(label = Product), size = 6, colour = "black") +
  transition_reveal(Time) +
  guides(colour = FALSE) +
  labs(title = "Coffee and tea purchases in a fictitious cafe", 
       x = "Minutes since opening",
       y = "Number sold") +
  theme(text = element_text(size = 15))

# Add .png images to animation 
data_recoded <- mutate(data, image = recode(Product,   
                                            "coffee" = "coffee.png",
                                            "tea" = "tea.png"))

data_recoded %>%
  ggplot(aes(x = n_sold, y = Time, group = image)) +
  coord_flip() +
  geom_line(size = 2, aes(colour = image)) +
  geom_image(aes(image = image), size = .15) +
  transition_reveal(Time) +
  guides(colour = FALSE) +
  labs(title = "Coffee and tea purchases in a fictitious cafe", 
       x = "Minutes since opening",
       y = "Number sold") +
  theme(text = element_text(size = 15))

# Animating the NHAMES dataset ####
library(NHANES)

# Boxplot of BMI by Race and AgeDecade 
NHANES %>% 
  distinct(ID, .keep_all = TRUE) %>%
  ggplot(aes(x = Race1, y = BMI, colour = Race1)) + 
  geom_boxplot() +
  guides(colour = FALSE) +
  labs(x = "Race", title = "Age = {closest_state}") +
  transition_states(AgeDecade)

NHANES_tidy <- NHANES %>%
  filter(Race1 != "Other") %>%
  filter(as.character(AgeDecade) != " 0-9")

my_plot <- NHANES_tidy %>%
  mutate(AgeDecade = fct_drop(AgeDecade," 0-9")) %>%
  group_by(AgeDecade, Race1) %>%
  summarise(median_BMI = median(BMI, na.rm = TRUE)) %>%
  ggplot(aes(x = median_BMI, y = reorder(Race1, median_BMI), colour = Race1)) +
  geom_point(size = 3) +
  labs(x = "Median BMI", y = "Race", title = "Median BMI by Race and by Age Group", 
       subtitle = "Age = {closest_state}") +
  transition_states(AgeDecade) +
  theme(text = element_text(size = 20)) +
  guides(colour = FALSE)

animate(my_plot, height = 300, width = 800)
anim_save("example_plot.gif")

# animated density histogram
my_plot <- NHANES_tidy %>%
  mutate(AgeDecade = fct_drop(AgeDecade," 0-9")) %>%
  ggplot(aes(x = BMI)) +
  geom_density(aes(y = ..density.., colour = "red", fill = "red")) +  
  labs(x = "BMI", title = "Density Histogram of BMI", 
       subtitle = "Age = {closest_state}") +
  transition_states(AgeDecade) +
  theme(text = element_text(size = 20)) +
  guides(colour = FALSE, fill = FALSE)

animate(my_plot, height = 400, width = 400)
anim_save("example_plot.gif")

# From David Robinson
# https://gist.github.com/dgrtwo/d590078aae86e24418a7cf5a9fb31ae3

# Setup
options(gganimate.nframes = 200)
set.seed(2019)

simulation <- tibble(roll = 1:10000) %>%
  mutate(result = sample(6, n(), replace = TRUE)) %>%
  crossing(nrolls = seq(10, 10000, 10)) %>%
  filter(roll <= nrolls) %>%
  count(nrolls, result)

ggplot(simulation, aes(result, n, fill = factor(result))) +
  geom_col(position = "identity", show.legend = FALSE) +
  transition_manual(nrolls) +
  view_follow() +
  scale_x_continuous(breaks = 1:6) +
  labs(y = "# of rolls with this result",
       title = "Distribution of results after { current_frame } rolls of a six-sided die")
