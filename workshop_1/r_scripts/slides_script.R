library(tidyverse)
library(viridis)
library(ggbeeswarm)
library(ggthemes)

# Data tidying and wrangling ####
# Create data for 10,000 people - each with measures of Working Memory (WM), IQ, and 
# reading Comprehension (Comp)

data1 <- read_csv("data_files/data1.csv")

# Create data for 48 participants (all present in data) taking part in an experiment

dataRT <- read_csv("data_files/dataRT.csv")

# Combine the data1 and dataRT datasets using the inner_join() function
# Map this onto a new variable we're calling dataRT_all
dataRT_all <- inner_join(data1, dataRT, by = "id")

data_transformed <- mutate(dataRT_all, log_simple = log(simple_sentence), 
                            log_complex = log(complex_sentence))

filtered_data <- filter(data_transformed, id != 2006)

data_long <- gather(dataRT, "condition", "rt", c("simple_sentence", "complex_sentence"))
View(data_long)

data_wide <- spread(data_long, "condition", "rt")
View(data_wide)

# Use dplyr to get some summary statistics from the RT dataset using the pipe operator
data_long %>% 
  group_by(condition) %>% 
  summarise(Mean = mean(rt), Min = min(rt), Max = max(rt), SD = sd(rt))

# Recode one column capturing 2x2 and then splitting
# First create the data set - 24 items each with one RT measure for each of 4 conditions

my_data <- read_csv("data_files/my_data.csv")
my_data

# Recode condition columns follows:
# Condition 1 = prime A, target A
# Condition 2 = prime A, target B
# Condition 3 = prime B, target A
# Condition 4 = prime B, target B

my_data <- my_data %>% 
  mutate(condition = recode(condition, 
                            "1" = "primeA_targetA",
                            "2" = "primeA_targetB", 
                            "3" = "primeB_targetA", 
                            "4" = "primeB_targetB"))

# now separate the Condition column using "_" as our separator
my_data <- separate(my_data, col = "condition", into = c("prime", "target"), sep = "_")
my_data

# combine again
my_data <- unite(my_data, col = "condition", c("prime", "target"), sep = "_")
wide_data <- spread(my_data, key = "condition", value = "rt")
wide_data

# or using the pipe %>%
my_data %>% 
  unite(col = "condition", c("prime", "target"), sep = "_") %>%
  spread(key = "condition", value = "rt")

# Visualisation ####
# Bar Graph
data_summ <- data_long %>% group_by(condition) %>% summarise(Mean = mean(rt), sd = sd(rt))

ggplot(data_summ, aes (x = condition, y = Mean, group = condition, 
                       fill = condition, ymin = Mean - sd, ymax = Mean + sd)) + 
  geom_bar(stat = "identity", width = .5) + 
  geom_errorbar(width = .25) +  
  ggtitle("Bar chart with Error Bars") + 
  guides(fill = FALSE) 

# When boxplots can mislead

data2 <- read_csv("data_files/data2.csv")

ggplot(data2, aes(x = group, y = rt)) + geom_boxplot()
ggplot(data2, aes(x = group, y = rt)) + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data2, aes(x = group, y = rt)) + geom_boxplot() + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data2, aes(x = group, y = rt)) + geom_violin() + geom_jitter(width = .1, alpha = .5)

# Violin Plot
ggplot(data_long, aes(x = condition, y = rt, group = condition, fill = condition)) + 
  geom_violin() + 
  geom_jitter(alpha = .25, position = position_jitter(0.05)) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

# Beeswarm plot
ggplot(data_long, aes(x = condition, y = rt, group = condition, fill = condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme(text = element_text(size = 15))

# Five Thirty Eight theme
data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_fivethirtyeight()

# Economist theme
data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_economist()

my_plot <- data_long %>%
  mutate(Condition = recode(condition, 
                            "complex_sentence" = "Complex Sentence",
                            "simple_sentence" = "Simple Sentence")) %>%
  ggplot(aes(x = Condition, y = rt, group = Condition, fill = Condition)) + 
  geom_beeswarm(alpha = .25) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Plot of Reaction Time (ms.) by Condition",
       x = "Condition", 
       y = "Reaction Time (ms.)") +
  theme_economist()
  
ggsave("my_plot.png", my_plot, height = 8, width = 15, units = "cm")



# Raincloud plot on data 
library(RColorBrewer)
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

set.seed(1234)
id <- sample(id, 48)
simple_sentence <- as.integer(rnorm(48, mean = 2000, sd = 140))
complex_sentence <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- tibble(id, simple_sentence, complex_sentence)

dataRT <- gather(dataRT, key = "condition", value = "rt", c("simple_sentence", "complex_sentence"))

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- plyr::ddply(dataRT, ~ condition, summarise, mean = mean(rt), median = median(rt), 
               lower = lb(rt), upper = ub(rt))

ggplot(data = dataRT, aes(y = rt, x = condition, fill = condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = rt, color = condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL) +
  scale_y_continuous(breaks = seq(1500,3000,by = 200))


# find out about the dataset and generate some descriptives
head(mpg)
unique(mpg$manufacturer)
length(unique (mpg$manufacturer))
mpg %>% group_by(class) %>% summarise(Mean = mean(hwy))
mpg %>% group_by(cyl) %>% summarise(Mean = mean(hwy))

# build a violin plot with added descriptives
ggplot(mpg, aes(x = factor(cyl), y = cty, fill = factor(cyl))) + 
  geom_violin() +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = .5) +
  labs(title = "City Fuel Consumption by Number of Cylinders", 
       x = "Number of Cylinders", y = "City Fuel Consumption (mpg)")

# facet wrap by vehicle class with displacement instead of cylinder number
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() +
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  labs(title = "Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# now add a linear function to each
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "lm") + 
  labs(title = "Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# plot basic histogram
ggplot(mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .5) +
  guides(fill = FALSE) +
  labs(title = "Histogram of Cylinder Displacement",
       x = "Displacement (litres)",
       y = "Count")

ggplot(mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .5) +
  geom_histogram(data = filter(mpg, class == "suv"), fill = "grey", binwidth = .5) +
  guides(fill = FALSE) +
  labs(title = "Histogram of Cylinder Displacement",
       subtitle = "SUVs highlighted",
       x = "Displacement (litres)",
       y = "Count")

ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  labs(title = "Scatterplot of Highway Fuel Consumption against \nEngine Displacement Grouped by Class",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# 2d histogram with density heatmap
ggplot(mpg, aes(x = displ, y = hwy)) +
  stat_bin2d(bins = 10, colour = "black") + 
  scale_fill_viridis() + 
  labs(title = "Density heatmap of Highway Fuel Consumption against Engine Displacement",
       x = "Displacement (litres)",
       y = "Highway Fuel Consumption (mpg)")

# Using gganimate
library(gganimate)

# Animated plot using the 'gapminder' dataset
library(gapminder)

# gganimate code of life expectancy by GDP for each contintent over time
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point(size = 2, alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +
  transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)

# Now vary each point according to population size
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +  
  transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)

# gganimate code faceted by continent
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  labs(title = "Gapminder dataset", subtitle = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life expectancy (years)') +   transition_time(year) +
  ease_aes('linear') +
  theme(text = element_text(size = 20))

animate(p, height = 500, width = 900)


# Dot plot on life expectancy data
# Static faceted by year
df_Americas <- gapminder %>% filter(continent == "Americas")

ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) + 
  facet_wrap(~ year) + 
  theme(text = element_text(size=8))

# Dynamic - separate frame per year
df_Americas <- gapminder %>% filter(continent == "Americas")
p <- ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "Life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(text = element_text(size = 14)) +
  transition_time(year) +
  ease_aes('linear')

animate(p, height = 500, width = 900)



