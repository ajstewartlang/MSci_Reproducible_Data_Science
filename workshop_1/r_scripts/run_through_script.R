# First we need to load the packages we need - the following loads all packages 
# within the tidyverse universe of packages (plus associated datasets).
# If you haven't yet installed them on your computers, you'll first need to 
# type:
# install.packages("tidyverse")  
# install.packages("viridis")

library(tidyverse)
library(viridis)

# One dataset that is included in the tidverse packages contains information 
# about the characters in the Star Wars movies.  The dataset is called 
# 'starwars'.  Note this is all lower case. R is sensitive to case - so 
# 'starwars' is not the same as 'StarWars'
# You can just type the name of the dataset to see the first 10 lines
starwars

# You can open the full dataset in a Viewing window
view(starwars)

# The following displays just the column called "name" in the "starwars" dataset
starwars %>% 
  select(name)

# Note this is the same as
select(starwars, name)
# But by using the pipe we can build up a sequence of functions as we'll see 
# below.

# The tally() function tells us how many elements are
starwars %>% 
  tally()

# We can add a group_by() so before the tally() is called, we have
# a grouping by homeworld. The tally is then done within those groups.
starwars %>%
  group_by(homeworld) %>%
  tally()

# If we want to order from the highest number of occurrences to the lowest
# we can use the sort parameter.

starwars %>%
  group_by(homeworld) %>%
  tally(sort = TRUE)

# We can check what other parameters the tally() function can take by typing
# either:

help(tally)

# or

?tally

# The following displays just the column called "homeworld" in the dataset - you'll see 
# lots of missing data that is indicated by "NA"
starwars %>%
  select(homeworld) %>%
  view()

# The distinct function gives us just the distinct (unique) homeworlds
starwars %>%
  select(homeworld) %>%
  distinct()

# The following uses the n_distinct to return the number of unique homeworlds
starwars %>%
  select(homeworld) %>%
  n_distinct() 

# The following displays just the column called "height" in the dataset
# Notice again that missing values are labelled as "NA"
starwars %>%
  select(height) %>%
  view()

# We can use the filter and is.na functions to filter out "NA" values in the 
# height column if we wanted to we could map the output of this onto a new 
# variable (we'll see how to do that in a bit)
starwars %>%
  select(height) %>%
  filter(!is.na(height)) %>%
  view()

# Now let's plot a basic histogram using the ggplot function.  ggplot is based 
# on the grammar of graphics and involves building up a plot layer by layer. 
# Below we tell ggplot we want to use the starwars dataset and set up our 
# aesthetics such that the x-axis corresponds to the height column. We then
# add the geom_histogram() layer which uses the information from the ggplot() 
# layer to build a histogram.
starwars %>%
  ggplot(aes(x = height)) + 
  geom_histogram()

# We can now add a title and labels
starwars %>%
  ggplot(aes(x = height)) + 
  geom_histogram() +
  labs(title = "Histogram of Characters' Heights in Star Wars", 
       x = "Height (cm)", 
       y = "Count")

# Use the facet_wrap function to plot separately by species - this will take a 
# little time. 
# You can click on the 'Zoom' button to make the plot bigger. You can resize the
# zoomed window and also copy or save it (by clickig on Export).
starwars %>% 
  ggplot(aes(x = height)) + 
  geom_histogram() +
  labs(title = "Histogram of Characters' Heights in Star Wars", 
       x = "Height (cm)", 
       y = "Count") +  
  facet_wrap(~species)

# Now let's use the filter() function to filter only when we have Human and 
# Droid species.  The operator == means 'is equal to', '|' means OR and & means
# AND.
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass)))

# Let's plot our filtered data separately for Droids and Humans
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  ggplot(aes(x = height)) + 
  geom_histogram() +
  labs(title = "Histogram of Characters' Heights in Star Wars\nOnly Droids and Humans", 
       x = "Height (cm)", 
       y = "Count") +
  facet_wrap(~species)

# Let's aggregate our filtered dataset working out the means and standard 
# deviations for each of our two groups (Humans vs. Droids).
# We start by piping the starwars data through to the group_by() function
# which isgroups the data by species.  We then pipe this grouped data through 
# to the summarise() function to calculate the mean and sd deviation for our 
# height and mass variables.
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  group_by(species) %>% 
  summarise(mean_height = mean(height), sd_height = sd(height), 
            mean_mass = mean(mass), sd_mass = sd(mass))

# We can do a basic plot of our means by species
# Notice we use the geom_col() function to add a layer to our plot involve 
# plotting a bar graph.
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  group_by(species) %>% 
  summarise(mean_height = mean(height), sd_height = sd(height), 
            mean_mass = mean(mass), sd_mass = sd(mass)) %>%
  ggplot(aes(x = species, y = mean_height)) +
  geom_col()

# Now we can add some error bars using the geom_errorbar function. This tells us about variability
# around our means.
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  group_by(species) %>% 
  summarise(mean_height = mean(height), sd_height = sd(height), 
            mean_mass = mean(mass), sd_mass = sd(mass)) %>%
  ggplot(aes(x = species, y = mean_height)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_height - sd_height, 
                    ymax = mean_height + sd_height, width=.25))

# Now let's add some colour and nicer labels
starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  group_by(species) %>% 
  summarise(mean_height = mean(height), sd_height = sd(height), 
            mean_mass = mean(mass), sd_mass = sd(mass)) %>%
  ggplot(aes(x = species, y = mean_height, fill = species)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_height - sd_height, 
                    ymax = mean_height + sd_height, width=.25)) +
  labs(title = "Mean Height of Droids vs. Humans in Star Wars \nwith SD bars",
       x = "Species", 
       y = "Mean Height (cm)") +
  guides(fill = FALSE) 

starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  ggplot(aes(x = species, y = height, colour = species)) +
  geom_jitter(width = .1, size = 3, alpha = .75)

starwars %>%
  filter((species == "Human" | species == "Droid") & 
           (!is.na(height) & !is.na(mass))) %>%
  ggplot(aes(x = species, y = height, colour = species)) +
  geom_jitter(width = .1, size = 3, alpha = .75) +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  labs(title = "Height of Droids vs. Humans in Star Wars \nwith SD bars",
       x = "Species", 
       y = "Height (cm)") +
  guides(colour = FALSE)
 
# Now can you redo the code from line 73 but this time do everything for the
# mass variable instead of height?
