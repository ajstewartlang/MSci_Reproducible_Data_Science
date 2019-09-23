library(tidyverse)
library(ggforce)
library(concaveman)
library(nycflights13)

# based on this great review here: https://rviews.rstudio.com/2019/09/19/intro-to-ggforce/

str(airports)
head(airports)

my_plot <- airports %>%
  filter(lon < 0, lat > 23, tzone != "\\N") %>%
  ggplot(aes(lon, lat, color = tzone)) + 
  geom_point(show.legend = FALSE)  

my_plot 

my_plot +
  geom_mark_rect(aes(label = tzone, fill = tzone), show.legend = FALSE) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  theme_void() 
  
my_plot +
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  theme_void() 

my_plot +
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE, expand = unit(3, "mm")) +
  labs(title = "Plot of continental US airports grouped by IANA time zone") +
  facet_zoom(xy = (tzone == "America/Anchorage")) +
  theme_no_axes() 

# Example of alluvial diagran

prep_planes <- planes %>%
  filter(year > 1998, year < 2005) %>%
  filter(engine != "Turbo-shaft") %>%
  select(manufacturer, engine) %>%
  mutate(manufacturer = str_to_title(manufacturer))
  
prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engine), show.legend = FALSE, alpha = 0.3) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

prep_planes <- planes %>%
  filter(year > 1960) %>%
  filter(engines != 2) %>%
  select(manufacturer, engines) %>%
  mutate(manufacturer = str_to_title(manufacturer))

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engines), show.legend = FALSE, alpha = 0.5) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

prep_planes %>%
  group_by(manufacturer, engines) %>%
  summarise(n())

# Star Wars dataset

star_wars_tally <- starwars %>%
  filter(!is.na (homeworld) & !is.na(species)) %>% 
  group_by(species) %>%
  tally() 

prep_star_wars <- left_join(starwars, star_wars_tally, by = "species") %>%
  filter(!is.na (homeworld) & !is.na(species)) %>%
  filter(n > 1) %>%
  select(homeworld, species) 

prep_star_wars %>%
  mutate(homeworld = factor(homeworld), species = factor(species)) %>%
  group_by(species) %>%
  summarise(n())

prep_star_wars %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x = x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = homeworld), show.legend = FALSE, alpha = 0.5) +
  geom_parallel_sets_axes(color = "white", fill = "white", size = 25) +
  geom_parallel_sets_labels(angle = 0, size = 3.5) +
  theme_no_axes() +
  labs(title = "Mapping of Homeworlds to Species in the Star Wars universe")



