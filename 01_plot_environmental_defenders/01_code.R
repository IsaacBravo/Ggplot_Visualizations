#------------------------------------------------------------------------------
# Topic: Environmental defenders killed 2021
# Created by: Isaac Bravo
# Source: https://www.globalwitness.org/
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(tidyverse)
library(readxl)
library(gender)
library(stringi)
library(sf)
library(scico)

#------------- Collecting data from Global Witness Report 2021 ----------------#

data <- read_excel("data.xlsx")

#------------- Clean and Processing the data ----------------------------------#

data <- data %>% 
  mutate(Name = tolower(Name),
         Name = stri_trans_general(str = Name, id = "Latin-ASCII"),
         Country = tolower(Country),
         Name_new = str_split(Name, " ") %>% map_chr(., 1),
         Country_new = case_when(
           grepl("hondur", Country) ~ "honduras",
           TRUE ~ Country
         ))

data <- data %>% left_join(genderdata::kantrowitz, by = c("Name_new" = "name"))
data <- data %>% left_join(genderdata::ssa_national %>% 
                             group_by(name) %>% 
                             mutate(n = n()) %>% 
                             filter(year == 2012), by = c("Name_new" = "name"))
  
data <- data %>% mutate(Gender_new = ifelse(female < male, "male", "female"),
                        Name_new = ifelse(is.na(Name_new), n, Name_new))


openxlsx::write.xlsx(data %>% select(name = Name_new, country = Country_new, gender = Gender_new), "data_clean.xlsx")

data_clean <- read_excel("data_clean.xlsx") %>% select(-1)

#------------- Collecting world data (borders) --------------------------------#

world <- map_data("world") %>%   filter(region != "Antarctica") 
world <- world %>% mutate (region = case_when(subregion == " US" & region == "Virgin Islands" ~ "United States Virgin Islands",TRUE ~ region))

world$region <- recode(world$region, "UK" = "United Kingdom")
world$region <- recode(world$region, "USA" = "United States")
world$region <- recode(world$region, "Trinidad" = "Trinidad and Tobago")
world$region <- recode(world$region, "Saint Vincent" = "Saint Vincent and the Grenadines")
world$region <- recode(world$region, "Saint Kitts" = "Saint Kitts and Nevis")
world$region <- recode(world$region, "Macedonia" = "North Macedonia")
world$region <- recode(world$region, "Micronesia" = "Micronesia (Entity)")
world$region <- recode(world$region, "Democratic Republic of the Congo" = "Democratic Republic of Congo")
world$region <- recode(world$region, "Republic of Congo" = "Congo")
world$region <- recode(world$region, "Czech Republic" = "Czechia")
world$region <- recode(world$region, "Ivory Coast" = "Cote d'Ivoire")
world$region <- recode(world$region, "Antigua" = "Antigua and Barbuda")

#------------- Merging data----------------------------------------------------#

data_world <- world %>% 
  left_join(data_clean %>% 
              mutate(country = stri_trans_totitle(country)) %>% 
              group_by(country) %>% summarize(n = n()), by = c("region" = "country"))

data_death <- data_clean %>% 
  mutate(country = stri_trans_totitle(country)) %>% 
  group_by(country) %>% summarize(n = n()) %>% 
  mutate(colour = cut(n, 
                   c(-Inf, quantile(n, c(.25, .75)), Inf), 
                   labels = c("Low", "Medium", "High"))) %>% 
  right_join(data_world %>% distinct(region), by = c("country"="region")) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         colour = as.character(colour),
         colour = case_when(is.na(colour) ~ "No data",
                            TRUE ~ colour))

#------------- Plotting data---------------------------------------------------#

# Define colors for each level
group.colors <- c(`High` = "#fcbc04",        
                  `Medium` = "#1c2c4c", 
                  `Low` = "#6b7790", 
                  `No data` = "white") 

# Version 01
ggplot() + 
  geom_map(data = data_world, map = data_world,
           aes(long, lat, group = group,  map_id = region),
           fill = "white", color = "#dcdcdc")  +
  geom_map(data = data_death, map = world,
           aes(fill = colour, map_id = country),
           color = "black", size = 0.15, alpha = .8) +
  scale_fill_manual(values = group.colors, na.value = "grey90",
                    labels = c("High", "Medium", "Low", "No data")
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) + 
  theme(
        legend.direction="horizontal",
        legend.position = c(0.55, 0.04),
        legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(1.2, "lines"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 9, color = "grey40"),
        panel.grid.major = element_line(color = "grey40", size = 0.15),
        plot.title = element_text(color = "black", size = 20, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", size = 18,hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.spacing.x = unit(0.2, 'cm'),
        plot.caption = element_text(colour = "grey40", face = "bold", size=10, hjust=1, margin = margin(2,4,2,4))
        )  +
  labs(x = NULL, y = NULL,
       title = "Mortality rate of environmental defenders killed by country",
       subtitle = glue::glue("Period: 2021"),
       caption = "I. Bravo | Data: Global Witness (2021). Last line of Defense Report.") 

# Version 02

sf_world <- st_as_sf(rworldmap::getMap(resolution = "low")) %>%
  st_transform(crs = "+proj=robin")

sf_world_borders <- sf_world %>% 
                    left_join(data_death, by = c("SOVEREIGNT" = "country")) %>% 
                    filter(SOVEREIGNT != "Antarctica")
                  
ggplot(sf_world_borders) +
  geom_sf(color = "#7d92af",
          fill = "white") +
  geom_sf(aes(fill = colour), 
          color = "black", 
          alpha = 0.75,
          size = 0.3) + 
  labs(x = NULL, y = NULL,
       title = "Mortality rate of environmental defenders killed by country",
       subtitle = glue::glue("Period: 2021"),
       caption = "I. Bravo | Data: Global Witness (2021). Last line of Defense Report.") +
  theme(plot.title = element_text(color = "black", size = 19, face = "bold"),
        plot.subtitle = element_text(color = "grey40", size = 15),
        plot.caption = element_text(colour = "grey40", face = "bold", size=10, hjust=1),
        legend.direction="horizontal",
        legend.position = c(0.51, 1.03),
        legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(1.2, "lines"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.spacing.x = unit(0.2, 'cm')) +
  scale_fill_manual(values = group.colors, na.value = "grey90",
                    labels = c("High", "Medium", "Low", "No data"))
  
  
  


  