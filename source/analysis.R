library(dplyr)
library(ggplot2)
library(mapproj)
data1 <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

Sum_of_each_race_by_state <- data1 %>% 
  select(year, state, county_name, total_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>% 
  group_by(state) %>% 
  filter(year >= 1998) %>% 
  summarize(
    sum_aapi = sum(aapi_jail_pop, na.rm = TRUE),
    sum_black = sum(black_jail_pop, na.rm = TRUE),
    sum_latinx = sum(latinx_jail_pop, na.rm = TRUE),
    sum_native = sum(native_jail_pop, na.rm = TRUE),
    sum_white = sum(white_jail_pop, na.rm = TRUE),
    sum_other = sum(other_race_jail_pop, na.rm = TRUE)
  ) 

#For 1998-2018, which state has the most white inmates?

most_white <- Sum_of_each_race_by_state %>% 
  filter(sum_white == max(sum_white)) %>% 
  pull(state)
  
most_black <- Sum_of_each_race_by_state %>% 
  filter(sum_black == max(sum_black)) %>% 
  pull(state)

most_native <- Sum_of_each_race_by_state %>% 
  filter(sum_native == max(sum_native)) %>% 
  pull(state)

#2013-2018 Regional data
by_region <- data1 %>% 
  select(year, state, region, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>% 
  group_by(region) %>% 
  filter(year >= 2013) %>% 
  summarize(
    sum_aapi = sum(aapi_jail_pop, na.rm = TRUE),
    sum_black = sum(black_jail_pop, na.rm = TRUE),
    sum_latinx = sum(latinx_jail_pop, na.rm = TRUE),
    sum_native = sum(native_jail_pop, na.rm = TRUE),
    sum_white = sum(white_jail_pop, na.rm = TRUE),
    sum_other = sum(other_race_jail_pop, na.rm = TRUE)
  ) 
  
most_latinx_by_region <- by_region %>% 
  filter(sum_latinx == max(sum_latinx)) %>% 
  pull(region)

most_aapi_by_region <- by_region %>% 
  filter(sum_aapi == max(sum_aapi)) %>% 
  pull(region)

most_black_by_region <- by_region %>% 
  filter(sum_black == max(sum_black)) %>% 
  pull(region)

most_white_by_region <- by_region %>% 
  filter(sum_white == max(sum_white)) %>% 
  pull(region)

most_native_by_region <- by_region %>% 
  filter(sum_native == max(sum_native)) %>% 
  pull(region)
  
most_other_by_region <- by_region %>% 
  filter(sum_other == max(sum_other)) %>% 
  pull(region)  

#Chart 1

Trends_10yrs_Chart_data <- data1 %>% 
  select(year, state, county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>% 
  filter(state == "WA" & county_name == "King County" & year >= 1998, na.rm = TRUE ) 

 KingCounty_racial_change <- ggplot(Trends_10yrs_Chart_data, aes(x = year)) +
   geom_line(aes(y = black_jail_pop, color = "black")) +
   geom_line(aes(y = latinx_jail_pop,  color = "Latinx")) +
   geom_line(aes(y = white_jail_pop, color = "White")) +
   geom_line(aes(y = aapi_jail_pop, color = "Asian American/Pasific Islander")) +
   labs(title = "The change of racial population in jails of King County, WA(1998-2018)", 
        x = "Year", y = "population", color = "Race") + 
   theme(legend.position = "bottom")
 KingCounty_racial_change

 
#Chart 2 Gender jail population change in 10 years,
 Female_vs_male <- data1 %>% 
   select(year, state, county_name, total_jail_pop, female_jail_pop, male_jail_pop) %>% 
   filter(state == "WA" & county_name == "King County" & year >= 1998, na.rm = TRUE ) 
 
 KingCounty_gender_change <- ggplot(Female_vs_male, aes(x = year)) +
   geom_line(aes(y = female_jail_pop, color = "Female")) +
   geom_line(aes(y = male_jail_pop, color = "Male")) +
   scale_fill_discrete(name = "gender",
                       labels = c("female","male"))+
   labs(title = "The change of gender population in jails of King County, WA(1998 - 2018)", 
        x = "Year", y = "population", color = "Gender") + 
   theme(legend.position = "right")

 KingCounty_gender_change
 
 #Map 
 black_prop_by_state <- data1 %>% 
   select(year, state, county_name, total_jail_pop, black_jail_pop) %>% 
   filter(year == max(year)) %>% 
   group_by(state) %>% 
   summarize(
     sum_total = sum(total_jail_pop, na.rm = TRUE),
     sum_black = sum(black_jail_pop, na.rm = TRUE),
     prop = sum_black/sum_total
   )
 black_prop_by_state$state <- tolower(state.name[match(black_prop_by_state$state,state.abb)])
 
#Join data (reference to online materials)
state_map <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(black_prop_by_state) 
  
#Map
  blank_theme <- theme_bw() + theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) #remove unwanted parts of the map;
  
  Map <- ggplot(state_map) + geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prop), 
    color = "White", size = .15
  ) + 
    coord_map() +
    scale_fill_continuous(low = "Blue", high = "Red") + 
    labs(fill = "Ratio of black inmates by state") + blank_theme

  
 

 
 
 
 

  