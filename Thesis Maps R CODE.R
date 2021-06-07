#install.packages("usmap")
library(usmap)
library(ggplot2)
library(readr)
library(tidyverse)


#COUNTY MAPS (use firstlist_09 and secondlist_09) from R code sheet "Thesis - Emp County data matched"
#create map for average emp first list

fordatamap <- transform(firstlist_09, CountyTotal= ave(firstlist_09$empflag_mean, firstlist_09$fips, FUN=sum))[-2]

data_map_firstlist <- fordatamap[,c("fips", "CountyTotal")]

data_map2_firstlist <- distinct(data_map_firstlist, .keep_all = TRUE)


firstlistmap_average <- plot_usmap(data = data_map2_firstlist, values = "CountyTotal") +
  labs(title = "Map 1: Average Jobs Targeted by First Tariff List (2009)") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "lightblue2", high ="red1", 
                        name = "Number of Targeted Jobs",label = scales::comma) + 
  theme(legend.position = "right")

#create map for max emp first list, but average works since relativeness for map is the same. 

fordatamap_max <- transform(firstlist_09, CountyTotal= ave(firstlist_09$empflag_max, firstlist_09$fips, FUN=sum))[-2]

data_map1_max <- fordatamap[,c("fips", "CountyTotal")]

data_map2_max <- distinct(data_map1_max, .keep_all = TRUE)

firstlistmap_max <- plot_usmap(data = data_map2_max, values = "CountyTotal") +
  labs(title = "Maximum Jobs Targeted by First Tariff List (2009)") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "lightblue2", high ="red1", 
    name = "Number of Targeted Jobs",label = scales::comma,) + 
  theme(legend.position = "right")


# Using secondlist_09 (SEE BELOW)

#create map for average emp second list

fordatamap1 <- transform(secondlist_09, CountyTotal= ave(secondlist_09$empflag_mean, secondlist_09$fips, FUN=sum))[-2]

data_map_secondlist <- fordatamap1[,c("fips", "CountyTotal")]

data_map4_secondlist <- distinct(data_map_secondlist, .keep_all = TRUE)

secondlistmap_average <- plot_usmap(data = data_map4_secondlist, values = "CountyTotal") +
  labs(title = "Map 2: Average Jobs Targeted Second Tariff List (2010)") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "lightblue2", high ="red1",
    name = "Number of Targeted Jobs",label = scales::comma,) + 
  theme(legend.position = "right")

#HURTING POINTS: comparison between first and second list (simple visual but should run differences in differences etc)

difference_in_lists <- merge(data_map_firstlist, data_map4_secondlist, by="fips")

difference_in_lists <- difference_in_lists %>% 
  rename(
    FirstList_emp_mean = CountyTotal.x,
    SecondList_emp_mean = CountyTotal.y)

difference_in_lists$first_to_second_list_pos_neg_hit <- (difference_in_lists$FirstList_emp_mean - difference_in_lists$SecondList_emp_mean)

difference_in_lists$difference <- ifelse(difference_in_lists$FirstList_emp_mean > difference_in_lists$SecondList_emp_mean, 'First List More Impact',
                     ifelse(difference_in_lists$FirstList_emp_mean < difference_in_lists$SecondList_emp_mean, 'Second List More Impact', 'Equal'))

#MAP THE CHANGE FROM FIRST TO SECOND LIST

# prep the two column dataset

diffmap1 <- data.frame(difference_in_lists$fips, difference_in_lists$first_to_second_list_pos_neg_hit)

diffmap1 <- diffmap1 %>% 
  rename(
    fips = difference_in_lists.fips,
    first_to_second_list_pos_neg_hit = difference_in_lists.first_to_second_list_pos_neg_hit)

#counties affected differences first and second list 

listdifference_09_mean_emp1 <- plot_usmap(data = diffmap1, values = "first_to_second_list_pos_neg_hit") +
  labs(title = "Map 3: Carousel Turn - Average Jobs Targeted from First to Second Tariff List") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "red1", high ="lightblue1", 
                        name = "Number of Targeted Jobs",label = scales::comma) + 
  theme(legend.position = "right")

#counties affected by more than one extra jobs in comparison to first list 
listdifference_09_mean_emp2 <- plot_usmap(data = diffmap1, values = "first_to_second_list_pos_neg_hit") +
  labs(title = "Map 3: Carousel Turn - Counties with 50+ additional jobs targeted from First to Second Tariff List") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "red1", high ="lightblue1", 
                        name = "Number of Targeted Jobs",label = scales::comma, limits = c(-4000, -50)) + 
  theme(legend.position = "right")

#counties affected by more than ten extra jobs in comparison to first list 

listdifference_09_mean_emp3 <- plot_usmap(data = diffmap1, values = "first_to_second_list_pos_neg_hit") +
  labs(title = "Counties with 100+ additional jobs targeted in Second Tariff List)") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "red1", high ="lightblue1", 
    name = "Number of Targeted Jobs",label = scales::comma, limits = c(-4000, -100)) + 
  theme(legend.position = "right")

#counties affected by more than 500 jobs in comparison to first list 

listdifference_09_mean_emp_4 <- plot_usmap(data = diffmap1, values = "first_to_second_list_pos_neg_hit") +
  labs(title = "Counties with  500 to 4000 additional jobs targeted in Second Tariff List)") + 
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "red1", high ="lightblue1", 
    name = "Number of Jobs",label = scales::comma, limits = c(-4000, -250)) + 
  theme(legend.position = "right")

#DISTRICT MAPS (use multiplecongressdistrict_firstlist_09 and multiplecongressdistrict_secondlist_09)
library(USAboundaries)
library(tidyverse)
library(readxl)
library(ggplot2)
library(shiny)  
library(sf)

district_110to112_data <- read_csv("Documents/HERTIE/SEMESTER 4/THESIS/R FILES/county to district relationship 110-112 congress .txt", 
                                                              skip = 1)

# adding congressional district based on counties (to run first test on mail to DOT and also can draw congressional district affected maps)

final_district_110to112_data <- district_110to112_data %>% 
  ungroup %>% 
  mutate(district = CD110 %>% str_replace("00", "1") %>% as.numeric) %>% 
  mutate(unique_ID = paste(STATE, district, sep="."))


us_congressional <- us_congressional(resolution = "low")

#removing alaska, hawaii, and puerto rico

us_congressional <- us_congressional[us_congressional$statefp != "02" & us_congressional$statefp != "15" & us_congressional$statefp != "72", ] 

#to see basic us district map

plot(st_geometry(us_congressional))

#create unique id also in this one for merging with emp data qith state and district together

us_congressional2 <- us_congressional %>% 
  ungroup %>% 
  mutate(district = cd115fp %>% str_replace("00", "1") %>% as.numeric) %>% 
  mutate(unique_ID = paste(statefp, district, sep=".")) 

district_and_mapdata <- merge(final_district_110to112_data, us_congressional2, by.x = "unique_ID", by.y = "unique_ID") 

district_and_mapdata$fips <- paste(district_and_mapdata$STATE, district_and_mapdata$COUNTY, sep="")

#with employment lists now we can create a dataset with county, state, and district variables and the rest of the data attached. 
#all.x = TRUE is so that we see old distrctis that did not find a match in the map data. around 300 rows only so it is ok loss of data.
#some of loss might also be alaska, hawaii, and puerto rico (plus district changes in mainland, so not many unplanned losses. 

all_for_district_data <- merge(firstlist_09, district_and_mapdata, by.x = "fips", by.y = "fips", all.x =TRUE) 

####RUN WITH ESTABLISHMENTS BELOW INSTEAD OF EMPFLAG?
#create map for average emp first list
#lose around 300 that did not have empflag
#we lose around 3k rows that had na in empflag (is there a way to retrieve some data from other columns after?)
clean_all_for_district_data <- all_for_district_data[!is.na(all_for_district_data$unique_ID),]

clean_all_for_district_data <- all_for_district_data[!is.na(all_for_district_data$empflag_mean),]


#average for all rows pointing to same state- district combination (unique congressional district)

average_district_empleffect <- clean_all_for_district_data %>% group_by(unique_ID) %>% summarise(emp.sum = sum(empflag_mean))

##all came out unique
#only problem is NAs in unique_ID which are counties that are not in current map structure since it is a new map but 
#they were old counties, so i can either hand code this or update map to a newer version with shape files. 
## WARNING(small thing to solve): FOR DATA ANALYSIS MAKE SURE TO HAVE DATASET THAT ACCOUNTS FOR ALL THESE COUNTIES

average_district_empleffect <- distinct(average_district_empleffect, .keep_all = TRUE)

#DID NOT NEED:  to add all geolocation back to these based on the uniqueID.y

#readymap_average_district_empleffect <- merge(average_district_empleffect, clean_all_for_district_data, by.x = "unique_ID.y", by.y = "unique_ID.y") 

#DID NOT NEED: get distinct on district to have unique totals and then plot 1:1 per district

#readymap_average_district_empleffect <- readymap_average_district_empleffect %>% distinct(unique_ID.y, .keep_all = TRUE)

#now onto maps (STILL NOT WORKING HERE) must bring data into congressional2 to follow example in web 
#have same ID names before merging

#average_district_empleffect$unique_ID <- average_district_empleffect$unique_ID.y
#average_district_empleffect$unique_ID.y <- NULL

#now merge
cong.map <- left_join(us_congressional2, average_district_empleffect, by = "unique_ID")

#FIRST MAP WORKS FOR FIRS LIST (REDO WITH SECOND LIST) http://congressdata.joshuamccrain.com/visualization.html#32_congressional_district_map

ggplot(cong.map, aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted by First Tariff List (2009)", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Number of Targeted Jobs") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#better version with square root to see differences (we can see  no data states, we can fiy after)

cong.map %>% 
  mutate(emp.sum = sqrt(emp.sum)) %>% 
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted (sqroot) by First Tariff List 2009", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Number of Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#if we need by states

cong.map %>% 
  filter(state_name %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota","Wisconsin")) %>% 
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted in the Midwest Region", subtitle = "Regional targeting from First Tariff List") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

cong.map %>% 
  filter(state_name %in% c("Maryland", "Delaware", "New Jersey", "Pennsylvania", "Connecticut", "Rhode Island", "New York", "Massachusetts", "Vermont", "New Hampshire","Maine")) %>% 
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted in the Northeast Region", subtitle = "Regional targeting from First Tariff List") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#jp own version with ranges that can be adapted

ggplot(cong.map, aes(fill = emp.sum)) +
  geom_sf(color="black", size=.2, alpha=.5) +
  labs(title = "3,000+ Jobs Targeted by First Tariff List (2009)", subtitle = "by Congressional District") +
  scale_fill_continuous(low = "lightblue1", high ="red1", 
                        name = "Number of Targeted Jobs",label = scales::comma, limits = c(3000, 14000)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#maps for second emp list

secondlist_09 #(SEE BELOW)

#with employment lists now we can create a dataset with county, state, and district variables and the rest of the data attached. 
#all.x = TRUE is so that we see old distrctis that did not find a match in the map data. around 300 rows only so it is ok loss of data.
#some of loss might also be alaska, hawaii, and puerto rico (plus district changes in mainland, so not many unplanned losses. 

second_all_for_district_data <- merge(secondlist_09, district_and_mapdata, by.x = "fips", by.y = "fips", all.x =TRUE) 


#create map for average emp first list
#lose around 300 that did not have empflag
#we lose around 3k rows that had na in empflaf (is there a way to retrieve some data from other columns after?)
second_clean_all_for_district_data <- second_all_for_district_data[!is.na(second_all_for_district_data$unique_ID.y),]

second_clean_all_for_district_data <- second_all_for_district_data[!is.na(second_all_for_district_data$empflag_mean),]


#average for all rows pointing to same state- district combination (unique congressional district)

second_average_district_empleffect <- second_clean_all_for_district_data %>% group_by(unique_ID) %>% summarise(emp.sum = sum(empflag_mean))

##all came out unique
#only problem is NAs in unique_ID.y which are counties that are not in current map structure since it is a new map but 
#they were old counties, so i can either hand code this or update map to a newer version with shape files. 
## WARNING(small thing to solve): FOR DATA ANALYSIS MAKE SURE TO HAVE DATASET THAT ACCOUNTS FOR ALL THESE COUNTIES

second_average_district_empleffect <- distinct(second_average_district_empleffect, .keep_all = TRUE)

#DID NOT NEED:  to add all geolocation back to these based on the uniqueID.y

#readymap_average_district_empleffect <- merge(average_district_empleffect, clean_all_for_district_data, by.x = "unique_ID.y", by.y = "unique_ID.y") 

#DID NOT NEED: get distinct on district to have unique totals and then plot 1:1 per district

#readymap_average_district_empleffect <- readymap_average_district_empleffect %>% distinct(unique_ID.y, .keep_all = TRUE)

#now onto maps (STILL NOT WORKING HERE) must bring data into congressional2 to follow example in web 
#have same ID names before merging

#second_average_district_empleffect$unique_ID <- second_average_district_empleffect$unique_ID.y
#second_average_district_empleffect$unique_ID.y <- NULL
#now merge
second_cong.map <- left_join(us_congressional2, second_average_district_empleffect, by = "unique_ID")

#FIRST MAP THAT WORKS FOR SECOND LIST  http://congressdata.joshuamccrain.com/visualization.html#32_congressional_district_map

ggplot(second_cong.map, aes(fill = emp.sum)) +
  geom_sf(color="black", size=.2, alpha=.5) +
  labs(title = "Jobs Targeted by Second Tariff List (2010)", subtitle = "by Congressional District") +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Number of Targeted Jobs") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#better version with square root to see differences (we can see  no data states, we can fiy after)

second_cong.map %>% 
  mutate(emp.sum = sqrt(emp.sum)) %>% 
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted (sqroot) by Second Tariff List 2010", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red", name = "Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#if we need by states

second_cong.map %>% 
  filter(state_name %in% c("Maryland", "Delaware", "New Jersey", "Pennsylvania", "Connecticut", "Rhode Island", "New York", "Massachusetts", "Vermont", "New Hampshire","Maine")) %>%
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted in the Northeast Region", subtitle = "Regional targeting from Second Tariff List") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

second_cong.map %>% 
  filter(state_name %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota","Wisconsin")) %>%
  ggplot(aes(fill = emp.sum)) +
  labs(title = "Jobs Targeted in the Midwest Region", subtitle = "Regional targeting from Second Tariff List") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "lightblue1", high = "red1", name = "Targeted Jobs")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#jp own version with ranges that can be adapted

ggplot(second_cong.map, aes(fill = emp.sum)) +
  geom_sf(color="black", size=.2, alpha=.5) +
  labs(title = "3,000+ Jobs Targeted by Second Tariff List (2010)", subtitle = "by Congressional District") +
  scale_fill_continuous(low = "lightblue1", high ="red", 
                        name = "Number of Targeted Jobs",label = scales::comma, limits = c(3000, 14000)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#map for difference between lists

#HURTING POINTS: comparison between first and second list (simple visual but should run differences in differences etc)

district_difference_in_lists <- merge(average_district_empleffect, second_average_district_empleffect, by="unique_ID")

#rename columns

district_difference_in_lists <- district_difference_in_lists %>% 
  rename(
    FirstList_emp_mean = emp.sum.x,
    SecondList_emp_mean = emp.sum.y)

district_difference_in_lists$first_to_second_list_pos_neg_hit <- (district_difference_in_lists$FirstList_emp_mean - district_difference_in_lists$SecondList_emp_mean)

district_difference_in_lists$difference <- ifelse(district_difference_in_lists$FirstList_emp_mean > district_difference_in_lists$SecondList_emp_mean, 'First List More Impact',
                                         ifelse(district_difference_in_lists$FirstList_emp_mean < district_difference_in_lists$SecondList_emp_mean, 'Second List More Impact', 'Equal'))

#DISTRICT MAP THE CHANGE FROM FIRST TO SECOND LIST

# prep the two column dataset

district_diffmap <- data.frame(district_difference_in_lists$unique_ID, district_difference_in_lists$first_to_second_list_pos_neg_hit)

district_diffmap <- district_diffmap %>% 
  rename(
    unique_ID = district_difference_in_lists.unique_ID,
    first_to_second_list_pos_neg_hit = district_difference_in_lists.first_to_second_list_pos_neg_hit)

#districts mapos of affected differences first and second list 

#now merge
listdifferences_cong.map <- left_join(us_congressional2, district_diffmap, by = "unique_ID")

#FIRST MAP Tfor differences LIST

ggplot(listdifferences_cong.map, aes(fill = first_to_second_list_pos_neg_hit)) +
  labs(title = "Employment Effect of Carousel Turn between Tariff lists ", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "red1", high = "lightblue1", name = "Number of Targeted Jobs") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#if we need by states

listdifferences_cong.map %>% 
  filter(state_name %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota","Wisconsin")) %>% 
  ggplot(aes(fill = first_to_second_list_pos_neg_hit)) +
  labs(title = "Carousel Effect in the Midwest Region", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "red1", high = "lightblue1", name = "Jobs Targeted Change")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

listdifferences_cong.map %>% 
  filter(state_name %in% c("Maryland", "Delaware", "New Jersey", "Pennsylvania", "Connecticut", "Rhode Island", "New York", "Massachusetts", "Vermont", "New Hampshire","Maine")) %>% 
  ggplot(aes(fill = first_to_second_list_pos_neg_hit)) +
  labs(title = "Carousel Effect in the Northeast Region", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_gradient(low = "red1", high = "lightblue1", name = "Jobs Targeted Change",)+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#jp own version with ranges that can be adapted

ggplot(listdifferences_cong.map, aes(fill = first_to_second_list_pos_neg_hit)) +
  labs(title = "250+ Additional Targeted Jobs in Carousel Turn", subtitle = "by Congressional District") +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_continuous(low = "red1", high ="lightblue1", 
                        name = "Number of Targeted Jobs",label = scales::comma, limits = c(-8500, -250)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))





