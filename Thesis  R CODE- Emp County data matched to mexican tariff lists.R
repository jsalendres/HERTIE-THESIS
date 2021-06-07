##Employment data  (https://www.census.gov/data/datasets/2008/econ/cbp/2008-cbp.html)

library(readr)
library(tidyverse)
#cpb datasets 2008-2009-2010
emp_county_data_08 <- read_csv("Downloads/Cbp08co.txt")
emp_county_data_09 <- read_csv("Downloads/Cbp09co.txt")
emp_county_data_10 <- read_csv("Downloads/Cbp10co.txt")

#TO BE USED FOR INTIIAL COUNTY EXPOSURE MAPS 

#09 emp first tariff list, 9055 county-products)

firstlist_09 #(SEE BELOW)

#09 emp second tariff list 9055 county-products)

secondlist_09 #(SEE BELOW)

############################################

#first list with 08 emp data (9339 countyy-products affected)

firsttarifflist <- list(111423, 111213, 311820, 111335, 311910, 311940, 312131, 312139, 311110, 339940, 325610, 339111, 322230,337210,335220, 335210, 335910, 337120,311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 323119, 313112, 314110, 327215, 339912, 337920, 332510, 333412, 333319, 333999, 334210, 339113, 339999)
empcounties_08_firsttarifflist <- filter(emp_county_data_08, naics %in% firsttarifflist)

#first list with 09 emp data (9055 countyy-products affected)

empcounties_09_firsttarifflist <- filter(emp_county_data_09,  naics %in% firsttarifflist)

##Employment exposure to Second Mexican Tariff List (2010) 
#Adding new sanctions (9/19 categories measured, 2915 county-product additions,  1762 counties-product removals )
#with 2008 cbp data
newsanctions <-  list(311612, 311513,311411,111310,111329,111336,311421,311214,311613,311340,311350,311423,311940,325610,325520,326290,332430,333120, 339111)

allcounties_08_secondtarifflist_newsanctions <- filter(emp_county_data_08,  naics %in% newsanctions)

#removing taken out sanctions (7/14 product categories measured, 1762 counties-product removals)

removedsanctions <- list(311910, 311940, 325610, 339111, 339940, 323119, 313112, 314110, 339912,  337920, 332510, 334210, 335910, 337120)

allcounties_08_secondtarifflist_removedsanctions <- filter(emp_county_data_08, naics %in% removedsanctions)

##########second tariff list dataset with Q12008 employment data (9830 county products affected)

secondtarifflist <- list(311612, 311513,311411,111310,111329,111336,311421,311214,311613,311340,311350,311423,325520,326290,332430,333120, 111423, 111213, 311820, 111335, 312131, 312139, 311110, 322230,337210,335220, 335210, 311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 327215, 333412, 333319, 333999, 339113, 339999)

empcounties_08_secondtarifflist <- filter(emp_county_data_08, naics %in% secondtarifflist)

#########second tariff list dataset with Q12009 employment data (9573 county products affected)

secondtarifflist <- list(311612, 311513,311411,111310,111329,111336,311421,311214,311613,311340,311350,311423,325520,326290,332430,333120, 111423, 111213, 311820, 111335, 312131, 312139, 311110, 322230,337210,335220, 335210, 311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 327215, 333412, 333319, 333999, 339113, 339999)

empcounties_09_secondtarifflist <- filter(emp_county_data_09, naics %in% secondtarifflist)

#########second tariff list dataset with Q12010  employment data (9492 county products affected)

secondtarifflist <- list(311612, 311513,311411,111310,111329,111336,311421,311214,311613,311340,311350,311423,325520,326290,332430,333120, 111423, 111213, 311820, 111335, 312131, 312139, 311110, 322230,337210,335220, 335210, 311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 327215, 333412, 333319, 333999, 339113, 339999)

empcounties_10_secondtarifflist <- filter(emp_county_data_10, naics %in% secondtarifflist)

#adding county names b merging spreadheets 

countynames <- read_csv("Documents/HERTIE/SEMESTER 4/THESIS/DATA/countynames.txt")

countynames$fips <- do.call(paste, c(countynames[c("fipstate", "fipscty")], sep = ""))

empcounties_09_firsttarifflist$fips <- do.call(paste, c(empcounties_09_firsttarifflist[c("fipstate", "fipscty")], sep = ""))

empcounties_09_secondtarifflist$fips <- do.call(paste, c(empcounties_09_secondtarifflist[c("fipstate", "fipscty")], sep = ""))

#with base data (09 emp first tariff list, confirmed good merge with 9055 county-products)

firstlist_09<- merge(countynames, empcounties_09_firsttarifflist, by.x = "fips", by.y = "fips")

#with base data (09 emp second tariff list, confirmed good merge 9055 county-products)

secondlist_09 <- merge(countynames, empcounties_09_secondtarifflist, by.x = "fips", by.y = "fips")

#duplicate column emp in firstlist09 in order to then have min, max and mean employment affected numeric

firstlist_09$empflag_min = firstlist_09$empflag
firstlist_09$empflag_max = firstlist_09$empflag
firstlist_09$empflag_mean = firstlist_09$empflag

minimum <- c("A" = 0, "B" = 20, "C" = 100, "E" = 250, "F" = 500, "G" = 1000, "H" = 2500,"I" = 5000, "J" = 10000, "K" = 25000,"L" = 50000, "M" = 100000)
firstlist_09$empflag_min <- minimum[firstlist_09$empflag_min]

maximum <- c("A" = 19, "B" = 99, "C" = 249, "E" = 499, "F" = 999, "G" = 2499, "H" = 4999,"I" = 9999, "J" = 24999, "K" = 49999,"L" = 99999, "M" = 101999)
firstlist_09$empflag_max <- maximum[firstlist_09$empflag_max]

firstlist_09$empflag_mean <- replace(firstlist_09$empflag_mean, is.na(firstlist_09$empflag_mean), 0)
firstlist_09$empflag_mean <- (firstlist_09$empflag_min + firstlist_09$empflag_max)/2

#forsecond list mean, min, max columns emp

secondlist_09$empflag_min = secondlist_09$empflag
secondlist_09$empflag_max = secondlist_09$empflag
secondlist_09$empflag_mean = secondlist_09$empflag

minimum <- c("A" = 0, "B" = 20, "C" = 100, "E" = 250, "F" = 500, "G" = 1000, "H" = 2500,"I" = 5000, "J" = 10000, "K" = 25000,"L" = 50000, "M" = 100000)
secondlist_09$empflag_min <- minimum[secondlist_09$empflag_min]

maximum <- c("A" = 19, "B" = 99, "C" = 249, "E" = 499, "F" = 999, "G" = 2499, "H" = 4999,"I" = 9999, "J" = 24999, "K" = 49999,"L" = 99999, "M" = 101999)
secondlist_09$empflag_max <- maximum[secondlist_09$empflag_max]

secondlist_09$empflag_mean <- replace(secondlist_09$empflag_mean, is.na(secondlist_09$empflag_mean), 0)
secondlist_09$empflag_mean <- (secondlist_09$empflag_min + secondlist_09$empflag_max)/2


###notes on additional code for creazion of above categories (no need to run code):
#secondtarifflist made using by adding1-3 and removing 4 basic data lists:

#1 new sanctions - 311612, 311513,311411,111310,111329,111336,311421,311214,311613,311340,311350,311423,311940,325610,325520,326290,332430,333120, 339111
#2 no county data - 111423, 111213, 311820, 111335, 311910, 311940, 312131, 312139, 311110, 339940, 325610, 339111, 322230,337210,335220, 335210, 335910, 337120
#3 yes county data - 311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 323119, 313112, 314110, 327215, 339912, 337920, 332510, 333412, 333319, 333999, 334210, 339113, 339999
#4 removed sanctions - 311910, 311940,325610, 339111, 339940, 323119, 313112, 314110,339912,337920,332510,334210,335910,337120

#breakdown for first list calculations
#no counties in products categories (matched to 08 employment county data)
productlist_firsttariff_no <- list(111423, 111213, 311820, 111335, 311910, 311940, 312131, 312139, 311110, 339940, 325610, 339111, 322230,337210,335220, 335210, 335910, 337120)
zerocounties_products_08 <- filter(emp_county_data_08, naics %in% productlist_firsttariff_no)

#yes counties in product categories (matched to 08 employment county data)

productlist_firsttariff_yes <- list(311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 323119, 313112, 314110, 327215, 339912, 337920, 332510, 333412, 333319, 333999, 334210, 339113, 339999)
counties_products_08 <- filter(emp_county_data_08,  naics %in% productlist_firsttariff_yes)

#no counties in products categories (matched to 09 employment county data)
productlist_firsttariff_no <- list(111423, 111213, 311820, 111335, 311910, 311940, 312131, 312139, 311110, 339940, 325610, 339111, 322230,337210,335220, 335210, 335910, 337120)
zerocounties_products_09 <- filter(emp_county_data_09, naics %in% productlist_firsttariff_no)

#yes counties in product categories (matched to 09 employment county data)

productlist_firsttariff_yes_09 <- list(311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 323119, 313112, 314110, 327215, 339912, 337920, 332510, 333412, 333319, 333999, 334210, 339113, 339999)
counties_products_09 <- filter(emp_county_data_09,  naics %in% productlist_firsttariff_yes)

#breakdown for second lsit addition calculations
#from secondtariff list no results in 08 employment data for 10: 
newsanctions_nocountyresults <-  list(325610, 339111, 311940, 111310,111329,111336, 311214, 311350, 326290, 332430)

#confirmation:
zerocounties_08_secondtarifflist <- filter(emp_county_data_08, naics %in% newsanctions_nocountyresults)

#and yes results for 9: 
newsanctions_yescountyresults <-  list(311612,  311613,  325520, 311340, 311411, 333120, 311513, 311421, 311423)

yescounties_08_secondtarifflist <- filter(emp_county_data_08,
                                          naics %in% newsanctions_yescountyresults)

#breakdown from second list add removal calculations 
#from these we already know that no results in employment data for 7: 311910, 311940, 325610, 339111, 339940, 335910, 337120

#and yes results for 7: 323119, 313112, 314110, 339912, 337920, 332510, 334210

#adds/removals with 09 data
#Adding new sanctions (2883 county-product additions, 1705 county-product taken out)

allcounties_09_secondtarifflist_newsanctions <- filter(emp_county_data_09, naics %in% newsanctions)

allcounties_09_secondtarifflist_removedsanctions <- filter(emp_county_data_09, naics %in% removedsanctions)

#adds/removals with 10 data
#Adding new sanctions (2888 county-product additions, 1642 county.product taken out)

allcounties_10_secondtarifflist_newsanctions <- filter(emp_county_data_10, naics %in% newsanctions)

allcounties_10_secondtarifflist_removedsanctions <- filter(emp_county_data_10, naics %in% removedsanctions)

#We can now look at total dataset after changes in tariff list using three data periods: 08,09, and 10 CBP DATA


