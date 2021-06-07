
#Employment exposure to Mexican Tariff List 1 (2009) 
#Employment data is from 2008 look at (https://www.census.gov/data/datasets/2008/econ/cbp/2008-cbp.html)
#therefore this shows pre-tariff situation, but static in time so use as basis levels. 

library(readr)
emp_county_data_08 <- read_csv("Downloads/Cbp08co.txt")

#no counties in products categories (matched to 08 employment county data)
productlist_firsttariff_no <- list(111423, 111213, 311820, 111335, 311910, 311940, 312131, 312139, 311110, 339940, 325610, 339111, 322230,337210,335220, 335210, 335910, 337120)
zerocounties_products_08 <- filter(emp_county_data_08,
                        naics %in% productlist_firsttariff_no)
#yes counties in product categories (matched to 08 employment county data)

productlist_firsttariff_yes <- list(311421, 311411, 311422, 311423, 311999, 312111, 311222, 325620, 326191, 326199, 322291, 323111, 323119, 313112, 314110, 327215, 339912, 337920, 332510, 333412, 333319, 333999, 334210, 339113, 339999)
counties_products_08 <- filter(emp_county_data_08,
                                   naics %in% productlist_firsttariff_yes)

# 0 results for cthe following 18 product categories
naics_111423_christmastrees <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 111423)
naics_111213_onion <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 111213)
naics_311820_cookies <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311820)
naics_311820_grapes <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 111335)
naics_311910_snacks <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311910)
naics_311940_spices <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311940)
naics_312131_alcoholicbeverages <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 312131)
naics_312139_cider <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 312139)
naics_311110_animalfood <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311110)
naics_339940_officesupplies <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 339940)
naics_325610_soaps <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 325610)
naics_339111_medicalequip <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 339111)
naics_322230_office<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 322230)
naics_337210_furniture <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 337210)
naics_335220_houseappliances <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 335220)
naics_335210_minorhouseappliances <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 335210)
naics_335910_batteries <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 335910)
naics_337120_furniture2 <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 337120)

#Counties affected: 

#Dehydrated fruits and begetables - 400 counties affected to various degrees employment-wise 

naics_311421_fruitsveggies <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311421)

#frozen food and vegetables - 137 counties affected to various degrees employment-wise

naics_311411_frozenfruitveggies <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311411)

#canned fruit and vegetables - 91 counties affected to various degrees employment-wise

naics_311422_cannedfruitveggies <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311422) 
#canned foods - 125 counties affected to various degrees employment-wise

naics_311423_cannedfoods <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311423)

#other foods - 402 counties affected to various degrees employment-wise 

naics_311999_otherfoods <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311999) 

#beverages - 283 counties affected to various degrees employment-wise 

naics_312111_beverages <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 312111) 

#oils - 99 counties affected to various degrees employment-wise 

naics_311222_oils <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 311222) 

#cosmetics - 295 counties affected to various degrees employment-wise 

naics_325620_cosmetics <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 325620) 

#plastics house products - 348 counties affected to various degrees employment-wise 
naics_326191_houseplastics <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 326191) 

#plastics other products - 1389 counties affected to various degrees employment-wise 

naics_326199_otherplastics<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 326199)

#diapers and other sanitary products - 96 counties affected to various degrees employment-wise 

naics_322291_diapersandothers<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 322291) 

#print products - 193 counties affected to various degrees employment-wise 

naics_323111_books<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 323111) 

#prints - 690 counties affected to various degrees employment-wise 

naics_323119_prints<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 323119) 

#strings (hilos) - 77 counties affected to various degrees employment-wise 

naics_313112_strings<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 313112)

#rugs  - 125 counties affected to various degrees employment-wise 

naics_314110_rugs<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 314110)

#glass products for domestic use  - 547 counties affected to various degrees employment-wise 

naics_327215_glass <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 327215) 

#jewlery  - 86 counties affected to various degrees employment-wise 

naics_339912_jewlery <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 339912)

#curtains  - 218 counties affected to various degrees employment-wise 

naics_337920_curtains <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 337920) 

#locks  - 393 counties affected to various degrees employment-wise 

naics_332510_locks <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 332510) 

#industrial fridges  - 124 counties affected to various degrees employment-wise 

naics_333412_industrialfridges <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 333412) 


#commercial machinery - 537 counties affected to various degrees employment-wise 

naics_333319_machinery <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 333319) 

#general machinery - 627 counties affected to various degrees employment-wise 

naics_333999_generalmachinery <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 333999) 

#phone equipment - 173 counties affected to various degrees employment-wise 

naics_3334210_phoneequip <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 334210) 


#oftamlmic- 729 counties affected to various degrees employment-wise 

naics_339113_oftalmicproducts <- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 339113)

#other manufacturing industries - 1155 counties affected to various degrees employment-wise 

naics_339999_manufacturing<- emp_county_data_08 %>% group_by(naics) %>% filter(naics == 339999) 


