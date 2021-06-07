#TOP COUNTIES AFFECTED - WILL HELP ANALYSIS OF WHAT DEPENDENT VARIABLES WE SEE CHANGING IN THESE TO USE IN WHOLE STUDY
#first list
# top counties in each list - what is going on in these. 

top_firstlist <- fordatamap[,c("fips", "ctyname", "CountyTotal")]

#ranking can be seen using this next spreadsheet
top_firstlist_unique <- distinct(top_firstlist, .keep_all = TRUE)

#first ten
top20_counties_firstlist <- top_n(top_firstlist_unique, 20, CountyTotal)

#second list

# top counties in each list - what is going on in these. 

top_secondlist <- fordatamap1[,c("fips", "ctyname", "CountyTotal")]

#ranking can be seen using this next spreadsheet
top_secondlist_unique <- distinct(top_secondlist, .keep_all = TRUE)

#first ten
top20_counties_secondlist <- top_n(top_secondlist_unique, 20, CountyTotal)


#major differences between lists

top20_difference_in_lists <- merge(top_firstlist_unique, top_secondlist_unique, by="fips")

top20_difference_in_lists <- top20_difference_in_lists %>% 
  rename(
    FirstList_emp_mean = CountyTotal.x,
    SecondList_emp_mean = CountyTotal.y)

top20_difference_in_lists$top_first_to_second_list_pos_neg_hit <- (top20_difference_in_lists$FirstList_emp_mean - top20_difference_in_lists$SecondList_emp_mean)


#first 20
top20_counties_listdifference <- top_n(top20_difference_in_lists, -20, top_first_to_second_list_pos_neg_hit)




