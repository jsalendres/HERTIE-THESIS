#comparing with letters sent to DOT by house representatives 


library(readxl)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(stargazer)

#upload excel sheet with data

depart_of_transport_pro_con_truck_congress_mail_09_10 <- read_excel("Documents/HERTIE/SEMESTER 4/THESIS/DATA/depart. of transport pro con truck congress mail 09-10.xlsx")


#CORRELATION WORK (MUST BE AT DISTRICT LEVEL)

#prep: create data frame with relevant columns of district emp data (average_district_empleffect) and mail data (depart_of_transport_pro_con_truck_congress_mail_09_10)


depart_of_transport_pro_con_truck_congress_mail_09_10$unique_ID <- depart_of_transport_pro_con_truck_congress_mail_09_10$`DISTRICT WITH NUMBERS`

#reduces to 174 cases since out of 435 districts, only 174 sent letters to DOT, so the loss of some districts is fine with this first data.
#but wait based on this some letters don't aply to first list, only things before august 18th, 2010.
#for first list data, we need to take out september and october 2010. 
#we can still run but keep this in mind, since second data will be useful for those letters also, and difference list. 

#which districts highly afefcted from (average_district_empleffect) are missing. i can see louisiana missing. 
#calculations can be based off of which are present or not based on certain levels of emp effect.
#do not just focus on this matched list since that leaves out those who were affected and did not sign. 

firstlist_prepcorr <- merge(depart_of_transport_pro_con_truck_congress_mail_09_10, average_district_empleffect, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

#mixed resulta it seems with people singing either in favor of pilot program or against
#analysis:

#pearson correlation: here we see a corr of -0.211 which is something but we how we need to expand analysis

res <- cor.test(firstlist_prepcorr$emp.sum, firstlist_prepcorr$CON_TRUCK, 
                method = "pearson")
res


#using second list it is -0.233 (if mexico is inc ontrol of tariffs they increase punishment on con truck)

secondlist_prepcorr <- merge(depart_of_transport_pro_con_truck_congress_mail_09_10, second_average_district_empleffect, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

res_secondlist <- cor.test(secondlist_prepcorr$emp.sum, secondlist_prepcorr$CON_TRUCK, 
                method = "pearson")
res_secondlist

#with difference lists it is 0.149 (logic is see targeting by mexico based on behaviour on these stances)
# i believe this swing in sign shows that the least employment affected the more they are likely to be against truck program
#AGAINST WHAT WE WOULD EXPECT

differenceslist_prepcorr <- merge(depart_of_transport_pro_con_truck_congress_mail_09_10, district_diffmap, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

res_differenceslist <- cor.test(differenceslist_prepcorr$first_to_second_list_pos_neg_hit, differenceslist_prepcorr$CON_TRUCK, 
                           method = "pearson")
res_differenceslist

#this is the formula for the scale:

difference_in_lists$first_to_second_list_pos_neg_hit <- (difference_in_lists$FirstList_emp_mean - difference_in_lists$SecondList_emp_mean)


#CONCLUSION-OF COURSE NOT MUCH BECAUSE CAMPS ARE DIVIDED ON WHICH LETTER TO SIGN

#https://www.congress.gov/bill/112th-congress/house-bill/2407/cosponsors

#FROM THIS LIST OF 23 congressmen STILL AGSINAT IT IN JULY 2011 AS DEAL IS BEING REACHED, what happened to the rest
#from thr 72 that signed with defazio? see what happened to their districts, were they targeted?




# METHOD2: "CONGRESS_ACTIVATION" merge the other way around to keep members that did not send letters

inclusive_firstlist_prepcorr <- merge(average_district_empleffect, depart_of_transport_pro_con_truck_congress_mail_09_10, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

inclusive_firstlist_prepcorr$ID[!is.na(inclusive_firstlist_prepcorr$ID)] <- 1

inclusive_firstlist_prepcorr$ID[is.na(inclusive_firstlist_prepcorr$ID)] <- 0

inclusive_firstlist_prepcorr$ID <- as.numeric(inclusive_firstlist_prepcorr$ID)

#regression to letter reponses first list: not stat significant

firstlm <- lm(ID ~ emp.sum, data = inclusive_firstlist_prepcorr)
summary(firstlm)

#first list logit: not stat significant (pre reaction)

inclusive_firstlist_prepcorr$ID <- factor(inclusive_firstlist_prepcorr$ID)
logit <- glm(ID ~ emp.sum, data = inclusive_firstlist_prepcorr, family = "binomial")
summary(logit)

stargazer(firstlm, logit, type = "html", out = "regression1.html" ,title = "CONGRESS ACTIVATION FIRST LIST TARIFFS")


#secondlist: not stat. significant

inclusive_secondlist_prepcorr <- merge(second_average_district_empleffect, depart_of_transport_pro_con_truck_congress_mail_09_10, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

inclusive_secondlist_prepcorr$ID[!is.na(inclusive_secondlist_prepcorr$ID)] <- 1

inclusive_secondlist_prepcorr$ID[is.na(inclusive_secondlist_prepcorr$ID)] <- 0

inclusive_secondlist_prepcorr$ID <- as.numeric(inclusive_secondlist_prepcorr$ID)

secondlm <- lm(ID ~ emp.sum, data = inclusive_secondlist_prepcorr)
summary(secondlm)

#second list logit: not stat significant 

inclusive_secondlist_prepcorr$ID <- factor(inclusive_secondlist_prepcorr$ID)

logit2 <- glm(ID ~ emp.sum, data = inclusive_secondlist_prepcorr, family = "binomial")
summary(logit2)

stargazer(secondlm, logit2, type = "html", out = "regression2.html" ,title = "CONGRESS ACTIVATION SECOND LIST TARIFFS")


# differences list: statistically significant *******

inclusive_difflist_prepcorr <- merge(district_diffmap, depart_of_transport_pro_con_truck_congress_mail_09_10, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

inclusive_difflist_prepcorr$ID[!is.na(inclusive_difflist_prepcorr$ID)] <- 1

inclusive_difflist_prepcorr$ID[is.na(inclusive_difflist_prepcorr$ID)] <- 0

inclusive_difflist_prepcorr$ID <- as.numeric(inclusive_difflist_prepcorr$ID)

#to get correct relationship since we want to invert meaning of a negative switch from 1st to second list to mean more employment targeted
#for ease of understanding 
inclusive_difflist_prepcorr$first_to_second_list_pos_neg_hit <-  (inclusive_difflist_prepcorr$first_to_second_list_pos_neg_hit * (-1))


thirdlm <- lm(ID ~ first_to_second_list_pos_neg_hit, data = inclusive_difflist_prepcorr)
summary(thirdlm)


#the below chart has both OLS and binomial in it as you can see:

stargazer(thirdlm, logit3, type = "html", out = "regression3.html" ,title = "CONGRESS ACTIVATION CAROUSEL EFFECT")


#difference list is statistically significant: letter reaction is well measured? *********

inclusive_difflist_prepcorr$ID <- factor(inclusive_difflist_prepcorr$ID)

logit3 <- glm(ID ~ first_to_second_list_pos_neg_hit, data = inclusive_difflist_prepcorr, family = "binomial")
summary(logit3)

# Calculate OR for specific increment step of continuous variable
#2 times more liekly to be activated into writing letter if more than 3,000 jobs were targeted in between lists

or_glm(data = inclusive_difflist_prepcorr, model = logit3, 
       incr = list(first_to_second_list_pos_neg_hit = -3000))

#3 times more likely if 5k jobs targeted?

or_glm(data = inclusive_difflist_prepcorr, model = logit3, 
       incr = list(first_to_second_list_pos_neg_hit = -5000))

#6 times more likely if 5k jobs targeted?

or_glm(data = inclusive_difflist_prepcorr, model = logit3, 
       incr = list(first_to_second_list_pos_neg_hit = -8300))

### FINAL STARGAZER HERE: GET HIGHER ESTIMATE BY CALCULATING PER 1000 JOBS INSTEAD OF PER JOB

inclusive_difflist_prepcorr$ID <- as.numeric(inclusive_difflist_prepcorr$ID) 

inclusive_difflist_prepcorr$hitby1000jobs <- ((inclusive_difflist_prepcorr$first_to_second_list_pos_neg_hit)/1000)

#we multiply by -1 in order to code for bigger second hits as being "higher" and therefore we take away negatuve sign.
#by inversing we make sure we get a positive relationship whenever more jobs are targeted in the second round
#initially this would have been a negatuve number since more jobs were "lost/targeted" so we inverse.
inclusive_difflist_prepcorr$hitby1000jobs <-  (inclusive_difflist_prepcorr$hitby1000jobs * (-1))

thirdlm_1000 <- lm(ID ~ hitby1000jobs, data = inclusive_difflist_prepcorr)
summary(thirdlm_1000)

inclusive_difflist_prepcorr$ID <- factor(inclusive_difflist_prepcorr$ID)

logit3_1000 <- glm(ID ~ hitby1000jobs, data = inclusive_difflist_prepcorr, family = "binomial")
summary(logit3_1000)
  
stargazer(thirdlm_1000, logit3_1000, type = "html", out = "regression4.html" ,title = "CAROUSEL EFFECT ON CONGRESS ACTIVATION (per 1000 jobs)")

####second list try

inclusive_secondlist_prepcorr <- merge(second_average_district_empleffect, depart_of_transport_pro_con_truck_congress_mail_09_10, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

inclusive_secondlist_prepcorr$ID[!is.na(inclusive_secondlist_prepcorr$ID)] <- 1

inclusive_secondlist_prepcorr$ID[is.na(inclusive_secondlist_prepcorr$ID)] <- 0

inclusive_secondlist_prepcorr$ID <- as.numeric(inclusive_secondlist_prepcorr$ID)

inclusive_secondlist_prepcorr$hitby1000jobs <- ((inclusive_secondlist_prepcorr$emp.sum)/1000)

#inclusive_secondlist_prepcorr$hitby1000jobs <-  (inclusive_secondlist_prepcorr$hitby1000jobs * (-1))


secondlm_1000 <- lm(ID ~ hitby1000jobs, data = inclusive_secondlist_prepcorr)
summary(secondlm_1000)

inclusive_difflist_prepcorr$ID <- factor(inclusive_secondlist_prepcorr$ID)

logit2_1000 <- glm(ID ~ hitby1000jobs, data = inclusive_secondlist_prepcorr, family = "binomial")
summary(logit2_1000)

stargazer(secondlm_1000, logit2_1000, type = "html", out = "regression5.html" ,title = "SECOND LIST EFFECT ON CONGRESS ACTIVATION (per 1000 jobs)")

##firs list per 1000

inclusive_firstlist_prepcorr <- merge(average_district_empleffect, depart_of_transport_pro_con_truck_congress_mail_09_10, by.x = "unique_ID", by.y = "unique_ID", all.x =TRUE) 

inclusive_firstlist_prepcorr$ID[!is.na(inclusive_firstlist_prepcorr$ID)] <- 1

inclusive_firstlist_prepcorr$ID[is.na(inclusive_firstlist_prepcorr$ID)] <- 0

inclusive_firstlist_prepcorr$ID <- as.numeric(inclusive_firstlist_prepcorr$ID)

inclusive_firstlist_prepcorr$hitby1000jobs <- ((inclusive_firstlist_prepcorr$emp.sum)/1000)

#inclusive_firstlist_prepcorr$hitby1000jobs <-  (inclusive_firstlist_prepcorr$hitby1000jobs * (-1))


firstlm_1000 <- lm(ID ~ hitby1000jobs, data = inclusive_firstlist_prepcorr)
summary(firstlm_1000)

inclusive_firstlist_prepcorr$ID <- factor(inclusive_firstlist_prepcorr$ID)

logit1_1000 <- glm(ID ~ hitby1000jobs, data = inclusive_firstlist_prepcorr, family = "binomial")
summary(logit1_1000)

stargazer(firstlm_1000, logit1_1000, secondlm_1000, logit2_1000, thirdlm_1000, logit3_1000, type = "html", out = "regression7.html" ,title = "Table 6: TARIFF EFFECTS ON CONGRESS ACTIVATION (per 1000 jobs)", covariate.labels = c("Employment Targeted", "Constant"),
          dep.var.caption  = "Dependent Variable",
          dep.var.labels   = "DOT Complaint", column.labels = c("1st List", "2nd List",  "Carousel"), column.separate = c(2, 2, 2), digits = 2, ci = FALSE, initial.zero = FALSE)

#second list logit: not stat significant 

inclusive_secondlist_prepcorr$ID <- factor(inclusive_secondlist_prepcorr$ID)

logit2 <- glm(ID ~ emp.sum, data = inclusive_secondlist_prepcorr, family = "binomial")
summary(logit2)

stargazer(secondlm, logit2, type = "html", out = "regression2.html" ,title = "CONGRESS ACTIVATION SECOND LIST TARIFFS")

###additional plots
#first list
plot(x = inclusive_firstlist_prepcorr$emp.sum, 
     y = inclusive_firstlist_prepcorr$ID,
     main = "Graph 6: First List Tariff - Congress Activation",
     xlab = "Employment Targeted",
     ylab = "Congress Activation",
     pch = 20,
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(10000, 0.9, cex = 0.8, "Letter sent")
text(10000, 0.1, cex= 0.8, "No Letter sent")

# add the estimated regression line
abline(firstlm, 
       lwd = 1.8, 
       col = "steelblue")

#second list:

plot(x = inclusive_secondlist_prepcorr$emp.sum, 
     y = inclusive_secondlist_prepcorr$ID ,
     main = "Graph 7: Second List Tariff - Congress Activation",
     xlab = "Employment Targeted",
     ylab = "Congress Activation",
     pch = 20,
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(13000, 0.9, cex = 0.8, "Letter sent")
text(13000, 0.1, cex= 0.8, "No Letter sent")

# add the estimated regression line
abline(secondlm, 
       lwd = 1.8, 
       col = "steelblue")

#carousel turn:

plot(x = inclusive_difflist_prepcorr$first_to_second_list_pos_neg_hit, 
     y = inclusive_difflist_prepcorr$ID ,
     main = "Graph 8: Carousel Turn - Congress Activation",
     xlab = "+/- Targeted Employment Change 1st to 2nd List",
     ylab = "Congress Activation",
     pch = 20,
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(6000, 0.9, cex = 0.8, "Letter sent")
text(6000, 0.1, cex= 0.8, "No Letter sent")

# add the estimated regression line
abline(thirdlm, 
       lwd = 1.8, 
       col = "steelblue")

#getting robust standard errors
#install.packages("lmtest")
library(lmtest)
coeftest(firstlm, type = "HC1")
coeftest(secondlm, type = "HC1")
coeftest(thirdlm, type = "HC1")

#confidence intervals
confint(logit1_1000)
confint.default(logit1_1000)

#exponentiating logit results 
exp(cbind(OR = coef(logit1_1000), confint(logit1_1000)))
exp(cbind(OR = coef(logit2_1000), confint(logit2_1000)))
exp(cbind(OR = coef(logit3_1000), confint(logit3_1000)))


x <- data.frame(hitby1000jobs=8)
p<- predict(logit3_1000,x)
p

