
##a. Dplyr

#Based on the visitor data how long did the visits last and did the longer visit times lead to purchases or vis versa?

setwd("C:/Users/Darius/Box Sync/Classes/Fall 2015/DirectedStudy-SoftwareEng/Assignments/SE_Project") 
source("SE_projectData.R")



visitor_time <- visitor %>% 
  mutate(new_dwell_time_mins = new_dwell_time/60, used_dwell_time_mins = used_dwell_time/60 , tot_dwell_time_mins =   tot_dwell_time/60 , tot_dwell_time_hours = tot_dwell_time_mins/60)%>% 
  select(visitor_key, tot_dwell_time, tot_dwell_time_mins,tot_dwell_time_hours, zip)


sum_visitor_time <- visitor_time %>% group_by(visitor_key)%>% summarise(hours_sum_time = sum(tot_dwell_time_hours, na.rm = TRUE))

#What was the average,min and max time spent on the site for all visits

mean_med_visitor_time <-sum_visitor_time %>% summarise(mean_hours_visited = mean(hours_sum_time , na.rm = TRUE),median_hours_visited = median(hours_sum_time , na.rm = TRUE))

mean_med_visitor_time


#Now for those that purchased new or used car what was the avg time visited?

new_bought <- transactions  %>% filter(new_or_used_bought == "N") %>% select(visitor_key, date_sold, new_or_used_bought, price_bought,model_bought,make_bought,model_year_bought,mileage_bought,bodytype_bought)%>% distinct()
used_bought <- transactions %>% filter(new_or_used_bought == "U") %>% select(visitor_key, date_sold, new_or_used_bought, price_bought,model_bought,make_bought,model_year_bought,mileage_bought,bodytype_bought)%>% distinct()



visitor_purchased_new <- sum_visitor_time %>%inner_join(new_bought)%>% distinct()
visitor_purchased_used <- sum_visitor_time %>%inner_join(used_bought)%>% distinct()

avgs_visitor_time_purchaseNw <-visitor_purchased_new %>% summarise(min_hours_visited = min(hours_sum_time , na.rm = TRUE), mean_hours_visited = mean(hours_sum_time , na.rm = TRUE),med_hours_visited = median(hours_sum_time , na.rm = TRUE), max_hours_visited = max(hours_sum_time , na.rm = TRUE))
avgs_visitor_time_purchaseUs <-visitor_purchased_used %>% summarise(min_hours_visited = min(hours_sum_time , na.rm = TRUE), mean_hours_visited = mean(hours_sum_time , na.rm = TRUE),med_hours_visited = median(hours_sum_time , na.rm = TRUE), max_hours_visited = max(hours_sum_time , na.rm = TRUE))

avgs_visitor_time_purchaseNw
avgs_visitor_time_purchaseUs



#How many purchases were  >1hr, 1-5, 5-10,10-20, gt20 hrs

new_bought_cat <- visitor_purchased_new %>% mutate(time_hours_cat = ifelse(hours_sum_time < 1, 1, ifelse(hours_sum_time >= 1 & hours_sum_time < 5,2,
                                                                                                               ifelse(hours_sum_time >= 5 & hours_sum_time < 10,3,   
                                                                                                                      ifelse(hours_sum_time >= 10 & hours_sum_time < 20,4,
                                                                                                                             ifelse(hours_sum_time >= 20 & hours_sum_time < 48,5,
                                                                                                                                    ifelse(hours_sum_time >= 48 & hours_sum_time < 72,6,
                                                                                                                                           ifelse(hours_sum_time >72,7,NA)))))))) 
new_bought_cat$time_hours_cat<- factor(new_bought_cat$time_hours_cat, levels = c(1,2,3,4,5,6,7), labels = c(">1hr", " 1-5hrs", " 5-10hrs","10-20hrs","20-48hrs", "48-72hrs", ">72hrs")) 


new_bought_cat$hours_sum_time_ <- round(new_bought_cat$hours_sum_time , 2)


used_bought_cat <- visitor_purchased_used %>% mutate(time_hours_cat = ifelse(hours_sum_time < 1, 1, ifelse(hours_sum_time >= 1 & hours_sum_time < 5,2,
                                                                                                                 ifelse(hours_sum_time >= 5 & hours_sum_time < 10,3,   
                                                                                                                        ifelse(hours_sum_time >= 10 & hours_sum_time < 20,4,
                                                                                                                               ifelse(hours_sum_time >= 20 & hours_sum_time < 48,5,
                                                                                                                                      ifelse(hours_sum_time >= 48 & hours_sum_time < 72,6,
                                                                                                                                             ifelse(hours_sum_time >72,7,NA)))))))) 


used_bought_cat$time_hours_cat<- factor(used_bought_cat$time_hours_cat, levels = c(1,2,3,4,5,6,7), labels = c(">1hr", " 1-5hrs", " 5-10hrs","10-20hrs","20-48hrs", "48-72hrs", ">72hrs")) 

used_bought_cat$hours_sum_time_ <- round(used_bought_cat$hours_sum_time , 2)

table(new_bought_cat$time_hours_cat )
table(used_bought_cat$time_hours_cat )

histogram(new_bought_cat$time_hours_cat)
histogram(used_bought_cat$time_hours_cat)

#Observation Over 50percent of purchases that happened within an hour of visiting the site for new and used cars

#Of the new and used purchases that visited the site how many where truly new and used based on milage?

#How many new cars had less than 100 miles on them and greater than 100 miles?

milage_bought_new <- new_bought_cat %>% mutate(mileage_cat100 <-ifelse(mileage_bought <= 100, 1, ifelse(mileage_bought > 100,2,0))) 
  table(milage_bought_new$mileage_cat100 )
  
milage_bought_new$year_purchased <- as.numeric( format( as.Date( milage_bought_new$date_sold, origin='1900-1-1'), '%Y'))

 
#How many used cars had less than 100 miles on them and greater than 100 miles?

milage_bought_used <- used_bought_cat %>% mutate(mileage_cat100 <-ifelse(mileage_bought <= 100, 1, ifelse(mileage_bought > 100,2,0))) 
 table(milage_bought_used$mileage_cat100 )

 milage_bought_used$year_purchased <- as.numeric( format( as.Date( milage_bought_used$date_sold, origin='1900-1-1'), '%Y'))
 
#b. Will dig deeper into the people who bought a car with over 100 miles on it and also who bought a used car with less than 100 miles?