#Edmonds data exploration

#These packages must be called to do below programs
library(plyr)
library(dplyr)
library(DescTools)
library(lubridate)# Makes parting dates easier
library(ggplot2)
library(mosaic)


setwd("C:/Users/Darius/Documents/DF2015_Data/DF2015_revised") 


visitor <- tbl_df(read.csv("visitor.csv", stringsAsFactors = FALSE))
transactions <- tbl_df(read.csv("transactions.csv", stringsAsFactors = FALSE))
shopping <- tbl_df(read.csv("shopping.csv", stringsAsFactors = FALSE))
leads <- tbl_df(read.csv("leads.csv", stringsAsFactors = FALSE))
configuration <- tbl_df(read.csv("configuration.csv", stringsAsFactors = FALSE))



#Note to self 
#1.Come up with some questions( look up common questiions asked about edmonds data)  
                          #---(look into how many leads lead to purchases...how many look at new cars vs used)
                          #---Most looked up cars in the country and reagions mapped 
                          #---Look at transactions 
                          #---Look at configurations 
                          #---Run basic states and get rid of all duplicates in data
#2.Clean data
#3.Blend data
#4. Graph and plots
#5. Find key variables




names(configuration)#The visit key is the unique ID
names(leads)
names(shopping)
names(transactions)

#The visit key is the unique ID
names(visitor)



#How many different models and makes did visitors look at and what where the popular years?

#configuration_ <- configuration[complete.cases(configuration),]  # Filter out incomplete cases

configuration_ <- configuration %>%  
  filter(!is.na(visitor_key)) %>% summarise(
      n = n(vehicle_make))%>% select(vehicle_make)
 # group_by(date, hour) %>%

table(configuration$vehicle_make)
table(configuration$vehicle_model)  
table(configuration$vehicle_model_year)  #from 2012 to 2016


configuration_yearCT <- as.data.frame(table(configuration$vehicle_model_year))


m1 <- ggplot(data=configuration_yearCT,aes(x=Var1,y=Freq)) 
m1 + geom_bar(stat="identity") + ggtitle("Model Years")


#Want to see the counts for models and makes

#models
counts <- rev(sort(table(configuration$vehicle_model)))
modelsdf <- as.data.frame(counts)
modelsdf$models <- rownames(modelsdf)

#makes
counts <- rev(sort(table(configuration$vehicle_make)))
makesdf <- as.data.frame(counts)
makesdf$makes <- rownames(makesdf)


modelsdf$models <- factor(modelsdf$models, levels=names(counts))
makesdf$makes <- factor(makesdf$makes, levels=names(counts))


# Plot the top 15 makes and models


models1 <- ggplot(modelsdf[1:15,],aes(x=models,y=counts))
models1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

makes1 <- ggplot(makesdf[1:15,],aes(x=makes,y=counts))
makes1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#How many of the top 15 where looking at used vs bought new
#How many of the top 15 where bought used vs bought new

#first merge vistor data by visitor_key but select only new_flag and used_flag then categorizeS

visitor_new_used <- visitor %>% select(visitor_key, new_flag ,used_flag, zip, age_range)
  
  # There are three categories search new only, used only or both new and used

visitor_new_used$new_used_both[visitor_new_used$new_flag == 1 & visitor_new_used$used_flag ==0 ] <- 1 
visitor_new_used$new_used_both[visitor_new_used$new_flag == 1 & visitor_new_used$used_flag ==1 ] <- 2
visitor_new_used$new_used_both[visitor_new_used$new_flag == 0 & visitor_new_used$used_flag ==1 ] <- 0

table(visitor_new_used$new_used_both)

config_makemodel <- configuration %>% select(visitor_key, vehicle_make,vehicle_model)%>% distinct()

transactions_bought <- transactions %>% select(visitor_key, date_sold, new_or_used_bought, price_bought,model_bought,make_bought)

visitor_NUB_config <- visitor_new_used %>% select(visitor_key, new_used_both) %>%inner_join(config_makemodel) %>% distinct()
names(visitor_NUB_config)

visitor_NUB_trans <- visitor_new_used %>% select(visitor_key, new_used_both) %>%inner_join(transactions_bought)%>% distinct()
names(visitor_NUB_trans)


visitor_NUB_trans_config_ <- visitor_new_used %>% select(visitor_key, new_used_both) %>%inner_join(config_makemodel)%>% distinct() %>%inner_join(transactions_bought)%>% distinct()
names(visitor_NUB_trans_config_)




#1.Now back to question how many looked top 15 based on used or new makes
# a. of the top 3 what are the models
#2.Now back to question how many bought used and new of the top 15 makes looked at



#makes
counts <- rev(sort(table(visitor_NUB_config$vehicle_make)))
visitor_NUB_configdf <- as.data.frame(counts)
visitor_NUB_configdf$makes <- rownames(visitor_NUB_configdf)

counts <- rev(sort(table(visitor_NUB_trans$make_bought)))
visitor_NUB_transdf <- as.data.frame(counts)
visitor_NUB_transdf$make_bought <- rownames(visitor_NUB_transdf)


visitor_NUB_configdf$makes <- factor(visitor_NUB_configdf$makes, levels=names(counts))
visitor_NUB_transdf$makes_bought <- factor(visitor_NUB_transdf$make_bought, levels=names(counts))



# Plot the top 15 makes 

visitor_NUB_configdf1 <- ggplot(visitor_NUB_configdf[1:15,],aes(x=makes,y=counts))
visitor_NUB_configdf1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

visitor_NUB_transdf1 <- ggplot(visitor_NUB_transdf[1:15,],aes(x=makes_bought,y=counts))
visitor_NUB_transdf1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))








v2isitor_NUB_configdf <- as.data.frame(table(visitor_NUB_config$vehicle_make,visitor_NUB_config$new_used_both))
names(v2isitor_NUB_configdf) <- c("Make","New_USED_both","Count")



v2isitor_NUB_transdfdf <- as.data.frame(table(visitor_NUB_trans$make_bought,visitor_NUB_trans$new_or_used_bought))
names(v2isitor_NUB_transdfdf) <- c("Make","new_or_used_bought","Count")

p <- ggplot(data=v2isitor_NUB_configdf[1:15,],aes(x=Var1,y=Freq,fill=Var2))
p + geom_bar(stat="identity") + ggtitle("Visits for makes New vs Used VS both")


p <- ggplot(data=v2isitor_NUB_transdfdf[1:15,],aes(x=Make,y=Count,fill=new_or_used_bought))
p + geom_bar(stat="identity") + ggtitle("Bought New vs Used VS both")

ggplot(data=v2isitor_NUB_transdfdf[1:15,], aes(x=Make,y=Count,fill=new_or_used_bought)) + geom_bar()

