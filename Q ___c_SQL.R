#c. SQL (sqldf / Rsqlite)
setwd("C:/Users/Darius/Documents/DF2015_Data/DF2015_revised") 

#Create a SQLite database and figure out how many purchases where classified as Trucks, SUVs and cars , and the popular years for makers using different queries?


library(sqldf)
library(tcltk)
require(RSQLite)


db <- dbConnect(SQLite(), dbname="Edmonds.sqlite")





#Below write the different dataset into sql tables

dbWriteTable(conn = db, name = "Visitor", value = "visitor.csv",
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "Transactions", value = "transactions.csv",
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "Configuration", value = "configuration.csv",
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "Leads", value = "leads.csv",
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "Shopping", value = "shopping.csv",
             row.names = FALSE, header = TRUE)


# The tables in the database
dbListTables(db)     

# The columns in Visitor and Transactions
dbListFields(db, "Visitor")         
dbListFields(db, "Transactions")  


      
#Lets see how many new cars where bought by manufactuer year
dbGetQuery(db,"SELECT model_year_bought as Model_year_purchased_New, count(*) as modelYR_freq from Transactions  WHERE new_or_used_bought = 'N' GROUP BY model_year_bought")

dbGetQuery(db,"SELECT model_year_bought as Model_year_purchased_Used, count(*) as modelYR_freq from Transactions  WHERE new_or_used_bought = 'U' GROUP BY model_year_bought")


#How many different body types were purchased new and used displaying only the top 10

dbGetQuery(db,"select bodytype_bought, count(*) as freq from Transactions WHERE new_or_used_bought = 'N' group by bodytype_bought order by freq desc")
dbGetQuery(db,"select bodytype_bought, count(*) as freq from Transactions WHERE new_or_used_bought = 'U' group by bodytype_bought order by freq desc")

#What are the top five makers that sold the most new and used cars 

dbGetQuery(db,"select make_bought, count(*) as freq from Transactions WHERE new_or_used_bought = 'N'  GROUP BY make_bought  order by freq desc LIMIT 5")

dbGetQuery(db,"select make_bought, count(*) as freq from Transactions WHERE new_or_used_bought = 'U' GROUP BY make_bought order by freq desc LIMIT 5")

#Honda and Toyota both have the most purchases for new and used


#What was the average, min and max mileages bought along with the average, min and max price bought for the top ten

#New
dbGetQuery(db,"select make_bought, count(*) as freq, min(price_bought), avg(price_bought), max(price_bought),min(mileage_bought),  avg(mileage_bought), max(mileage_bought)  
      from Transactions WHERE new_or_used_bought = 'N'  GROUP BY make_bought order by freq desc LIMIT 10")
#Used
dbGetQuery(db,"select make_bought, count(*) as freq, min(price_bought), avg(price_bought), max(price_bought),min(mileage_bought),  avg(mileage_bought), max(mileage_bought)  
      from Transactions WHERE new_or_used_bought = 'U'  GROUP BY make_bought order by freq desc LIMIT 10")



#Honda and Toyota both have the most purchases for new and used

