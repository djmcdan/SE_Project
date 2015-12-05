#c. SQL (sqldf / Rsqlite)


#Create a SQLite database and figure out how many purchases where classified as Trucks, SUVs and cars , and the popular years for makers using different queries?



install.packages('sqldf')
library(sqldf)

#Lets see how many new cars where bought by year

sqldf("select model_year_bought, count(*) from new_bought_latlon group by model_year_bought")

#This shows us that there are 52 different body types

sqldf("select bodytype_bought, count(*) as freq from new_bought_latlon group by bodytype_bought order by freq desc")
sqldf("select bodytype_bought, count(*) as freq from use_bought_latlon group by bodytype_bought order by freq desc")

#How many new and used purchases by makes

sqldf("select make_bought, count(*) as freq from new_bought_latlon group by make_bought order by freq desc")

sqldf("select make_bought, count(*) as freq from use_bought_latlon group by make_bought order by freq desc")

#What was the average, min and max mileages bought along with the average, min and max price bought 
#New
mb_avs_nw <- sqldf("select make_bought, count(*) as freq, min(price_bought), avg(price_bought), max(price_bought),min(mileage_bought),  avg(mileage_bought), max(mileage_bought)  
      from new_bought_latlon group by make_bought order by freq desc")
#Used
mb_avs_us <- sqldf("select make_bought, count(*) as freq, min(price_bought), avg(price_bought), max(price_bought),min(mileage_bought),  avg(mileage_bought), max(mileage_bought)  
      from use_bought_latlon group by make_bought order by freq desc")
mb_avs_us

#What are the makes that had more than 3000 new purhases

sqldf("select * from mb_avs_nw where freq > 3000 order by freq desc")

#What are the makes that had more than 600 new purhases
sqldf("select * from mb_avs_us where freq > 600 order by freq desc")





#Honda and Toyota both have the most purchases for new and used



#So we can see thather are outlier that have high mileage in the 20000s

head(visitor,5)


SELECT last_name, first_name, avg(salary) AS average_salary, 
count(*) AS years_worked 
FROM employees 
GROUP BY last_name, first_name 
HAVING count(*) > 2 
ORDER BY avg(salary) DESC;




