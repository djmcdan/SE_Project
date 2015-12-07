
#JSON/XML

#e.   Create a data frame based on top models bought in each year and pull important consummer rating data?

library(RJSONIO)
library(RCurl)

#First what are the top 5 makes bought

dbGetQuery(db,"select make_bought, count(*) as freq,model_bought,model_year_bought from Transactions WHERE new_or_used_bought = 'N'  GROUP BY make_bought,model_bought  order by freq desc LIMIT 5")

# Honda has the top four purchased. The new models are 2015 Accord, 2014 CR-V, 2015 Civic and 2015 Odyssey.
# followed by Suburu Forester 2015.


dbGetQuery(db,"select make_bought, count(*) as freq,model_bought,model_year_bought from Transactions WHERE new_or_used_bought = 'U'  GROUP BY make_bought,model_bought  order by freq desc LIMIT 5")

# Honda has the top three used purchased. The use models are 2011 Accord, 2014 CR-V and 2011 Civic.
# followed by 2014 BMW 3 Series and 2011 Toyota Camry.


#So what are there ratings and reviews and price. 


#Edmonds API key  d2d4xfrkywhw6dmmh55qjuma

#NEW top 5 purchased
api<- paste("https://api.edmunds.com/api/vehiclereviews/v2/honda/accord/2015?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
api<- paste("https://api.edmunds.com/api/vehiclereviews/v2/honda/cr-v/2014?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
api<- paste("https://api.edmunds.com/api/vehiclereviews/v2/honda/civic/2015?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
api<- paste("https://api.edmunds.com/api/vehiclereviews/v2/honda/odyssey/2015?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")

#Useds top 5 purchased
api<- paste("https://api.edmunds.com/api/vehicle/v2/honda/accord/2014?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")


addr <- getURL(api)

url.json <- fromJSON(addr)

str(url.json,1)
str(url.json,2)
str(url.json,3)
str(url.json,4)
str(url.json,5)


HondaOdyssey2014Ratings <- unlist(url.json$averageRating[1]) 


unlist(url.json$location[5]) -> STATE
unlist(url.json$location[4]) -> COUNTRY

HondaAccord2015Ratings <- unlist(url.json$averageRating[1]) 
HondaCRV2014Ratings <- unlist(url.json$averageRating[1]) 
HondaCivic2015Ratings <- unlist(url.json$averageRating[1]) 
HondaOdyssey2015Ratings <- unlist(url.json$averageRating[1]) 


#Create a function that gives consumer data ratings for cars pulling from Edmonds

cars <- function(id,make,model,year) {
  
  library(RJSONIO)
  library(RCurl)
  
  #Main part of api
  api_first<- paste("https://api.edmunds.com/api/vehiclereviews/v2/",sep="")
  
  
  make_mod_yr <- paste(make,model,year,sep = "/")


  api_last <- paste("?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
  url <- paste(api_first,make_mod_yr,api_last,sep = "")
  addr <- getURL(url)
  
   
  
   
  #JSON parsed
  url.json <- fromJSON(addr)
  
  #Used to count for correct parsing
  #str(url.json,2)
  
  Ratings <- unlist(url.json$averageRating[1])
  

  #This gives us all personal weather stations data in area specified by lat and lon
  df <- rbind(id, make, model, year, Ratings )
  df_ <- data.frame(df)
  df_n <- t(df_)
  df_n_ <- data.frame(df_n)
  
  
    #This returns url, location and station data for lat lon
  return(df_n_)
  
  
}

cars(id=1,make="honda",model= "odyssey", year = 2015) ->t1
cars(id=2,make="honda",model= "odyssey", year = 2015) ->t2
cars(id=3,make="honda",model= "odyssey", year = 2015) ->t3
cars(id=4,make="honda",model= "odyssey", year = 2015) ->t4
cars(id=5,make="honda",model= "odyssey", year = 2015) ->t4


#QA

myPWS(lat=-23.533333,lon=-46.616666,dist=3)   # Sao Paulo Brazil
myPWS(lat=48.86666,lon=2.333333,dist=1)  # Paris, France
myPWS(lat=42.3313889,lon=-83.045833,dist=5) # Detroit, Michigan
myPWS(lat=40.714166,lon=-74.00638,dist=5) # New York, New York





