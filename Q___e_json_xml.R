
#JSON/XML

#e.   Create a data frame based on top models bought in each year and pull important consummer rating data?

setwd("C:/Users/Darius/Documents/DF2015_Data/DF2015_revised") 
library(RJSONIO)
library(RCurl)
library(dplyr)

db <- dbConnect(SQLite(), dbname="Edmonds.sqlite")

#First what are the top 5 makes bought

dbGetQuery(db,"select make_bought, count(*) as freq,model_bought,model_year_bought from Transactions WHERE new_or_used_bought = 'N'  GROUP BY make_bought,model_bought  order by freq desc LIMIT 5")

# Honda has the top four purchased. The new models are 2015 Accord, 2014 CR-V, 2015 Civic and 2015 Odyssey.
# followed by Suburu Forester 2015.


New_data<- dbGetQuery(db,"select make_bought as make, count(*) as freq,model_bought as model,model_year_bought as year 
                      from Transactions WHERE new_or_used_bought = 'N'  GROUP BY make_bought,model_bought  order by freq desc LIMIT 10") %>% 
  mutate(id = as.character(rownames(New_data),levels=rownames(New_data)))


dbGetQuery(db,"select make_bought, count(*) as freq,model_bought,model_year_bought from Transactions WHERE new_or_used_bought = 'U'  GROUP BY make_bought,model_bought  order by freq desc LIMIT 5")

# Honda has the top three used purchased. The use models are 2011 Accord, 2014 CR-V and 2011 Civic.
# followed by 2014 BMW 3 Series and 2011 Toyota Camry.


#So what are there ratings and reviews and price. 


#Edmonds API key  d2d4xfrkywhw6dmmh55qjuma


#Create a function that gives consumer data ratings for cars pulling from Edmonds

cars <- function(id,make,model,year) {
  
  library(RJSONIO)
  library(RCurl)
  
  #Main part of api
  api_first<- paste("https://api.edmunds.com/api/vehiclereviews/v2/",sep="")
  
  makeLow <-tolower(make)
  modelLow <-tolower(model)
  yearLow <-tolower(year)
  make_mod_yr <- paste(makeLow,modelLow,yearLow,sep = "/")
  
  
  api_last <- paste("?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
  url <- paste(api_first,make_mod_yr,api_last,sep = "")
  addr <- getURL(url)
  
  
  
  
  #JSON parsed
  url.json <- fromJSON(addr)
  
  #Used to count for correct parsing
  str(url.json,2)
  
  Ratings <- unlist(url.json$averageRating[1])
  
  
  #This gives us all personal weather stations data in area specified by lat and lon
  df_n_ = data.frame(id, makeLow, modelLow, yearLow, Ratings )
  #This returns url, location and station data for lat lon
  return(df_n_)
  
  
}


#NEW top 5 purchased


cars(id=1,make="HONDA",model= "acCord", year = 2015) ->t1
cars(id=2,make="honDa",model= "Cr-v", year = 2014) ->t2
cars(id=3,make="honda",model= "civic", year = 2015) ->t3
cars(id=4,make="honda",model= "odyssey", year = 2015) ->t4
cars(id=5,make="Subaru",model= "forester", year = 2015) ->t5


#Used top 5 purchased





#This is nice but I want to just add this rating to my data sent of all makes and models

carsdf <- function(df) {
  
  df_ <- df
  # df<-New_data
  
  #Main part of api
  df$api_first<- paste("https://api.edmunds.com/api/vehiclereviews/v2/",sep="")
  
  df$makeLow <-tolower(df$make)
  df$modelLow <-tolower(df$model)
  df$yearLow <-tolower(df$year)
  
  df$make_mod_yr <- paste(df$makeLow,df$modelLow,df$yearLow,sep = "/") 
  
  df$api_last <- paste("?fmt=json&api_key=akw86gbvscxm4szkb3t53rnw",sep="")
  df$url <- paste(df$api_first,df$make_mod_yr,df$api_last,sep = "")
  #     df$urladd <- paste(url[ii],sep = "")
  
  url<-df$url
  
  
  
  
  addr <- getURL(url) 
  
  masterlist <- list() # Blank master list
  jj = 1 # Counter for incrementing the masterlist
  
  for (ii in 1:length(addr)) { 
    #JSON parsed
    url.json <- fromJSON(addr[ii])
    
    #Used to count for correct parsing
    str(url.json,2)
    
    # df$Ratings <- unlist(url.json$averageRating[1])
    
    
    Ratings <- unlist(url.json$averageRating[1])
    # vehic <- unlist(url.json$links$rel)
    
    masterlist[[jj]] <- data.frame(Ratings)
    jj = jj + 1
  } 
  
  car_rating <-  do.call(rbind,masterlist) %>% mutate(id = as.character(rownames(New_data),levels=rownames(New_data)))
  
  car_rating_ <-df_ %>% inner_join(car_rating)
  
  
  return(car_rating_)
}



carsdf(New_data) -> c1






dput(New_data[1:5,])
