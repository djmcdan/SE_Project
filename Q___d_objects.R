#d. Objects

setwd("C:/Users/Darius/Box Sync/Classes/Fall 2015/DirectedStudy-SoftwareEng/Assignments/SE_Project") 
source("SE_projectData.R")


#Create a plot to see the all the new cars bought over certian milage with the price and also used


#First  wanna see used cars under 100 miles and price
# Now Used cars




NewMilesOver100 <- transactions %>% filter(mileage_bought >100 & new_or_used_bought == "N")
UsedMilesUnder100 <- transactions %>% filter(mileage_bought < 100 & new_or_used_bought == "U")

cardata <- function(m,x,y,z) {
  Edcars <- list(title = m, id=x, source=y, data=z)
  class(Edcars) <- "cars"
  return(Edcars)
}

PurchasedNewOver100 <- cardata("New Cars Purchased with Miles Over 100 in","US","http://www.edmunds.com",NewMilesOver100)
PurchasedUsedUnder100 <- cardata("Used Cars Purchased with Miles Under 100 in","US","http://www.edmunds.com",UsedMilesUnder100)

print.cars <- function(object) {
  header <- paste("Data:",object$title,"Country:",object$id,"SOURCE:",object$source,sep=" ")
  cat(header,"\n\n")
  str(object$data,0)
}


print(PurchasedNewOver100)
print(PurchasedUsedUnder100)

df=transactions
#Want to see all all the people who bought cars over or under 100 miles new and used


plot.cars <- function(object,...) {
   df <- object$data

  
  zipcode_tran <- df %>% mutate(zip = zip_bought) %>% select(zip) %>% distinct()
  library(zipcode)
  data(zipcode)
  
  dfout <- df %>% mutate(zip = as.factor(zip_bought),year_purchased = as.numeric( format( as.Date( date_sold, origin='1900-1-1'), '%Y')))
  
  dfout_latlon = merge(dfout,zipcode, by.x='zip', by.y='zip') 

  
  p <- qmplot(longitude, latitude, data = dfout_latlon, colour = I("red"),size = I(2), darken = .1) + facet_wrap(~ year_purchased, ncol = 1) +  expand_limits() + theme_minimal()

  
  plot(p)
  }


plot(PurchasedNewOver100)

plot(PurchasedUsedUnder100)




summary.cars <- function(object) {
 
  df <- object$data
   sum <- df %>% summarise(mean_price_purchased = mean(price_bought , na.rm = TRUE),lowest_price_purchased = min(price_bought , na.rm = TRUE),highest_price_purchased = max(price_bought , na.rm = TRUE) )
     
  print(sum)
}
summary(PurchasedNewOver100)

summary(PurchasedUsedUnder100)


