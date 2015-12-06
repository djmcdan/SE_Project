#d. Objects

setwd("C:/Users/Darius/Box Sync/Classes/Fall 2015/DirectedStudy-SoftwareEng/Assignments/SE_Project") 
source("SE_projectData.R")


#Create prints new cars bought with over 100 miles 
#Create print for the US with visited time catgory

#Create a plot to see the all the new cars bought over certian milage with the price and also used


#First  wanna see used cars under 100 miles and price
# Now Used cars



MilesOver <- function(data,milegeVar,milege_parameter,new_usedVAr, NewUseSelect) {
   x= NewUseSelect
  milesData <- data %>% filter(mileage_bought > milege_parameter & new_usedVAr == x)
  milesData$Over_Under = "Over"
   class(milesData) <- "miles"
  return(milesData)
}
newMile <- MilesOver(transactions, transactions$mileage_bought,100,new_usedVAr = transactions$new_or_used_bought,NewUseSelect = "N")



data = transactions
new_usedVAr = transactions$new_or_used_bought
milegeVar = data$mileage_bought
NewUseSelect = "U"
milege_parameter = 100
Over_Under = "Under"


Miles <- function(data,milegeVar,milege_parameter,Over_Under,new_usedVAr, NewUseSelect) {
  
  new_usedVAr = data$new_or_used_bought 

  data$new_usedVAr_[new_usedVAr == "U" ] <- "Used"
  data$new_usedVAr_[new_usedVAr == "N" ] <- "New"
  
  data$Over_Under = Over_Under
  
  data$Over_UnderMile[data$Over_Under == "Under" & milegeVar < milege_parameter ] <- "Under"
  data$Over_UnderMile[data$Over_Under == "Over" & milegeVar > milege_parameter ] <- "Over"
  
  milesData <- data %>% filter(Over_Under == Over_UnderMile & new_usedVAr == NewUseSelect )
  
  
 
  
  class(milesData) <- "miles"
  return(milesData)
}


UsedMile <- Miles(transactions,transactions$mileage_bought,100,"Under",new_or_used_bought,"U")

class(UsedMile)

xyplot(price_bought~mileage_bought,data=UsedMile,type=c("l","g"))
z <- transactions %>% mutate(mileage_bought_ = round(mileage_bought,2),price_bought_ = round(price_bought,2)) 

ggplot(z,aes(x=mileage_bought_, y=price_bought)) + geom_line() + ylim(0, max(z$price_bought))

plot.Miles <- function(obj) {
  library(lattice)
  db <- UsedMile
  title <- paste("Price vs. Mileage for",db$new_usedVAr_,sep=" ")
   pm<- xyplot(db$mileage_bought~db$price_bought,data=db,main=title,type=c("l","g"))
   plot(pm)
}
plot(UsedMile)



#Plot visits less than an hour that purchased cars to the 1-5hr visits?

#First create a function for creating a new class 









#Give a summary of the top 10 cars makes and models bought










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
