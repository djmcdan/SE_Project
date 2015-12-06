#d. Objects

setwd("C:/Users/Darius/Box Sync/Classes/Fall 2015/DirectedStudy-SoftwareEng/Assignments/SE_Project") 
source("SE_projectData.R")


#Create prints new cars bought with over 100 miles 
#Create print for the US with visited time catgory

#Create a plot to see the places where new cars over 100 miles are and used less than 100

#Dummy Vars
data <-transactions
Over_Under <-"Under"
milege_parameter <-100
NewUseSelect <- "N"


MilesOver <- function(data,milegeVar,milege_parameter,new_usedVAr, NewUseSelect) {
   x= NewUseSelect
  milesData <- data %>% filter(mileage_bought < milege_parameter & new_usedVAr == x)

   class(milesData) <- "miles"
  return(milesData)
}
newMile <- MilesOver(transactions, transactions$mileage_bought,100,new_usedVAr = transactions$new_or_used_bought,NewUseSelect = "N")



MilesUnder <- function(data,milegeVar,milege_parameter,new_usedVAr, NewUseSelect) {
  x= NewUseSelect
  milesData <- data %>% filter(mileage_bought > milege_parameter & new_usedVAr == x)
  
  class(milesData) <- "miles"
  return(milesData)
}


UsedMile <- MilesUnder(transactions, transactions$mileage_bought,100,new_usedVAr = transactions$new_or_used_bought,NewUseSelect = "U")

class(UsedMile)


plot.homosapien <- function(obj) {
  library(lattice)
  hold <- obj$info
  title <- paste("Price vs. mileage_bought for",obj$name,sep=" ")
  xyplot(price_bought~mileage_bought,data=hold,main=title,type=c("l","g"))
}
plot(john)

#Plot visits less than an hour that purchased cars to the 1-5hr visits?

#First create a function for creating a new class 









#Give a summary of the top 10 cars makes and models bought