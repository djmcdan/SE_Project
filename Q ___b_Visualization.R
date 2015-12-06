#   
#b. Visualizations ( googlevis, ggplot2,  fusion tables)



#Using zip codes provided from transactions map the US new and used cars? 

#install.packages("zipcode")
suppressPackageStartupMessages(library(zipcode))

zipcode_tran <- transactions %>% mutate(zip = zip_bought) %>% select(zip) %>% distinct()
data(zipcode)

milage_bought_new <- milage_bought_new %>% mutate(zip = as.factor(zip_bought))
milage_bought_used <- milage_bought_used %>% mutate(zip = as.factor(zip_bought))

  
new_bought_latlon = merge(milage_bought_new,zipcode, by.x='zip', by.y='zip') 
use_bought_latlon = merge(milage_bought_used,zipcode, by.x='zip', by.y='zip') 



#Googlevis

#Paste the lat and long together for googlevis
new_bought_latlon$LatLon <- paste(round(new_bought_latlon$latitude,2),round(new_bought_latlon$longitude,2),sep=":")
use_bought_latlon$LatLon <- paste(round(use_bought_latlon$latitude,2),round(use_bought_latlon$longitude,2),sep=":")

#Remove any NAs in both
new_bought_latlon_ <- new_bought_latlon[new_bought_latlon$LatLon != "NA:NA",] 
use_bought_latlon_ <- use_bought_latlon[use_bought_latlon$LatLon != "NA:NA",]

#Plot
new_bought_latlon_.plot <- gvisMap(new_bought_latlon_, "LatLon" ,
                                   options=list( showLine=TRUE, enableScrollWheel=TRUE,
                                                 mapType='hybrid', useMapTypeControl=TRUE,
                                                 width=1500,height=800))

plot(new_bought_latlon_.plot)

#Looks like all the new transaction data are based on the east coast buyers but not true if you run freq

table(new_bought_latlon_$state)

#So there maybe an issue with googlevis handling alot of points and same as for the used cars below

use_bought_latlon_.plot <- gvisMap(use_bought_latlon_, "LatLon" ,
              options=list( showLine=TRUE, enableScrollWheel=TRUE,
                           mapType='hybrid', useMapTypeControl=TRUE,
                           width=1500,height=800))

plot(use_bought_latlon_.plot)


#Now will using ggplot  see if this is better

statecount_nbll <- new_bought_latlon_ %>% group_by(state,year_purchased) %>% summarise(n = n ()) %>% mutate(ct = n/100)
statecount_ubll <- use_bought_latlon_ %>% group_by(state,year_purchased) %>% summarise(n = n ()) %>% mutate(ct = n/100)


statecount_nbll$region_ <- state.name[match(statecount_nbll$state,state.abb)]
statecount_nbll$region <- tolower(statecount_nbll$region_)

statecount_ubll$region_ <- state.name[match(statecount_ubll$state,state.abb)]
statecount_ubll$region <- tolower(statecount_ubll$region_)

#New car plot

us <- map_data("state")

gg_nw <- ggplot() + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg_nw <- gg_nw + geom_map(data=statecount_nbll, map=us,
                    aes(fill=n, map_id=region),
                    color="#ffffff", size=0.15) + facet_wrap(~ year_purchased) 
#Want to see them the maps horizontally and in new color

gg_nw <- gg_nw + scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')+ facet_wrap(~ year_purchased, ncol = 1)  + labs(title = "Map of New purchased cars in US")  
gg_nw

#Want to make it look alittle better 
gg_nw <- gg_nw + labs(x=NULL, y=NULL) + coord_map("albers", lat0 = 39, lat1 = 45) + theme(panel.border = element_blank())  + theme(panel.background = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())
gg_nw

#Used car plot
gg_us <- ggplot() + geom_map(data=us, map=us,
                          aes(x=long, y=lat, map_id=region),
                          fill="#ffffff", color="#ffffff", size=0.15)
gg_us <- gg_us + geom_map(data=statecount_ubll, map=us,
                          aes(fill=n, map_id=region),
                          color="#ffffff", size=0.15) + facet_wrap(~ year_purchased) 

#Want to see them the maps horizontally and in new color

gg_us <- gg_us + scale_fill_continuous(low='thistle2', high='darkred', 
                                       guide='colorbar')+ facet_wrap(~ year_purchased, ncol = 1) + labs(title = "Map of Used purchased cars in US")  

gg_us

#Want to make it look alittle better 

gg_us <- gg_us + labs(x=NULL, y=NULL) + coord_map("albers", lat0 = 39, lat1 = 45) + theme(panel.border = element_blank())  + theme(panel.background = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())
gg_us