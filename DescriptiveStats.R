#######################Descriptive Statistics with aRules###################################

#What percentage had these present
#Basic Amenities - Dry-Cleaning_PL,Elevators_PL,Indoor Corridors_PL,Laundry_PL,Restaurant_PL
requiredRoomData <- data.frame(newJapan_condition_data_3_dec)
View(requiredRoomData)

requiredRoomData[,30] <- newJapan_condition_data_6_dec$newJapan_condition_data_4_dec

satisfyFunction <- function(japan_condition_data_2_dec)
{
  totalAmenitiesSum<-1
  
  #Parse through the data set to check the values against the input parameter values 
  for(i in 1: nrow(japan_condition_data_2_dec))
  {
    if(japan_condition_data_2_dec$Dry.Cleaning_PL[i] == 1 & japan_condition_data_2_dec$Elevators_PL[i] == 1
       & japan_condition_data_2_dec$Indoor.Corridors_PL[i] == 1 & japan_condition_data_2_dec$Laundry_PL[i] == 1
       & japan_condition_data_2_dec$Restaurant_PL[i]==1)
    {
      totalAmenitiesSum<-1+totalAmenitiesSum
    }
  }
  
  #Return the values of the dataset in those indices
  return (totalAmenitiesSum)
}


#How many have the basic facilities of the amenities available in their hotels
AmenitiesPerc<- satisfyFunction(requiredRoomData)/ length(requiredRoomData$All.Suites_PL)
AmenitiesPerc
#It is 74.33%




#SpaFacilites 
totalSpaSum<-1
requiredRoomData$Spa.F.B.offering_PL <- as.numeric(requiredRoomData$Spa.F.B.offering_PL)
requiredRoomData$Spa.services.in.fitness.center_PL <- as.numeric(requiredRoomData$Spa.services.in.fitness.center_PL)
requiredRoomData$Spa.online.booking_PL <- as.numeric(requiredRoomData$Spa.online.booking_PL)



for(i in 1: nrow(requiredRoomData))
{
  if(requiredRoomData$Spa_PL[i] == 1 & requiredRoomData$Spa.services.in.fitness.center_PL[i] == 2
     & requiredRoomData$Spa.online.booking_PL[i] == 2 & requiredRoomData$Spa.F.B.offering_PL[i] == 2)
  {
    totalSpaSum<-1+totalSpaSum
  }
}
totalSpaSum
#Surprisingly - 1


#With Basic facilities one of Spa facilities available
totalspaBasic <- 1
for(i in 1: nrow(requiredRoomData))
{
  
  if(requiredRoomData$Dry.Cleaning_PL[i] == 1 & requiredRoomData$Elevators_PL[i] == 1
        & requiredRoomData$Indoor.Corridors_PL[i] == 1 & requiredRoomData$Laundry_PL[i] == 1
        & requiredRoomData$Restaurant_PL[i]==1)
  {  
  if(requiredRoomData$Spa_PL[i] == 1 | requiredRoomData$Spa.services.in.fitness.center_PL[i] == 2
     | requiredRoomData$Spa.online.booking_PL[i] == 2 | requiredRoomData$Spa.F.B.offering_PL[i] == 2)
  {
    totalspaBasic<- totalspaBasic+1
  }
  }
}
totalspaBasic


SpaBasicPerc <- totalspaBasic/length(requiredRoomData$All.Suites_PL)
SpaBasicPerc
#73.98%



install.packages("arules")
library(arules)
summary(requiredRoomData)

requiredRoomData<- data.frame(lapply(requiredRoomData,as.numeric))

#Forming the transaction matrix
requiredRoomData$V30<-as.factor(requiredRoomData$V30)

room_Cond_matrix<- as(requiredRoomData, "transactions")

#Determining the influence of Spa with the support 
itemFrequencyPlot(room_Cond_matrix,support=0.5, cex.names=0.9)

#Top 20 observations
itemFrequencyPlot(room_Cond_matrix,topN=20)


rulesset<-apriori(requiredRoomData, parameter=list(support=0.5, confidence=0.8, maxlen=3))

#USing the inspect function to observe the clusters of rules and the respective lift, support and confidence values  
inspect(rulesset)



#Set of relations between lhs and rhs

rulesset_2<-apriori(requiredRoomData, parameter=list(support=0.6, confidence=0.8, minlen=20, maxlen=20))
inspect(rulesset_2)


#Checking for the RHS being SPA services
rulesset_3<-apriori(room_Cond_matrix, parameter=list(support=0.5, confidence=0.2, maxlen=10),
appearance = list (default="rhs",lhs='Spa_PL=1'))
inspect(rulesset_3)

rules_conf <- sort (rulesset_3, by="confidence", decreasing=TRUE) # 'high-confidence' rules.

inspect(head(rules_conf))


#Frequent room conditions provided - mostly provided
frequentItems <- eclat (room_Cond_matrix, parameter = list(supp = 0.8,maxlen = 20))
inspect(frequentItems)

itemFrequencyPlot(room_Cond_matrix,topN=20, type="relative", main="Most Provided Facilities")

##2)	Visualize the results

#Load the aRulesVIz library 
install.packages("arulesViz")
library(arulesViz)

#Using the plot function, visualize the above set of rules
plot(rulesset_2)
plot(rulesset_3)

#To pick the good rules out of these, use the lift factor. 
#From the above visualization plot, it can be observed that the most probable lift value lies around 1.49 to 1.5
#Hence, choosing the value to be greater than 1.49   

goodrules <- rulesset[(quality(rulesset_2)$lift > 1.49),]
#Examining this ruleset
sorted_goodrules<- rulesset[(order(quality(rulesset_2)$lift)), ]
inspect(sorted_goodrules)
plot(goodrules)

goodrules_2 <- rulesset[quality(rulesset_2)$lift > 1.5]

#Examining this ruleset
inspect(goodrules_2)
plot(goodrules_2)

