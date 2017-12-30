

### internet data
install.packages("data.table")
library(data.table)
internet_data <- fread("C:/Courses/Fall2017/IST687Project/out-201402.csv", select =  c("Country_PL","Internet_Sat_H", "Internet_Dissat_Lobby_H","Internet_Dissat_Slow_H","Internet_Dissat_Expensive_H","Internet_Dissat_Connectivity_H", "Internet_Dissat_Billing_H", "Internet_Dissat_Wired_H", "Internet_Dissat_Other_H", "TV_Internet_General_H"))
View(internet_data)


#summary(internet_data)

internet_data_country <- internet_data[which(internet_data$Country_PL == "Japan" |
                                               internet_data$Country_PL == "India" |
                                               internet_data$Country_PL == "Egypt"),]
View(internet_data_country)
rowindices_internet<-which(internet_data$Country_PL == "Japan" | 
                             internet_data$Country_PL == "India" |
                             internet_data$Country_PL == "Egypt")
rownames(internet_data_country)<- rowindices_internet

xxx <- c()
new_internet_data <- data.frame(internet_data_country)
rownames(new_internet_data)<- rowindices_internet

for(i in rownames(new_internet_data)) {
  if(sum(new_internet_data[i,3:10] != "") > 0.5*ncol(new_internet_data))
  {
    xxx <- append(xxx,i) 
  }
}
#new_x <- data.frame(xxx,stringsAsFactors = FALSE)
#View(new_x)
y <- as.numeric(unlist(xxx))
y
#str(xxx)
#xxx = as.numeric(xxx)
internet_data_2 = data.frame(internet_data)
internet_data_country_1 <- internet_data_2[y,]
View(internet_data_country_1)

for(i in 1:nrow(internet_data_country_1)) {
  internet_data_country_1[i,1]
  if( is.na(internet_data_country_1[i,2]) == TRUE){
    sum_yes<- sum(internet_data_country_1[i,3:10] == "Yes")
    sum_no <- sum(internet_data_country_1[i,3:10] == "No")
    internet_data_country_1$Internet_Sat_H[i] <- (sum_yes*1.25)+ (sum_no*(-1))
  }
}
View(internet_data_country_1)
round(internet_data_country_1$Internet_Sat_H, digits = 0)
View(internet_data_country_1)

for(i in 1:nrow(internet_data_country_1)) {
  internet_data_country_1$Internet_Sat_H <- replace(internet_data_country_1$Internet_Sat_H, internet_data_country_1$Internet_Sat_H<0, 0)
}
View(internet_data_country_1)
internet_data_country_2 = internet_data_country_1[,-3:-length(internet_data_country_1)]
View(internet_data_country_2)

#rownames(internet_data_country_1) <- xxx[,1]


#cx <- which(xxx[1]== v[1]) 






##consolidation of the data into  required data set

#library(data.table)

#Creating the vector of required columns for overall satisfaction 
#overallSatColumns <- c("Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H","F.B_Overall_Experience_H")
#consolidatedData <- fread("out-201402.csv",select=overallSatColumns)


#Reading the specific columns for Room conditions
roomConditionColumns <- c("All Suites_PL","Bell Staff_PL","Boutique_PL","Business Center_PL",
                          "Casino_PL","Conference_PL","Convention_PL","Dry-Cleaning_PL","Elevators_PL",
                          "Fitness Center_PL","Fitness Trainer_PL","Golf_PL","Indoor Corridors_PL",
                          "Laundry_PL","Limo Service_PL","Mini-Bar_PL","Pool-Indoor_PL","Pool-Outdoor_PL",
                          "Regency Grand Club_PL","Resort_PL","Restaurant_PL","Self-Parking_PL",
                          "Shuttle Service_PL","Ski_PL","Spa_PL","Spa services in fitness center_PL",
                          "Spa online booking_PL","Spa F&B offering_PL","Valet Parking_PL", "Country_PL");

roomConditionData <- fread("C:/Courses/Fall2017/IST687Project/out-201402.csv",select=roomConditionColumns)

View(roomConditionData)

#for(newCol in colnames(roomConditionData))
#{
#  lapply(roomConditionData$newCol, as.numeric)  
#}

#roomConditionData$roomCondition<- NA


#predictionModel<-lm(formula=roomConditionData$roomCondition~., data=roomConditionData)
#Priority_1
#
japan_condition_data <- roomConditionData[which(roomConditionData$Country_PL == "Japan" |
                                                  roomConditionData$Country_PL == "India" |
                                                  roomConditionData$Country_PL == "Egypt"),]
rowindices_room<-which(roomConditionData$Country_PL == "Japan" | 
                         roomConditionData$Country_PL == "India" |
                         roomConditionData$Country_PL == "Egypt")
rownames(japan_condition_data)<- rowindices_room
View(japan_condition_data)

#Read the specific columns from the data
#temp <- japan_condition_data
#View(temp)
#length(temp)
japan_condition_data_2 <- data.frame(japan_condition_data)
for(j in rownames(japan_condition_data_2))
{
  japan_condition_data_2[j,]<- as.numeric(japan_condition_data_2[j,]=='Y', na.rm=TRUE)
  
}
View(japan_condition_data_2)
rownames(japan_condition_data_2)<- rowindices_room


numOfCols <- ncol(japan_condition_data_2)
#View(temp)
#numOfCols
halfCols <- round(numOfCols/2)
#temp<-temp[,-31]
#View(temp)
retainRows<- c()
count<-1
#View(temp)
for(eachRow in rownames(japan_condition_data_2))#1:nrow
{
  #eachRow
  if(sum(japan_condition_data_2[eachRow,]==1, na.rm = TRUE )> 2)
  {
    
    #append(retainRows,eachRow) 
    #    retainRows <- append(retainRows,eachRow) 
    
    #  eachRow
    retainRows[count]<- eachRow
    count<-count+1
  }
}
retainRows
z <- as.numeric(unlist(retainRows))
z
View(retainRows)
## rownumber - 522631
## count - 84769
View(japan_condition_data_2)



install.packages("data.table")
library(data.table)
#df<-fread("D:/desktop files/sem_1_ischool/ist 687/out-201402.csv", select=c('Customer_SVC_H','Internet_Sat_H','Guest_Room_H'))
#View(df)


df1<-fread("C:/Courses/Fall2017/IST687Project/out-201402.csv", select=c('F&B_FREQ_H','Spa F&B offering_PL','F&B_Overall_Experience_H', 'Country_PL'))
View(df1)
japan_food_data <- df1[which(df1$Country_PL == "Japan" |
                               df1$Country_PL == "India" |
                               df1$Country_PL == "Egypt"),]
View(japan_food_data)
rowindices<-which(df1$Country_PL == "Japan" |
                    df1$Country_PL == "India" |
                    df1$Country_PL == "Egypt")
rownames(japan_food_data)<- rowindices

v<-c()
#df1
count<-1
for(i in rowindices)
{
  if((is.na(japan_food_data[i,1])==FALSE) & (is.na(japan_food_data[i,2])==FALSE))
  {
    v[count]<-i
    count<- count+1
  }
}
length(v)
class(v)
#v <- data.frame(v)
#View(v)
df2<- japan_food_data[v,]
View(df2)
row.names(df2)<-v
typeof(df2)

#df4<- US_fb_city_data[v,]
#View(df4)
#row.names(df4)<-v

#row.names(df2)<-v

#yyy <- c()
#count<-1
#df
#for(i in rownames(df2)) {
# if((df2[i,2] != ""))
#  {
#    yyy[count] <- i
#   count<-count+1
# }
#}

#View(v)
#v3<-c()
#count<-1
Newdf<- data.frame(df2)
View(Newdf)
rownames(Newdf) <-rownames(df2)









#integration of sanjay's and sandya-shama's work

#The indices of sandya's shama's part

View(v)

#The indices of Sanjay's part
View(xxx)
i=1
j=1
sandhya <-rownames(Newdf)
new_new <- merge(xxx, sandhya, by=c("xxx[1]"="sandhya[1]"), all=TRUE)



x <- as.numeric(unlist(xxx))
y <- as.numeric(unlist(sandhya))

common <- intersect(x,v)
new_common <- intersect(common,z)
new_common



