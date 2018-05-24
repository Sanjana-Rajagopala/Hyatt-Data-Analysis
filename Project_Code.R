

### Read the data containing 'internet' details
install.packages("data.table")
library(data.table)
internet_data <- fread("C:/Courses/Fall2017/IST687Project/out-201402.csv", select =  c("Country_PL","Internet_Sat_H", "Internet_Dissat_Lobby_H","Internet_Dissat_Slow_H","Internet_Dissat_Expensive_H","Internet_Dissat_Connectivity_H", "Internet_Dissat_Billing_H", "Internet_Dissat_Wired_H", "Internet_Dissat_Other_H", "TV_Internet_General_H"))
View(internet_data)

#Load the data related to specific countries -India, Japan, Egypt 
internet_data_country <- internet_data[which(internet_data$Country_PL == "Japan" |
                                               internet_data$Country_PL == "India" |
                                               internet_data$Country_PL == "Egypt"),]
View(internet_data_country)
rowindices_internet<-which(internet_data$Country_PL == "Japan" | 
                             internet_data$Country_PL == "India" |
                             internet_data$Country_PL == "Egypt")
rownames(internet_data_country)<- rowindices_internet

cols_1 <- c()
new_internet_data <- data.frame(internet_data_country)
rownames(new_internet_data)<- rowindices_internet

#Determine the values with aggreagted value > 0.5
for(i in rownames(new_internet_data)) {
  if(sum(new_internet_data[i,3:10] != "") > 0.5*ncol(new_internet_data))
  {
    cols_1 <- append(cols_1,i) 
  }
}

y <- as.numeric(unlist(cols_1))

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


##consolidation into  required data set

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

#Priority_1

japan_condition_data <- roomConditionData[which(roomConditionData$Country_PL == "Japan" |
                                                  roomConditionData$Country_PL == "India" |
                                                  roomConditionData$Country_PL == "Egypt"),]
rowindices_room<-which(roomConditionData$Country_PL == "Japan" | 
                         roomConditionData$Country_PL == "India" |
                         roomConditionData$Country_PL == "Egypt")
rownames(japan_condition_data)<- rowindices_room
View(japan_condition_data)

#Read the specific columns from the data

japan_condition_data_2 <- data.frame(japan_condition_data)
for(j in rownames(japan_condition_data_2))
{
  japan_condition_data_2[j,]<- as.numeric(japan_condition_data_2[j,]=='Y', na.rm=TRUE)
  
}
View(japan_condition_data_2)
rownames(japan_condition_data_2)<- rowindices_room


numOfCols <- ncol(japan_condition_data_2)
halfCols <- round(numOfCols/2)
retainRows<- c()
count<-1
for(eachRow in rownames(japan_condition_data_2))#1:nrow
{
  if(sum(japan_condition_data_2[eachRow,]==1, na.rm = TRUE )> 2)
  {
    
    retainRows[count]<- eachRow
    count<-count+1
  }
}
View(retainRows)
View(japan_condition_data_2)

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

cols_2 <- c()
count<-1
for(i in rowindices)
{
  if((is.na(japan_food_data[i,1])==FALSE) & (is.na(japan_food_data[i,2])==FALSE))
  {
    cols_2[count]<-i
    count <- count+1
  }
}

df2<- japan_food_data[cols_2,]
View(df2)

Newdf<- data.frame(df2)
View(Newdf)
rownames(Newdf) <-rownames(df2)
