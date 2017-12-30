library(gdata)
getwd()
sample_data = read.csv("SurveyDataExample.csv",header= TRUE); 
col1 = 233; 
new_data = sample_data[,c(col1)]; 
new_data
sum_p <- sum(new_data == "Promoter")
sum_p
sum_d <- sum(new_data == "Detractors")
sum_d

NPS <- sum_p - sum_d
NPS

#NPS_Type


options(max.print=10000000)

#guest_room_h = customer_svc, internet
Customer_SVC_H = 143

Staff_Cared_H= 144

Internet_Sat_H = 145
Check_In_H = 146
F&B_Overall_Experience_H  = 148


sample <- fread("out-201402.csv", select =  c("Country_PL"))
sum(sample =="India")

sample_1 <- gsub("",NA,sample)
na.omit(sample)
View(sample)
sum(sample =="Tokyo")
View(internet_data)
whole_data <- fread("out-201402.csv")
View(whole_data)








### internet data
install.packages("data.table")
library(data.table)
internet_data <- fread("out-201402.csv", select =  c("Country_PL","Internet_Sat_H", "Internet_Dissat_Lobby_H","Internet_Dissat_Slow_H","Internet_Dissat_Expensive_H","Internet_Dissat_Connectivity_H", "Internet_Dissat_Billing_H", "Internet_Dissat_Wired_H", "Internet_Dissat_Other_H", "TV_Internet_General_H"))
View(internet_data)


#summary(internet_data)

internet_data_country <- internet_data[which(internet_data$Country_PL == "India"),]
View(internet_data_country)
rowindices_internet<-which(internet_data$Country_PL == "India")
rownames(internet_data_country)<- rowindices_internet

xxx <- c()
new_internet_data <- data.frame(internet_data_country)
rownames(new_internet_data)<- rowindices_internet

for(i in rownames(new_internet_data)) {
  if(sum(new_internet_data[i,3:10] != "") > 0.5*ncol(new_internet_data))
    xxx <- append(xxx,i) 
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

roomConditionData <- fread("out-201402.csv",select=roomConditionColumns)

View(roomConditionData)

#for(newCol in colnames(roomConditionData))
#{
#  lapply(roomConditionData$newCol, as.numeric)  
#}

#roomConditionData$roomCondition<- NA


#predictionModel<-lm(formula=roomConditionData$roomCondition~., data=roomConditionData)
#Priority_1
#
japan_condition_data <- roomConditionData[which(roomConditionData$Country_PL == "India"),]
rowindices_room<-which(roomConditionData$Country_PL == "India")
rownames(japan_condition_data)<- rowindices_room
View(japan_condition_data)

#Read the specific columns from the data
#temp <- japan_condition_data
View(temp)
length(temp)
japan_condition_data_2 <- data.frame(japan_condition_data)
for(j in rownames(japan_condition_data_2))
{
  japan_condition_data_2[j,]<- as.numeric(japan_condition_data_2[j,]=='Y', na.rm=TRUE)
  
}
View(japan_condition_data_2)
rownames(japan_condition_data_2)<- rowindices_room

#temp[,29]<- as.numeric(temp[,1]=='Y', na.rm=TRUE)

warnings()
#View(temp)
#sum(temp$==0)
#TO remove the data where more than half of the values are NA or null 
warnings()
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


df1<-fread("out-201402.csv", select=c('F&B_FREQ_H','Spa F&B offering_PL','F&B_Overall_Experience_H', 'Country_PL'))
View(df1)
japan_food_data <- df1[which(df1$Country_PL == "United States"),]
View(japan_food_data)
rowindices<-which(df1$Country_PL == "United States")
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
sandhya <-rownames(new_data_frame)
new_new <- merge(xxx, sandhya, by=c("xxx[1]"="sandhya[1]"), all=TRUE)



x <- as.numeric(unlist(xxx))
y <- as.numeric(unlist(sandhya))

new_common <- intersect(y,z)
new_common
new_common_1 <- intersect(new_common,v3)
new_common_1




new_common <- data.frame(new_common)
View(new_common)
new_new <- which(v %in% xxx)
for( i in 1:nrow(v))
{
  for(j in 1:nrow(xxx))
  {
    if(v[i]==xxx[j])
    {
      Newxxx<-cbind(xxx[j],v[i])
    }}}
View(xxx)











typeof(internet_data$Internet_Sat_H)
sum(internet_data$Internet_Sat_H == 0)
#FL_data <- internet_data[new_internet_data$STATE_R == "FL"]

#length(which(is.na(new_internet_data[i,]))
xxx <- c()
for(i in 1:nrow(internet_data)) {
  if(sum(internet_data[i,2:9] != "") < 4) 
    xxx <- append(xxx,i)  
}
new_new <- internet_data[-xxx,]
View(new_new)
#length(which(is.na(data[i,])))
#new_internet_data <- internet_data[,2:9]
#View(new_internet_data)
i<-0
for(i in new_new)
{
  if(is.na(new_new$Internet_Sat_H))
    temp <- sum(new_new[i,] != "")
  if(temp<5)
  {
    new_new_internet_data <- new_internet_data[-i,]
  }
  
}
sum(new_internet_data[1,] != "")

sum(internet_data$Internet_Dissat_Lobby_H == "No")
#internet_data$ <- as.numeric(gsub(" ", "",internet_data))
hist(internet_data$Internet_Dissat_Lobby_H,internet_data$Internet_Dissat_Slow_H)
internet_data <- na.omit(internet_data)
View(internet_data)
internet_data <- gsub(" ","",internet_data)
View(internet_data)

data_2014_2 = read.csv("out-201402.csv",header= TRUE); 
#new_1 <- data_2014_2
#new_data_2014_2 <- data.frame(matrix(unlist(data_2014_2),
#nrow = length(data_2014_2), byrow = T), stringsAsFactors = FALSE)
typeof(data_2014_2)
nrow(data_2014_2)
ncol(data_2014_2)
str(data_2014_2)
na.omit(data_2014_2)
nrow(data_2014_2)
#col1 = 232;
col10 = 142
col11 = 143;
col12 = 144;
col13 = 145;
col14 = 146;
col15 = 148;
col16 = 149;
col17 = 150;
col18 = 151;
col19 = 152;
col20 = 153;
col21 = 154;
col22 = 155;
col23 = 156;

guest_room <-  data_2014_2[,c(col10,col12)]; 
guest_room_new <- na.omit(guest_room)
View(guest_room_new)
internet <- data_2014_2[,c(col16,col17,col18,col19,col20,col21,col22,col23)];
View(internet)
internet_new <- na.omit(internet)


xyz = data_2014_2[,c(col11,col12,col13,col14)]; 
ncol(xyz)
View(xyz)
typeof(xyz)
new_data_2014_2 <- data.frame(matrix(unlist(xyz),
                                     nrow = length(xyz), byrow = T), stringsAsFactors = FALSE)

typeof(xyz)
xyz_1 <- na.omit(xyz)
#na.omit(xyz$Staff_Cared_H)
#na.omit(xyz$Internet_Sat_H)
#na.omit(xyz$Check_In_H)
#na.omit(xyz$F.B_FREQ_H)
nrow(xyz_1)
View(xyz_1)
pro <- sum(xyz == "Promoter")
pro
sum_pro_per <- ((pro/nrow(data_2014_2))*100)
sum_pro_per

detr <- sum(xyz == "Detractor")
detr
sum_detr_per <- (detr/nrow(data_2014_2))*100
sum_detr_per


NPS_02 <- sum_pro_per - sum_detr_per
NPS_02

col2 = 200;
col3 = 201;
col4 = 202;
col5 = 203;
col6 = 204;
col7 = 205;
col8 = 206;
col19 = 140;


aaa <- sum(xyz == "")
aaa



#####################################################################################
##consolidation of the data into  required data set

library(data.table)


#Reading the specific columns for Room conditions
roomConditionColumns <- c("All Suites_PL","Bell Staff_PL","Boutique_PL","Business Center_PL",
                          "Casino_PL","Conference_PL","Convention_PL","Dry-Cleaning_PL","Elevators_PL",
                          "Fitness Center_PL","Fitness Trainer_PL","Golf_PL","Indoor Corridors_PL",
                          "Laundry_PL","Limo Service_PL","Mini-Bar_PL","Pool-Indoor_PL","Pool-Outdoor_PL",
                          "Regency Grand Club_PL","Resort_PL","Restaurant_PL","Self-Parking_PL",
                          "Shuttle Service_PL","Ski_PL","Spa_PL","Spa services in fitness center_PL",
                          "Spa online booking_PL","Spa F&B offering_PL","Valet Parking_PL", "Country_PL");

roomConditionData <- fread("out-201402.csv",select=roomConditionColumns)

View(roomConditionData)


#Read the specific columns from the data
roomConditionData<-data.frame(roomConditionData)
temp <- data.frame(roomConditionData )

temp <- roomConditionData[which(roomConditionData$Country_PL=='United States'),]
View(temp)
for(i in 1:nrow(temp))
{
  for(j in 1:ncol(temp))
  {
    temp[i,j]<- as.numeric(temp[i,j]=='Y', na.rm=TRUE) 
  }
}


#TO remove the data where more than half of the values are NA or null 

numOfCols <- ncol(temp)
halfCols <- round(numOfCols/2)
temp<-temp[,-numOfCols]
retainRows<- c()
count<-1

for(eachRow in 1:nrow(temp))#1:nrow
{
  
  if(sum(temp[eachRow,]==1, na.rm = TRUE )> halfCols)
  {
    retainRows[count]<- eachRow
    count<-count+1
  }
}


