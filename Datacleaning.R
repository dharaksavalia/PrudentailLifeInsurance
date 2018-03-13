library(rpart)

myDummyData<-read.csv(file = "D:/subjects/Sping2018/IFO7800DataSciences/MidTermProject/train.csv/train.csv")
myData <- read.csv( file = "D:/subjects/Sping2018/IFO7800DataSciences/MidTermProject/train.csv/train.csv",
                    colClasses=c(Product_Info_1='factor', Product_Info_2='factor', Product_Info_3='factor', Product_Info_5='factor', 
                                 Product_Info_6='factor', Product_Info_7='factor', Employment_Info_2='factor', Employment_Info_3='factor', 
                                 Employment_Info_5='factor', InsuredInfo_1='factor', InsuredInfo_2='factor', InsuredInfo_3='factor', InsuredInfo_4='factor', 
                                 InsuredInfo_5='factor', InsuredInfo_6='factor', InsuredInfo_7='factor', Insurance_History_1='factor', 
                                 Insurance_History_2='factor', Insurance_History_3='factor', Insurance_History_4='factor', 
                                 Insurance_History_7='factor', Insurance_History_8='factor', Insurance_History_9='factor', Family_Hist_1='factor', 
                                 Medical_History_2='factor', Medical_History_3='factor', Medical_History_4='factor', Medical_History_5='factor', Medical_History_6='factor',
                                 Medical_History_7='factor', Medical_History_8='factor', Medical_History_9='factor', Medical_History_11='factor', 
                                 Medical_History_12='factor', Medical_History_13='factor', Medical_History_14='factor', Medical_History_16='factor', Medical_History_17='factor', 
                                 Medical_History_18='factor', Medical_History_19='factor', Medical_History_20='factor', Medical_History_21='factor', Medical_History_22='factor', 
                                 Medical_History_23='factor', Medical_History_25='factor', Medical_History_26='factor', Medical_History_27='factor', Medical_History_28='factor', 
                                 Medical_History_29='factor', Medical_History_30='factor', Medical_History_31='factor', Medical_History_33='factor', Medical_History_34='factor', 
                                 Medical_History_35='factor', Medical_History_36='factor', Medical_History_37='factor', Medical_History_38='factor', Medical_History_39='factor', 
                                 Medical_History_40='factor', Medical_History_41='factor',
                                 Product_Info_4='numeric', Ins_Age='numeric', Ht='numeric', Wt='numeric', BMI='numeric', Employment_Info_1='numeric', Employment_Info_4='numeric', Employment_Info_6='numeric', 
                                 Insurance_History_5='numeric', Family_Hist_2='numeric', Family_Hist_3='numeric', Family_Hist_4='numeric', Family_Hist_5='numeric',
                                 Medical_History_1='integer', Medical_History_10='integer', Medical_History_15='integer', Medical_History_24='integer', Medical_History_32='integer'))
str(myData)
#Technically corrrect data
str(myData$Product_Info_1)
myData$Product_Info_1=as.factor(myData$Product_Info_1)
str(myData$Product_Info_1)
str(myData$Product_Info_2)
summary(myData$Product_Info_2)

str(myData$Product_Info_3)
summary(myData$Product_Info_3)

categoricalValues=c("Product_Info_1","Product_Info_2","Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2"
                    , "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", 
                    "Medical_History_5", "Medical_History_6", "Medical_History_7","Medical_History_8", "Medical_History_9", "Medical_History_11","Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", 
                    "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22","Medical_History_23", "Medical_History_25", 
                    "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33",
                    "Medical_History_34", "Medical_History_35", "Medical_History_36","Medical_History_37","Medical_History_38", "Medical_History_39", "Medical_History_40","Medical_History_41",
                    "Response"="ordinal")
categoricalValues
for (i in categoricalValues)
  print(summary(myData[i]))

#abnormalilites seems to appered in Inusred info _3, employment Info _2 producct infor 3 and 2 unsured infor _3
str(myData$Product_Info_3)#data is correct
str(myData$Employment_Info_2)
str(myData$InsuredInfo_3)
str(myData$Medical_History_2)
str(myData$InsuredInfo_3)
#categirucak data are proper now going to numeric values
numericValues=c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", 
                "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")
for (i in numericValues)
  print(summary(myData[i]))
for (i in numericValues)
  print(summary(myDummyData[i]))

discreteValues<-c("Medical_History_1", "Medical_History_10", 
                  "Medical_History_15", "Medical_History_24", "Medical_History_32")
discreteValues

for (i in discreteValues)
  print(summary(myData[i]))

summary(myData$Medical_Keyword_1)
str(myData$Medical_Keyword_1)
myData
plot(myData$Product_Info_2,myData$Response)
summary(myData$Medical_Keyword_1)
summary(myData$Medical_Keyword_10)
#we can clearly Seed that medical key word 1 to 10 count of medical key word, we can just add them into single column
myData <- transform( myData,
                     count = Medical_Keyword_1+Medical_Keyword_2+Medical_Keyword_3+Medical_Keyword_4+
                     Medical_Keyword_5+Medical_Keyword_6+Medical_Keyword_7+Medical_Keyword_8+
                       Medical_Keyword_9+Medical_Keyword_10+Medical_Keyword_11+Medical_Keyword_12+
                       Medical_Keyword_13+Medical_Keyword_14+Medical_Keyword_15+Medical_Keyword_16+Medical_Keyword_17+
                       Medical_Keyword_18+Medical_Keyword_19+Medical_Keyword_20+Medical_Keyword_21+
                       Medical_Keyword_22+Medical_Keyword_23++Medical_Keyword_24+Medical_Keyword_25+
                       Medical_Keyword_26+Medical_Keyword_27+Medical_Keyword_28+Medical_Keyword_29+
                       Medical_Keyword_30+Medical_Keyword_31+Medical_Keyword_32+Medical_Keyword_33+
                       Medical_Keyword_34+Medical_Keyword_35+Medical_Keyword_36+Medical_Keyword_37+
                       Medical_Keyword_38+Medical_Keyword_39+Medical_Keyword_40+Medical_Keyword_41+
                       Medical_Keyword_42+Medical_Keyword_43+Medical_Keyword_44+Medical_Keyword_45+
                       Medical_Keyword_46+Medical_Keyword_47+Medical_Keyword_48)

summary(myData$Product_Info_2)
#from various sources in internet and public script available,it is shown that Product Info 2 result in better if we devide them. 

plot(myData$Product_Info_2,myData$Response)


myData[,"Product_Info_2_char"]<-as.factor(substr(myData[,3],0,1))
myData[,"Product_Info_2_num"]<-as.factor(substr(myData[,3],2,2))
myData$Product_Info_2_num
myData$Product_Info_2_num
summary(myData$Product_Info_2_char)
plot(myData$Product_Info_2_char,myData$Response)
plot(myData$Product_Info_2_num,myData$Response)

cor(myData[sapply(myData, is.numeric)])
myData[,"BMI_Age"]<-myData[,"BMI"]*myData[,"Ins_Age"]
myData$BMI_Age
nrow(myData)

#Dealing with negative values

names(myData)
#for employment
for ( i in 1:nrow(myData))
  if(is.na(myData[i,13]))
    myData[i,13]=0
summary(myData$Employment_Info_1)

for ( i in 1:nrow(myData))
  if(is.na(myData[i,16]))
    myData[i,16]=0
summary(myData$Employment_Info_1)

myZeroAdjustement <- function(Data,value,index){
  for ( i in 1:nrow(Data)){
    if(is.na(Data[i,index])){
      Data[i,index]=value
    }
  }
  return(Data)
}
myData<-myZeroAdjustement(myData,0.362,18)
summary(myData$Employment_Info_6)
#filling null values of insurance history 5
myData<-myZeroAdjustement(myData,0,30)
summary(myData$Insurance_History_5)
#filling null values family history 2

myData<-myZeroAdjustement(myData,0.464,35)
summary(myData$Family_Hist_2)
#filling null values family history 3
myData<-myZeroAdjustement(myData,0.52,36)
summary(myData$Family_Hist_3)
#family history 4
myData<-myZeroAdjustement(myData,0.445,36)
summary(myData$Family_Hist_3)
#setting family history 5 as null
myData$Family_Hist_5<-NULL
#setting medical history 1 NA as 7.962
myData<-myZeroAdjustement(myData,7.962,39)
summary(myData$Medical_History_1)
#setting medical null all other 
myData$Medical_History_10<-NULL
myData$Medical_History_15<-NULL
myData$Medical_History_24<-NULL
myData$Medical_History_32<-NULL

myData[is.na(myData)]<-0
names(myData)
#removing medical keyword 1 to 48
myData<-myData[,-(38:122),drop=FALSE]
names(myData)
write.csv(myData, file = "CleanedData.csv")
