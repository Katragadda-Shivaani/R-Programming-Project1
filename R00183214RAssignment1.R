#installing tidyverse package used for transforming and visualizing the data.
install.packages("tidyverse")
library(tidyverse)#loads and attaches the add-on packages.

#installing readxl package useful to get the data from excel to R easily.
install.packages("readxl")
library(readxl)#loads and attaches the add-on packages.

#installing dplyr package which is useful for manipulating datasets in R very effectively.
install.packages("dplyr")
library(dplyr)#loads and attaches the add-on packages.

#installing ggplot2 package,it is particularly useful for visualizing the data.
install.packages("ggplot2")
library(ggplot2)#loads and attaches the add-on packages.

#-----------------------------------------------------------------------------------------------------------------------
  
#Question1 : Using the xlsx or readxl package or otherwise, read each sheet in the "assignment1.xlsx" file into R. 

#Reading the sheet1 of assignment.xlsx file by using read_excel() function and it is named as assignment1_sheet1

assignment1_sheet1 <- read_excel("F:/R Assignment/assignment1.xlsx", 
                                   sheet = "Sheet1")
View(assignment1_sheet1)#viewing the assignment1_sheet1


#Reading the sheet2 of assignment.xlsx file by using read_excel() function and it is named as assignment1_sheet2

assignment1_sheet2 <- read_excel("F:/R Assignment/assignment1.xlsx", 
                          sheet = "Sheet2")

View(assignment1_sheet2)#viewing the assignment1_sheet2

#--------------------------------------------------------------------------------------------------------------------------------
  
#Question2:Generate a data frame for each sheet in the file. 

#Generating the dataframe for assignment1_sheet1 using as.data.frame() function and that dataframe is named as assignment1_sheet1.df 
  
assignment1_sheet1.df <- as.data.frame(assignment1_sheet1)

str(assignment1_sheet1.df) #structure of assignment1_sheet1.df 

#Generating the dataframe for assignment1_sheet2 using as.data.frame() function and that dataframe is named as assignment1_sheet2.df 

assignment1_sheet2.df <- as.data.frame(assignment1_sheet2)

str(assignment1_sheet2.df)#structure of assignment1_sheet2.df 

#---------------------------------------------------------------------------------------------------------------------------------
  
#Question3: The dataset in the first sheet is a random selection from a larger dataset. You will never get access to the full dataset so
#you should regenerate a new identification number for each subject in the dataset.This should be the row number of each entry in Sheet 1.You do not need to do this for Sheet 2. 


#Creating a new columnn for assignment1_sheet1.df which is named as NewIDNumber and contains unique identification for each rows starts from 1 and ends at 160. 
#this is done by using seq() function which will generate the numbers in a sequence order
assignment1_sheet1.df$NewIDNumber<-c(seq(1,160))           

View(assignment1_sheet1.df)#Viewing the assignment1_sheet1.df which contains new colum NewIDNumber

#--------------------------------------------------------------------------------------------------------------------------------
#Question4:It is also required to have an additional identifier which is the number you have generated in (3) followed by the first letter 
#of each subjects first name and then followed by the first letter of each subject's surname.You do not need to do this for Sheet 2. 

#taking the first letter from Firstname column and storing it in the variable z.
#this is done by using substr() function which will give the which helps to take the desired position of letter in the string
z<-substr(assignment1_sheet1.df$FirstName,1,1)
z#to see the output of z

#Now taking i am removing the spaces which is present in the surname column in the starting of the name because I need the first letter of the surname
# but there is space.so Inorder to get the first letter of surname I am removing the space. 
x <- gsub("\\s+", "",assignment1_sheet1.df$Surname)
x#to see the output of x

#After that i am taking the first letter of surname and storing it in variable z1
z1<-substr(x,1,1)
z1#to see the output of z1

#we have to take the IDNumber,first letter of firstname,first letter of surname in one new column for assignment1_sheet1.df and named as Identifier 
#In that column there should be no space in between so sep(" ") is used which will not allow any spaces in between which is named as Identifier
#the paste() will combine all together and gives the output
assignment1_sheet1.df$Identifier<- paste(assignment1_sheet1.df$NewIDNumber,z,z1,sep="")

View(assignment1_sheet1.df)#viewing the assignment1_sheet1.df which contains new column Identifier

#---------------------------------------------------------------------------------------------------------------------------------------

#Question5 : Although the data is not available for most subjects, some data highlighting subjects state of health is available in Sheet 2. 
#You should use the subjects ID number to match it and merge it with the data in Sheet 1. 

#I have used the left_join for merging both the dataframes by matching it with IDNumber,so both the data frames will be merged and it is assigned to merge_data variable. 
merge_data<-left_join(assignment1_sheet1.df,assignment1_sheet2.df[c("IDNumber","Health")], by="IDNumber")

View(merge_data)#viewing the merge_data 

#------------------------------------------------------------------------------------------------------------------------------------

#Question6 : Not every subject has its ID number included in Sheet 2.  You should attempt to match the remaining subjects using their first and surnames. 
#This must be done using tidyverse in a robust manner.Your code for doing this should work again in the case of a new sample of data being provided.  

#Now i have to fill the data in rows  by using FirstName and surname columns so I used left_join and piped it with the merge_data and stored that data in merge_data1. 
merge_data1 <- merge_data%>% left_join(assignment1_sheet2.df[c("FirstName","Surname","Health")],by = c("FirstName","Surname"))
View(merge_data1) #viewing the merge_data1 which contains two  Health columns i.e Health.x,Health.y

#I have to use unite() function to unite both the Health.x,Health.y columns inorder to get the whole data in one column and assigned it to r variable. The data will be united and will be stored in Health.x
r=unite(merge_data1,Health.x,Health.y)
View(r) #viewing the data which contains only one health column

#Renaming health.x as Health, assigned to final_df variable
final_df= r%>% rename(Health = Health.x)
View(final_df) #Viewing the final dataset which contains over all data of both the sheets.

#---------------------------------------------------------------------------------------------------------------------------------------

#Question7: You should add a column for age range. This should be  Age Range Category Name 0-17->1,18-35-> 2,35-54->3,54-74->4,74+-> 5 

#Adding the new colum CategoryName which contains the category number of the age range. 
final_df$CategoryName[final_df$Age >= 0 & final_df$Age <= 18] <- "1" #categoryName is 1 if the age is between 0-18
final_df$CategoryName[final_df$Age >= 18 & final_df$Age < 35] <- "2" #categoryName is 2 if the age is between 18-35
final_df$CategoryName[final_df$Age >= 35 & final_df$Age < 54] <- "3" #categoryName is 3 if the age is between 35-54
final_df$CategoryName[final_df$Age >= 54 & final_df$Age < 74] <- "4" #categoryName is 4 if the age is between 54-74
final_df$CategoryName[final_df$Age >= 74] <- "5" #categoryName is 5 if the age is more than 74

View(final_df)

#-------------------------------------------------------------------------------------------------------------------------------------

#Question8 :You should filter the data by each age category. Generate a bar plot using ggplot2 for the criminal record variable.

#converting the Criminal Record data in to categorical data because the category name is also categorical data 
final_df$CriminalRecord<-as.factor(final_df$CriminalRecord)

#plotting bar graph between category name and criminal record using ggplot
ggplot(final_df,aes(x=CategoryName,fill=CriminalRecord))+geom_bar(position="dodge")+labs(caption = "Source: final_df")+
  ggtitle(label="Barplot of category Name vs criminal Record")+xlab("category name")+ylab("criminal Record")+theme_minimal()
  

#-------------------------------------------------------------------------------------------------------------------------------------

#Question9:You should generate an appropriate visualisation examining the relationships between height,weight, age and criminal records. 


#converting the height(m) data into categorical data
final_df$`Height(m)`<-as.factor(final_df$`Height(m)`)
#converting the CriminalRecord data onto categorical data
final_df$CriminalRecord<-as.factor(final_df$CriminalRecord)


#plotting scatterplot between Age,Height(m),Weight(kg) and criminal record using ggplot

ggplot(final_df, aes(x=Age, y=`Height(m)`)) + geom_point(aes(col=CriminalRecord, size=`Weight(kg)`))+
 theme_minimal()+labs(subtitle="Age Vs Height", x="Age",y="Height(m)",title="Scatterplot",caption = "Source: final_df")


#plotting Barplot between Age,Height(m),Weight(kg) and criminal record using ggplot

ggplot(final_df, aes(x=Age, y=`Height(m)`)) + geom_bar(stat="identity",aes(col=CriminalRecord, size=`Weight(kg)`))+
  geom_jitter(width = .5, size=1)+ theme_minimal()+labs(subtitle="Age Vs Height", x="Age",y="Height(m)",title="Barplot",caption = "Source: final_df")


#plotting bubble plot between Age,Height(m),Weight(kg) and criminal record using ggplot

g <- ggplot(final_df, aes(x = Age,y=`Height(m)`)) + 
  theme_minimal()+labs(subtitle="Age Vs Height", x="Age",y="Height(m)",title="bubble chart",caption = "Source: final_df")+
  geom_jitter(aes(col=CriminalRecord, size=`Weight(kg)`))+ 
  geom_smooth(aes(col=CriminalRecord), method="lm", se=F)
g
 
#From the plots  we can say that the criminal record 1 is normal lessly scattered and it is not showing any effective changes  when height and age increasing.
#When compare to both criminal record 2 is showing much progress and densly populated with respect to increase in the height and weight.

#-----------------------------------------------------------------------------------------------------------------

#Question10:. Using filters, you should analyse if there are any interesting results in the dataset regarding the
#relationships between height, weight and criminal record. Use appropriate visualisations.


#filtering the data by taking CriminalRecord==1 and assigning that filtered frame to f

f<-filter(final_df,CriminalRecord==1)
View(f) #viewing the dataframe

#plotting the Barplot by taking the filtered dataframe f ,Height(m) on x axis and Weight(kg) on y axis

ggplot(f,aes(x=`Height(m)`, y=`Weight(kg)`)) + geom_bar(stat="identity",position="dodge",fill="red")+
  labs(subtitle="Age Vs Height", x="Height(m)",
       y="Weight(Kg)",title="Bar Plot",caption = "Source: f")

#filtering the data by taking CriminalRecord==2 and assigning that filtered frame to f1

f1<-filter(final_df,CriminalRecord==2)
View(f1)#viewing the dataframe

#plotting the Barplot by taking the filtered dataframe f1 ,Height(m) on x axis and Weight(kg) on y axis

ggplot(f1,aes(x=`Height(m)`, y=`Weight(kg)`)) + geom_bar(stat="identity",position="dodge",fill="blue")+
  labs(subtitle="Age Vs Height", x="Height(m)",
       y="Weight(Kg)",title="Bar Plot",caption = "Source: f")

#From the two graphs based on criminalRecord we can say that for criminal record 1 the graph is constantly increasing from certain point and there is no deep raise or fall from the particular point with respect to weight and height.
#But in case of  criminal Record2   the graph is constantly changing fromone point to another point with respect to height and weight.

#-----------------------------------------------------------------------------------------------------------

#Question11: Generate a smaller data frame for the subjects where health related data is available. Examine if
#there is a relationship between the different states of health and height, weight or age. Use
#appropriate visualisations. Note this should include a modelling type analysis such as regression. 

#subsetting the data from the health column, I am considering all the rows of data where health not equal to missing values and assigned to ss variable
ss<-subset(final_df,Health!="NA")
View(ss)#Viewing the data

#plotting bubble chart between Age,Height(m),Weight(kg) and Health
g <- ggplot(final_df, aes(x = Age,y=`Height(m)`)) + 
  theme_minimal()+labs(subtitle="Age Vs Height", x="Age",y="Height(m)",title="bubble chart",caption = "Source: final_df")+
  geom_jitter(aes(col=Health, size=`Weight(kg)`))+ 
  geom_smooth(aes(col=Health), method="lm", se=F)
g

#from the graph we can say that Health 2 value is densly populated with respect to Age,Height(m),Weight(kg).

#Multinomial Logistic regression

#installing nnet package used for neutral networks
install.packages("nnet")
library(nnet)#loads and attaches the add-on packages.

#converting health data in to categorical data

ss$Health <- as.factor(ss$Health)

#Assigining ss$Health to variable Health
Health<-ss$Health

#Assigining ss$Age to variable Age
Age<-ss$Age

#Assigining ss$Height(m) to variable Height
Height<-ss$`Height(m)`

#Assigining ss$Weight(kg) to variable weight
Weight<-ss$`Weight(kg)`

#there are 2 values in health data so we need reference level therefore relevel() is used and considered reference 1 i.e ref=1 and assigining to ss$out variable
ss$out<-relevel(ss$Health, ref="1")

#creating the model by using the three columns i.e Age,Height,Weight
mymodel<-multinom(out~Age+Height+Weight, data=ss)

#summary of the model
summary(mymodel)

#predict
predict(mymodel,ss) #predicting the model 

predict(mymodel,ss,type="prob") #prediciting the model with probability


#missclassification error

cm<-table(predict(mymodel),ss$Health)#preparing confusion matrix and named as cm

cm #printing the output of confusion matrix

1-sum(diag(cm))/sum(cm)#checking the missclassification error percentage by formula

#2-tailed z test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors#performing 2-tailed z test

p <- (1 - pnorm(abs(z), 0, 1)) * 2#performing 2-tailed z test

p#printing the output of p


       
       



