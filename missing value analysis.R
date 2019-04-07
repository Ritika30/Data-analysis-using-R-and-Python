rm(list = ls())

setwd("C:/Users/ritikaaswani/Documents/r practice/")
getwd()
my_data=read.csv("my_data1.csv")
View(my_data)
str(my_data)


#converting datatypes



my_data$education[my_data$education %in% "illiterate"] = "unknown"
#wherever value of education is in basic.4y....... and among the elements of list, we make it high school.
my_data$education[my_data$education %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
my_data$default[my_data$default %in% "yes"] = "unknown"
my_data$default = as.factor(as.character(my_data$default))
#in factor we define categorical data which contains levels and number levels mean number of unique values/categories
my_data$marital[my_data$marital %in% "unknown"] = "married"
my_data$marital = as.factor(as.character(my_data$marital))
my_data$month[my_data$month %in% c("sep","oct","mar","dec")] = "dec"
my_data$month[my_data$month %in% c("aug","jul","jun","may","nov")] = "jun"
my_data$month = as.factor(as.character(my_data$month))
my_data$loan[my_data$loan %in% "unknown"] = "no"
my_data$loan = as.factor(as.character(my_data$loan))
my_data$education = as.factor(as.character(my_data$education))
my_data$job[my_data$job %in% c("management","unknown","unemployed","admin.")] = "admin."
my_data$job[my_data$job %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
my_data$job = as.factor(as.character(my_data$job))


#missing value analysis

##if missing values are more than 30 then we tend drop the variable becuase it would'nt give us much info
#here we are tryinh to find number of missing vaues in each variable
is.na(my_data)
sum(is.na(my_data))

#to know missing values on all variables
#this is a function with parameter x where x is where apply is used that is on my_data,2 i.e on the columns of my_data

missing_val=data.frame(apply(my_data,2,function(x){sum(is.na(x))}))#here 2 means we are taking columns(1 means row) of my_data in apply

#converting it with proper column names
View(missing_val)
 missing_val$columns=row.names(missing_val)
View(missing_val)
row.names(missing_val)=NULL


#applying above lines of code on dataframe to unserstand how it works
# a=data.frame(c('a1','b','c','d','e'),c(1,2,3,4,5),c('a','b','c','d','e'))
# View(a)
# b=data.frame(apply(a,2,function(x){(sum(is.na(x)))}))
# View(b)
# a$new_column=row.names(a)
# row.names(a)=NULL

#converting in percentage 

nrow(my_data)
View((missing_val))
names(missing_val)[1]='missing_value_percent'
missing_val$missing_val_percent=(missing_val$missing_value_percent)/nrow(my_data)*100
missing_val$missing_val_percent
#arranging percentage of missing values in ascending order.
missing_val=missing_val[order(-missing_val$missing_val_percent),]

#rearranging the columns for better visualization

missing_val=missing_val[,c(2,1)]
View(missing_val)


#save data to local
write.csv(missing_val,"missing_percentage.csv")

#now we will apply various methods to fill the missing value

#let us first take one value of first column of our dataset and make it NA 
#and then find it's value by applying different methods and check which method fits best by comparing it's actual value.

#example :in our data lets take value of first column 811 row which is 31 and make it NA deliberatly

#actual value=31

#after noting the value let us convert it to NA

my_data[811,1]=NA
View(my_data)
#mean method

my_data$age[is.na(my_data$age)]=mean(my_data$age,na.rm = T)
my_data[811,1]
#value is 39.96024

#median method
my_data$age[is.na(my_data$age)]=median(my_data$age,na.rm = T)
my_data[811,1]
#value =57

#knn imputation
impute.knn(my_data,k=5)
#value=36

#so knn imputation gives the nearest value to actual value so we will use this method for filling outliers of this column of this dataset

  