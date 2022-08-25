#data entry & data visualization

house=read.csv("housePrice.csv")
summary(house)
house$Area=as.numeric(factor(house$Area))
house$Parking=factor(house$Parking)
house$Warehouse=factor(house$Warehouse)
house$Elevator=factor(house$Elevator)
house$Address=factor(house$Address)
summary(house)

plot(house$Price.USD. ~ house$Area, xlab="Area", ylab = "Price (USD)")
hist(house$Room, main = "Number of Rooms histogram",xlab="Rooms" )
plot(house$Parking, main = "parking chart",xlab="parking",ylim=c(0,3000)  )
plot(house$Warehouse, main = "Warehouse chart",xlab="Warehouse", ylim=c(0,3500) )
plot(house$Elevator, main = "Elevator chart",xlab="Elevator",ylim=c(0,3000) )
barplot(house$Address, main = "Address histogram",xlab="Address" )

#data pre-process:
##Area: outlier
##price: unnecessary field
##price.USD. : missing values

Area_LL1=0.3*quantile(house$Area,0.01) #outlier treatment
Area_UL2=3*quantile(house$Area,0.99) #outlier treatment
Area_LL1
Area_UL2
house$Area[house$Area<Area_LL1]=Area_LL1 #outlier treatment
house$Area[house$Area>Area_UL2]=Area_UL2 #outlier treatment

house=house[,-7] #unnecessary field

empty=which(is.na(house$Price.USD.)) #missing value treatment
house=house[-empty,] #missing value treatment

Price_LL1=0.3*quantile(house$Price.USD.,0.01) #outlier treatment
Price_UL1=3*quantile(house$Price.USD.,0.99) #outlier treatment
Price_LL1
Price_UL1
house$Price.USD.[house$Price.USD.<Price_LL1]=Price_LL1 #outlier treatment
house$Price.USD.[house$Price.USD.>Price_UL1]=Price_UL1 #outlier treatment

#variables transformation
unique_addresses=unique(house$Address)
write.csv(unique_addresses,"Adress.csv")
write.csv(house,"New_House.csv")
house=read.csv("New_House.csv")
house=house[,c(-1,-7,-10,-11)]

#dummy variable
house$Parking=factor(house$Parking)
house$Warehouse=factor(house$Warehouse)
house$Elevator=factor(house$Elevator)
house$New_address=factor(house$New_address)

house=dummy.data.frame(house)  #dummy variables
house=house[,c(-3,-5,-7,-10)]  #dummy variables
cor(house)

#train-test split
set.seed(0)  
x=sample.split(house, SplitRatio=0.8)  
training_set= subset(house, x==TRUE)  
test_set= subset(house, x==FALSE) 

#creating the model
linear_model1=lm(Price.USD.~ . ,data=training_set)
summary(linear_model1)

z1=predict(linear_model1, training_set)
z2=predict(linear_model1, test_set)  
mean((training_set$dependent_variable-z1)^2)  
mean((test_set$dependent_variable-z2)^2) 

##best subset selection
x1=regsubsets(Price.USD.~ . ,data=house, nvmax=6)  
summary(x1)
summary(x1)$adjr2  
y= which.max(summary(x1)$adjr2)  
coef(x1)  
coef(x1, y) 
