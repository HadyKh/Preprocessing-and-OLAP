#*******************************installing packages****************************************
install.packages("reshape")
library(reshape)
install.packages("plyr")
library(plyr)
install.packages("modeest")
library(modeest)
install.packages("ggplot")
library(ggplot)
install.packages("ggplot2")
library(ggplot2)

#*****************************************************************************************************
#*******************************Part 1****************************************
#*****************************************************************************************************



#*******************************reading Data****************************************
emp <- read.csv("bank-additional-full.csv",
                header =TRUE, sep =";")[c('age', 'education', 'previous', 'pdays', 'y')]

#*******************************plotting pdays with 999****************************************
hist(emp$pdays, 
     col = 'green',
     xlim = c(0,1000),
     xlab = 'Pdays',
     ylab = 'Frequency'
)

#*******************************manipulating pdays****************************************
emp_without_999 <- data.frame(emp)
emp_without_999$pdays[emp$pdays == '999'] <- NA

#*******************************plotting pdays with NA****************************************
hist(emp_without_999$pdays, 
     col = 'green',
     # border = NULL,
     xlim = c(0,40),
     xlab = 'Pdays',
     ylab = 'Frequency',
)


#*******************************manipulating education****************************************
emp_final <- data.frame(emp_without_999)
emp_final$education <- revalue(x = emp$education,
        replace= c("illiterate" = 0,"basic.4y" = 4, "basic.6y" = 6, "basic.9y" = 9,
                   "high.school" = 12, "professional.course" = 14, "university.degree" = 16,
                   "unknown" = NA))

#*******************************Measuring mean, median, mode****************************************
mean_value_age <- mean(emp_final$age)
median_value_age <- median(emp_final$age)
mode_value_age <- mlv(emp_final$age, method = mfv)

mean_value_age
median_value_age
mode_value_age
#***********Plotting age***************
boxplot(emp_final$age,
        main = "Age",
        xlab = " Data",
        ylab = " Measures",
        horizontal = TRUE,
        col = "green",
        staplewex = 1,
        axes = TRUE
)
text(x=fivenum(emp_final$age), labels =fivenum(emp_final$age), y=1.25)
summary(emp_final$age)

#**********Plotting quantiles***************

qqnorm(x = emp_final$age_z, y = emp_final$age)

#*******************************Standardizing age****************************************
emp_final$age_z <- scale(x = emp_final$age)

#*******************************getting the outliers****************************************
outliers <-  emp_final$age_z[ which(emp_final$age_z < -3 | emp_final$age_z > 3), ]
outliers <- data.frame(outliers)
outliers





#*****************************************************************************************************
#*******************************Part 2****************************************
#*****************************************************************************************************

#*******************************reading Data****************************************
cheese_table <- read.csv("Cheese.csv", header =TRUE, sep =",")
names(cheese_table)[3]<-"cheese_price"
date_table <- read.csv("Date.csv", header =TRUE, sep =",")
day_table <- read.csv("Day.csv", header =TRUE, sep =",")
dough_table <- read.csv("Dough.csv", header =TRUE, sep =",")
names(dough_table)[3]<-"dough_price"
hour_table <- read.csv("Hour.csv", header =TRUE, sep =",")
location_table <- read.csv("Location.csv", header =TRUE, sep =",")
orders_table <- read.csv("Orders.csv", header =TRUE, sep =",")
size_table <- read.csv("Size.csv", header =TRUE, sep =",")
names(size_table)[3]<-"size_price"
topping_table <- read.csv("Topping.csv", header =TRUE, sep =",")
names(topping_table)[3]<-"top_price"
year_table <- read.csv("Year.csv", header =TRUE, sep =",")
month_table <- read.csv("month.csv", header =TRUE, sep =",")


#*******************************Generate data****************************************
#*******************************Generate data for the dimension table****************************************
gen_dates_dm <- function(records) {
        date_id <- expand.grid(1:records)
        day_id <- sample(day_table$Day_Name, records, replace=T, prob=c(2,2,2,3,2,5,7))
        month_id <- sample(month_table$Month, records, replace=T, prob=c(2,2,2,3,2,2,2,4,2,2,3,8))
        year_id <- sample(year_table$Year, records, replace=T, prob=c(5,1))
        hour_id <- sample(hour_table$Hour, records, replace=T, prob=c(2,2,2,3,2,2,2,4,2,5,3,4,1,1,3,4,4,5,2,7,4,5,1))
        
        date_table <- data.frame(Date_ID=date_id,
                            Year_ID=year_id,
                            Month_ID=month_id,
                            Day_ID=day_id,
                            Hour_ID=hour_id)
}
date_table <- gen_dates_dm(100)
View(date_table)

names(date_table)[1]<-"Date_ID"
date_table
#*******************************Generate data for the Fact table****************************************
gen_orders <- function(records) {
        loc <- sample(location_table$Loc_ID, records, replace=T, prob=c(2, 2, 4))
        cheese <- sample(cheese_table$Cheese_ID, records, replace=T, prob=c(2, 2, 4))
        dough <- sample(dough_table$Dough_ID, records, replace=T, prob=c(3, 2, 2))
        topping <- sample(topping_table$Topping_ID, records, replace=T, prob=c(1, 3, 2, 3))
        size <- sample(size_table$Size_ID, records, replace=T, prob=c(5, 4, 3, 6, 9))
        date <- sample(date_table$Date_ID, records, replace=T)
        quantity <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), records, replace=T, prob=c(10, 8, 5, 4, 2, 2, 2, 1, 1, 1))
        profit <- quantity*(cheese_table[as.factor(cheese),]$cheese_price+
                                    dough_table[as.factor(dough),]$dough_price+
                                    topping_table[as.factor(topping),]$top_price+
                                    size_table[as.factor(size),]$size_price)

        orders_table <- data.frame(Loc_ID = loc,
                            Dough_ID = dough,
                            Topping_ID = topping,
                            Cheese_ID = cheese,
                            Size_ID = size,
                            Date_ID = date,
                            Quantity = quantity,
                            Profit = profit
                            )
}

orders_table <- gen_orders(5000)
View(orders_table)

#****************************************************************************************
#*******************************Join Tables****************************************
table1 <- merge(orders_table, cheese_table, by = c("Cheese_ID", "Cheese_ID"))
table2 <- merge(table1, date_table, by = c("Date_ID", "Date_ID"))
table3 <- merge(table2, dough_table, by = c("Dough_ID", "Dough_ID"))
table4 <- merge(table3, size_table, by = c("Size_ID", "Size_ID"))
data_snf <- merge(table4, topping_table, by = c("Topping_ID", "Topping_ID"))

#****************************************************************************************
#*******************************Building-up cubes****************************************

Quantity_cube <- 
        tapply(data_snf$Quantity, 
               data_snf[c( "D_Type", "Top_name", "Size", "Cheese_Type", "Month_ID", "Year_ID")], 
               FUN=function(x){return(sum(x))})
Quantity_cube

dimnames(Quantity_cube)

#*******************************Drill-down & Roll-up***************************************

apply(Quantity_cube, c("D_Type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
apply(Quantity_cube, c("Top_name"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
apply(Quantity_cube, c("Size"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
apply(Quantity_cube, c("Cheese_Type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
apply(Quantity_cube, c("Month_ID", "Year_ID"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#*****************************************************************************************
#*******************************Trend over time*****************************
month_df <- apply(Quantity_cube, c("Month_ID"), 
                 FUN=function(x) {return(sum(x, na.rm=TRUE))})
month_df

barplot(month_df, main = "Among months",
        xlab="Months", ylab="Orders amount", names.arg = rownames(month_df),
        col="green",ylim = c(0,5000),axes = TRUE)

year_df <- apply(Quantity_cube, c("Year_ID"), 
                 FUN=function(x) {return(sum(x, na.rm=TRUE))})
year_df

barplot(year_df, main = "Among Years",
        xlab="Years", ylab="quantity", names.arg = rownames(year_df),
        col="green",ylim = c(0,15000),axes = TRUE)


#*****************************************************************************************
#*******************************Getting component consumption*****************************
dough_quantity <- apply(Quantity_cube, c("D_Type"), 
                        FUN=function(x) {return(sum(x, na.rm=TRUE))})
dough_quantity_df <- data.frame(quant = dough_quantity)
top_quantity <- apply(Quantity_cube, c("Top_name"), 
                      FUN=function(x) {return(sum(x, na.rm=TRUE))})
top_quantity_df <- data.frame(quant = top_quantity)
cheese_quantity <- apply(Quantity_cube, c("Cheese_Type"), 
                         FUN=function(x) {return(sum(x, na.rm=TRUE))})
cheese_quantity_df <- data.frame(quant = cheese_quantity)

top_quantity

quantity_df <- rbind(dough_quantity_df, top_quantity_df, cheese_quantity_df)
quantity_df

barplot(quantity_df$quant, main = "Components Consumption",
        xlab="component", ylab="quantity", names.arg = rownames(quantity_df),
        col="green",ylim = c(0,10000),axes = TRUE)



#*****************************************************************************************
#*******************************Loving bigger pizza*****************************

size_quantity <- apply(Quantity_cube, c("Size"), 
                        FUN=function(x) {return(sum(x, na.rm=TRUE))})
size_quantity_df <- data.frame(quant = size_quantity)

barplot(size_quantity_df$quant, main = "Pizza size",
        xlab="size", ylab="amount of orders", names.arg = rownames(size_quantity_df),
        col="green",xlim = c(0,6000),axes = TRUE, horiz = TRUE)



