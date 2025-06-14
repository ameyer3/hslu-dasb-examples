2 ^ 3 - 7 %% 3
2 ^ (3 - 7) %% 3 ==  (1 / 2) ^ (2 * 2)
2 ^ (3 - 7 %% 3)
2 ^ 3 - 7 %% 3 - 2 ^ (3 - 7 %% 3)

#comment
nr <- 19; y <- 4; #assignment
while (nr < 42) {print(paste(nr," : we have ",42-nr," still spaces...")); nr=nr+1}
nr

#importing the library named "tidyverse" 
#it provideds numerous function to import, manipulate and visualize data
library(tidyverse)

nr <- as_tibble(c(2,"Test",TRUE))
nr


library(readr)
car_197x_costs <- read_delim("car_197x_costs.csv", 
                             "-", escape_double = FALSE, 
                             col_types = cols(Location = col_factor(levels = c("","California")), 
                                              `Matriculation Year` = col_integer()), 
                             trim_ws = TRUE)
View(car_197x_costs)



#as demonstration, let's import a local dataset into a data.frame
#remember to change the path accordingly to the location where the data file (here: "auto-mpg.data") is saved
DS <- read.table("C:/Users/JumpStart/switchdrive/teaching/2020_02_HS2020/DASB/SW02/auto-mpg.data", quote="\"", comment.char="")
#let's open it in the "object inspector 
View(DS)
#let's rename the columns from V1, V2,... to the names (they were manually extracted form the "auto-mpg.name" file
names(DS) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name")
#let's see it in the terminal 
DS
#for better manipulation and readibilitty, let's transform the data.frame into a "tibble" object
DS <- as_tibble(DS)
#let's see again it (but as tibble) in the terminal 
DS

#we would like to see only the "heavy" cars --> weight bigger than 3500
#pay attention that we have no idea about the measurement unit used...
DS1 <- filter(DS, DS$weight > 3500)

#let's reorder the "heavy cars" based on their descending acceleration
DS2 <- arrange(filter(DS, DS$weight > 3500), desc(acceleration))

#examples of selection of some columns
#remove the first column
select(DS , -1)
#let's use only the 2, 3 and 4 column
select(DS, c(2,3,4))
#mixing colum index with symbolic name N.B: possible but strongly discouraged for comprehensiility reasons...
select(DS, c(mpg,5))

#LEt's compute soem basic statistics --> number of car for each year with the "table" function
table(DS$`model year`)
  
#let's create two additional columns depending on data in the current tibble: a4w that is the ration between acceleration and weight (sort of power) and a "binary flag" for cars pre 1975
TTT <- mutate(DS, a4w = acceleration/weight, pre75 = as.numeric(DS$`model year`<75))
View(TTT)

#as before with the table, but we can decide the type of aggregations we want, not limited only to the counting...
summarise(group_by(DS, `model year`), mean_displ = mean(displacement), sd_displ = sd(displacement), n =n())

#compose arrange and summarise to reorder the summarisation by the number of "version" (or better yearly models) of each car name
arrange(summarise(group_by(DS, `car name`), n =n()), desc(n))

#let's import another dataset, with the costs of oldies
#this call shows a little bit more soem of the parameters available...
Car_cost <- read_delim("C:/Users/JumpStart/switchdrive/teaching/2020_02_HS2020/DASB/SW02/car_197x_costs_2.csv", 
                       delim = "-", escape_double = FALSE, na = "NA", 
                       trim_ws = TRUE,
                       col_types = cols(
                         `Car Name` = col_factor(),
                         Price = col_character(),
                         Location = col_character(),
                         `Matriculation Year` = col_integer()
                       ))

View(Car_cost)

#again, we transform the the data.frame in tibble
Car_cost <- as_tibble(Car_cost)
Car_cost
#we manipulate the tibble by applying functions and saving it back tothe same object
# the () around works as a call to the print method
#we tranform the location from string (set of chars) into a factor (-> categorical)
print(Car_cost <- mutate(Car_cost, Location = sapply(Location,as.factor)))
#we tranform the cost in integer: before we remove the $ sign and then remove the "," used as thausends separator 
(Car_cost <- mutate(Car_cost, Price =  as.integer(str_replace(str_sub(Price, 2, -1),",",""))))

#we cahnge then the column name to lower chars
Car_cost <- mutate(Car_cost, `Car Name` = tolower(Car_cost$`Car Name`))
#and make the year by only using the last 2 numbers (1973 --> 73)
Car_cost <- mutate(Car_cost, `Matriculation Year` = Car_cost$`Matriculation Year` %% 100)

#for security, we remove the object...
rm(Merged_table)

#we merge the records sharing the car name and matriculation/model year (only common record)
(Merged_table1 <- inner_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
View(Merged_table1)


#we merge the records sharing the car name and matriculation/model year (maintaing all the entries in the first table (DS))
(Merged_table2 <- left_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
View(Merged_table2)

nr <- as_tibble(c(2,"Test",TRUE))
nr <- 234

#we merge the records sharing the car name and matriculation/model year (maintaing all the entries, also when not having any match
(Merged_table3 <- full_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
View(Merged_table3)
