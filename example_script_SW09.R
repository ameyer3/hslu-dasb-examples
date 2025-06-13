#LECTURE9 --> exploring how to extend the linear model for forecasting

library(modelr)
library(tidyverse)

#lm.test <- lm(log2(y) ~ sin(x1) + I(cos(sd)^2) + sin(rep), ds_test)


##################### SIM3: Comparison of different models ########################################################

#simple simulated dataset
sim3
summary(sim3)
View(sim3)

#print it out using 3 dimensions: the categorical is represented using the color 
ggplot(data=sim3)+geom_point(aes(x=x1,y=y,color=x2)) + scale_x_discrete()

#let's force x1 to be consider a factor
ggplot(data=sim3)+geom_point(aes(x=as.factor(x1),y=y,color=x2))

#let's see with faceting if we can observe differences wrt to the smoothed interpolation
ggplot(data=sim3, mapping=aes(x=x1,y=y))+
  geom_point(aes(color=as.factor(rep))) + 
  facet_wrap(~x2) + 
  geom_smooth(method = lm, se = FALSE)

# for convenient window arrangement in plotting
library(gridExtra) 

# Create training and test data
set.seed(99)
train.data <- sample_frac(sim3, 0.7) # select 70% random samples
test.data <- setdiff(sim3,train.data)

#print train (BLUE) and test (RED) data, wrt the two different variables x1 and x2
p1 <- ggplot() + geom_point(data = train.data, aes(x=as.factor(x1), y=y), color='steelblue3') + 
  geom_point(data = test.data, aes(x=as.factor(x1), y=y), color='darkred', size=2) 
p2 <- ggplot() + geom_point(data = train.data, aes(x=x2, y=y), color='steelblue3') + 
  geom_point(data = test.data, aes(x=x2, y=y), color='darkred', size=2) 
grid.arrange(p1, p2, nrow = 1)

mod_parz_x1 <- lm(y ~ x1,data=train.data)
mod_parz_x2 <- lm(y ~ x2,data=train.data)

#create a data grid with all the prediction in every point
sim3_pred_parz <- sim3 %>% data_grid(x1, x2) %>%
  gather_predictions(mod_parz_x1, mod_parz_x2)

#use the two models prediction to compute the trend lines
# x1 gives us a LM for all the x2 classes, 
# whether x2 gives us 4 constants, one for each value of x2
ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = sim3_pred_parz, aes(y = pred)) +
  facet_wrap(~ model)

#let's consider them both, but INDEPENDENTLY
mod_indep <- lm(y ~ x1 + x2,data=train.data)
summary(mod_indep)
#let's also them interact, by using the "*" operatror in the formula insted of the "+"
mod_interaction <- lm(y ~ x1 * x2,data=train.data)
summary(mod_interaction)

# x1 is continuous, then it takes one parameter, 
# x2 is a factor (discrete), so it takes #level-1 factors,
# the interaction takes 1*3 parameters (#x1*#x3)

# F(x1,x2)= a0 + a1 * x1 + a2 * [x2_b] + a3 * [x2_c] + a4 * [x2_d] 
#  + a5 * x1 * [x2_b] + a6 * x1 * [x2_c] + a7 * x1 * [x2_d]

#eg: --> if x2 is equal to a
# F(x1,'a')= a0 + a1*x1 
#--> if x2 is equal to b
# F(x1,'b')= a0 + a2 + (a1 + a5) * x1 
#--> if x2 is equal to c
# F(x1,'c')= a0 + a3 + (a1 + a6) * x1
#--> if x2 is equal to d
# F(x1,'d')= a0 + a4 + (a1 + a7) * x1

#let's add also te rep variable as predictor, both in linear and in interaction modality
mod_over1 <- lm(y ~ x1 * x2 + rep,data=train.data)
mod_over2 <- lm(y ~ x1 * x2 * rep,data=train.data)

#let's gather the predictions for all this models, on the grid created before...
sim3_pred_final <- sim3 %>% data_grid(x1, x2, rep) %>%
  gather_predictions(mod_indep, mod_interaction, mod_over1, mod_over2)

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = sim3_pred_final, aes(y = pred)) +
  facet_grid(rep~ model)

#let's now compare the models, with the ANOVA function
anova(mod_parz_x1,mod_parz_x2,mod_indep,mod_interaction,mod_over1,mod_over2)
summary(mod_over2)

##################### Base transformation ########################################################

# working with an external datasource
#dir="C:/Users/JumpStart/switchdrive/teaching/2020_01_FS2021/DASB/SW09/"
#setwd(dir)
library(readr)
BMI_ds <- read_csv("500_Person_Gender_Height_Weight_Index.csv", 
                   col_types = cols(Gender = col_factor(levels = c("Male", "Female")), 
                                    Height = col_integer(), 
                                    Index = col_factor(levels = c("0", "1", "2", "3", "4", "5")), 
                                    Weight = col_integer()))

# Gender : Male / Female
# Height : Number (cm)
# Weight : Number (Kg)
# Index :
#  0 - Extremely Weak
#  1 - Weak
#  2 - Normal
#  3 - Overweight
#  4 - Obesity
#  5 - Extreme Obesity

View(BMI_ds)

#let's compute the BMI using the "real" formula and add it to the tibble (data structure)
BMI_ds <- BMI_ds %>% mutate(BMI =  Weight / (Height/100)^2) # real formula BMI = Weight (Kg) / Height(m)^2

BMI_ds %>% group_by(Index) %>% summarise(min=min(BMI), max=max(BMI), mean=mean(BMI), sd=sd(BMI), nr=n()) %>% ungroup()

#let's see the effect of the gender (M/F)
BMI_ds %>% group_by(Gender, Index) %>% summarise(min=min(BMI), max=max(BMI), mean=mean(BMI), sd=sd(BMI), 
                                                 lim_inf=mean-2*sd, lim_sup=mean+2*sd, nr=n()) %>% ungroup()

ggplot(data=BMI_ds, mapping=aes(x=Height,y=Weight)) + 
  geom_point(aes(color=Gender)) + 
  facet_wrap(.~Index) + 
  geom_smooth(method = lm, se = FALSE)

library(nnet) # for using multiclass classification
mod_lin <- multinom(Index ~ Height + Weight, data=BMI_ds)
#let's use the second power of Height, letting the library to decide which base to use --> poly()
#and let's also consider the interaction with Weight
mod_non_lin <- multinom(Index ~ poly(Height,2) * Weight, data=BMI_ds) 
#let's force the real formula, where we know that ONLY then second power of Height is relevant
mod_non_lin2 <- multinom(Index ~ I(Height^2) * Weight, data=BMI_ds) # real formula BMI = Weight / Height^2

#let's create a Height and Weight range, using a certain number of values (n)
# covering the full range of values included in the original ranges
Height_range <- seq_range(BMI_ds$Height, n=150, pretty=TRUE)
Weight_range <- seq_range(BMI_ds$Weight, n=300, pretty=TRUE)

#let's agther the prediction for the three models
BMI_ds_for <- BMI_ds %>% data_grid(Height = Height_range, Weight = Weight_range) %>%
  gather_predictions(mod_lin, mod_non_lin, mod_non_lin2)

ggplot(data=BMI_ds, aes(x=Height,y=Weight)) + 
  geom_point(aes(color=Index)) + 
  geom_tile(data = BMI_ds_for, aes(fill=pred), alpha = 0.3, show.legend = F) +
  facet_wrap(.~model) + 
  theme_bw(base_size = 15) +
  ggtitle('Model comparisons') +
  coord_fixed(ratio = 0.8) + 
  theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none')

# let's use a regression on the actual BMI value
mod_lin_regression <- lm(BMI ~ I(Height^2) * Weight, data=BMI_ds)

predictions <- predict(mod_lin_regression,BMI_ds)

BMI_ds_enriched <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

#let's add an interaction with the gender
mod_lin_regression2 <- lm(BMI ~ I(Height^2) * Weight * Gender, data=BMI_ds)

predictions2 <- predict(mod_lin_regression2,BMI_ds)

BMI_ds_enriched2 <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched2 %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched2, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

anova(mod_lin_regression,mod_lin_regression2)

#let's consider the gender as indipendent factor only
mod_lin_regression3 <- lm(BMI ~ I(Height^2) * Weight + Gender, data=BMI_ds)

predictions3 <- predict(mod_lin_regression3,BMI_ds)

BMI_ds_enriched3 <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched3 %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched3, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

anova(mod_lin_regression,mod_lin_regression3,mod_lin_regression2)

##################### Function application ########################################################

##change in the variable space by function application
library(ggplot2)
View(diamonds)

cor(diamonds$carat,diamonds$price)

ggplot(data=diamonds, aes(x=carat,y=price)) + geom_point(color="blue")
ggplot(data=diamonds, aes(x=carat)) + geom_histogram(color="blue")
ggplot(data=diamonds, aes(x=price)) + geom_histogram(color="red")
ggplot(data=diamonds, aes(x=log(price))) + geom_histogram(color="red")
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50)

ggplot(data=diamonds, aes(x=log(carat),y=log(price))) + geom_point(color="red")
ggplot(diamonds, aes(log(carat), price)) +  geom_hex(bins = 50)
ggplot(diamonds, aes(carat, log(price))) +  geom_hex(bins = 50)
ggplot(diamonds, aes(log(carat), log(price))) +  geom_hex(bins = 50)

library(tidyverse)
diamonds2 <- diamonds %>% mutate(log_price = log(price), log_carat = log(carat))

mod_diam_1 <- lm(price ~ carat, data=diamonds)
mod_diam_1_resid <- diamonds %>% mutate(pred = predict(mod_diam_1,diamonds), delta = price - pred)
mod_diam_1_resid
#ISSUE: we have negative prices!!! HOW to fix it?
summary(mod_diam_1)


#let's remove the intercept : force it to pass from the origin of the axes, using the "-1" in the formula
mod_diam_1_bis <- lm(price ~ carat - 1, data=diamonds)
mod_diam_1_bis_resid <- diamonds %>% mutate(pred = predict(mod_diam_1_bis,diamonds), delta = price - pred)
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_1_resid,aes(carat,pred), color = "red", size = 1, alpha=0.5)+ 
  geom_line(data=mod_diam_1_bis_resid,aes(carat,pred), color = "orange", size = 1, alpha=0.5)

summary(mod_diam_1_bis)

mod_diam_2 <- lm(log(price) ~ log(carat), data=diamonds)
mod_diam_2_bis <- lm(log_price ~ log_carat, data=diamonds2)
mod_diam_2_resid <- diamonds %>% mutate(pred = predict(mod_diam_2,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_2_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)+ 
  geom_line(data=mod_diam_1_resid,aes(carat,pred), color = "red", size = 1, alpha=0.5)+ 
  geom_line(data=mod_diam_1_bis_resid,aes(carat,pred), color = "orange", size = 1, alpha=0.5)


#let's analyze the residuals of this model,
ggplot(mod_diam_2_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) 

#let's analyze the residuals of this model, wrt the other variables
ggplot(mod_diam_2_resid, aes(cut, delta_log)) + geom_boxplot()
# ggplot(mod_diam_2_resid, aes(cut, delta_log)) + geom_jitter()
ggplot(mod_diam_2_resid, aes(color, delta_log)) + geom_boxplot()
ggplot(mod_diam_2_resid, aes(clarity, delta_log)) + geom_boxplot()
ggplot(mod_diam_2_resid, aes(depth, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(x, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(y, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(z, delta_log)) + geom_point()

mod_diam_3 <- lm(log(price) ~ log(carat) + cut + color + clarity, data=diamonds)

anova(mod_diam_2,mod_diam_3)

mod_diam_3_resid <- diamonds %>% mutate(pred = predict(mod_diam_3,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_3_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)

#let's analyze the residuals of this model,
ggplot(mod_diam_3_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) + 
  geom_hline(yintercept=0,color="red",alpha=0.5,size=2)

ggplot(mod_diam_3_resid, aes(depth, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(x, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(y, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(z, delta_log)) + geom_point()

#as a verification, let's see how the model with also these variables as predictor performs
mod_diam_4 <- lm(log(price) ~ log(carat) + cut + color + clarity + depth + x +y + z, data=diamonds)

anova(mod_diam_2,mod_diam_3, mod_diam_4)

mod_diam_4_resid <- diamonds %>% mutate(pred = predict(mod_diam_4,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_4_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)

#let's analyze the residuals of this model,
ggplot(mod_diam_4_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) + 
  geom_hline(yintercept=0,color="red",alpha=0.5,size=2)

