test <- c(1,2,3,4,5)
test[-1:-2] # 3 4 5 (without first and second)
test[-1:-3] # 4 5 (without first to third), can't mix pos and neg

#alert for not multiple lenghts
1:5 # = 1 2 3 4 5
T1A <- c(10,20,30) + 1:5 # 11 22 33 14 25
T1A[c(2,4,5)] # (can also be T1A[2,4,5]) # 22 14 25

t1b[2:3] == t1b[-1:-2] # FALSE TRUE
t1b[-1:-2] # = 13
t1b[2:3] # = 12 13

V3_xx <- matrix(c(1,1,2,1,2,3,2,3,5),ncol=3)
V3_xx # filled column after column
# 1 1 2
# 1 2 3
# 2 3 5
V4_xx <- matrix(c(1,1,2,3,1,2,3,5,2,3,5,8),ncol=3)
V4_xx_bis <- matrix(c(1,1,2,1,2,3,2,3,5,3,5,8),ncol=3)
V4_xx_bis # 12 elements and 3 cols = 4 rows
V4_xx[2:4,]/V3_xx # (but not V4_xx[1:4,]/V3_xx or V4_xx/V3_xx ==> Error in V4_xx[1:4, ]/V3_xx : non-conformable arrays)
# TODO: DO MATRIX HAVE TO BE EXACT SAME DIMENOSNS?

#extend the matrix with a new column
cbind(V3_xx, c(9, 9, 9))
V4_xx[2:3] # first column
V4_xx[2:3,] # all columns

#Lists (more flexible objects) can store objects of different classes and different dimensions
l.1 <- list(A = "a", num.vec = 10:5, mat1 = V3_xx, V4_xx_bis) # kind of like a dictionary here
l.1$num.vec
(l.1$mat1==l.1[[3]]) 
l.1[[4]][1:2,3]
l.1[4]





#let's rename the columns
names(DS) <- c("mpg","cylinders")

View(DS) # opens window with table
data <- data.frame(a = 1:3, b = letters[1:3], c = Sys.Date() - 1:3)
data
#>   a b          c
#> 1 1 a 2025-03-18
#> 2 2 b 2025-03-17
#> 3 3 c 2025-03-16
as_tibble(data)
#> # A tibble: 3 × 3
#>       a b     c         
#>   <int> <chr> <date>    
#> 1     1 a     2025-03-18
#> 2     2 b     2025-03-17
#> 3     3 c     2025-03-16
#tibble() does much less than data.frame():
# it never changes the type of the inputs (e.g. it keeps list columns as is),
# it never changes the names of variables, 
# it only recycles inputs of length 1, 
# it never creates row.names()



#reorder only heavy ones by their descending acceleration
DS2 <- arrange(filter(DS, DS$weight > 3500), desc(acceleration))

select(DS , -1) #remove the first column
select(DS, c(2,3,4)) #only the 2, 3 and 4 column
#mixing colum index with symbolic name: possible but strongly discouraged for comprehensibility
select(DS, c(mpg,5))

#number of cars for each year
table(DS$`model year)

#let's create two additional columns depending on data in the current tibble
TTT <- mutate(DS, a4w = acceleration/weight, pre75 = as.numeric(DS$`model year`<75))
summarise(group_by(DS, `model year`), mean_displ = mean(displacement), sd_displ = sd(displacement), n =n())
#If you use summarise() on a data frame without grouping it first, it treats the entire data frame as one single group. The result is always a data frame with just one row.
# Split: group_by() splits your data into distinct groups.
# Apply: summarise() calculates summary statistics for each group independently.
# Combine: The results are combined into a new, smaller data frame.

arrange(summarise(group_by(DS, `car name`), n =n()), desc(n))

diamonds2 <- diamonds %>% mutate(log_price = log(price), log_carat = log(carat))
print(Car_cost <- mutate(Car_cost, Location = sapply(Location,as.factor)))
(Car_cost <- mutate(Car_cost, Price =  as.integer(str_replace(str_sub(Price, 2, -1),",",""))))
Car_cost <- mutate(Car_cost, `Car Name` = tolower(Car_cost$`Car Name`)) # all values to lower
Car_cost <- mutate(Car_cost, `Matriculation Year` = Car_cost$`Matriculation Year` %% 100) # last 2 numbers (1973 --> 73)
(Merged_table1 <- inner_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
(Merged_table2 <- left_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
(Merged_table3 <- full_join(DS,Car_cost,by=c("car name"="Car Name","model year"="Matriculation Year")))
BMI_ds <- BMI_ds %>% mutate(BMI =  Weight / (Height/100)^2)
BMI_ds %>% group_by(Index) %>% summarise(min=min(BMI), max=max(BMI), mean=mean(BMI), sd=sd(BMI), nr=n()) %>% ungroup()


? #opens like a man page window
help() # kind of like ?
summary(Default)
#  default    student       balance           income     
#  No :9667   No :7056   Min.   :   0.0   Min.   :  772  
#  Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
#                        Median : 823.6   Median :34553  
#                        Mean   : 835.4   Mean   :33517  
#                        3rd Qu.:1166.3   3rd Qu.:43808  
#                        Max.   :2654.3   Max.   :73554  


# Starting from the Rabbit dataset, the %>% is the piping, meaning the output of the command is passed to the next command
# grouping the entries using the treatment (MDL vs. Control) and the dose given, and
# creating a summary using the mean function (so you will have one record/row for each combination of treatment and dose).
Summarized_Rabbit <- Rabbit %>%
  group_by(Treatment, Dose) %>%
  summarise(mean_BPchange = mean(BPchange))
#alternatively, you can do it in distinct operations
t1 <- group_by(Rabbit, Treatment, Dose) 
Summarized_Rabbit <- summarize(t1, mean_BPchange = mean(BPchange))





heatmap(x = Boston.corr, col = palette, symm = TRUE)
correlation <- Boston.corr[,'medv']
col <- colnames(Boston.corr)  
medv.corr <- tibble(col, correlation)
ggplot(data=medv.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)

lm2 <- lm(medv~lstat+rm,data=Boston)
lm.red2 <- lm(medv~.-age-indus-crim-chas,data=Boston)
summary(lm.red2)
# > summary(lm.red2)

# Call:
# lm(formula = medv ~ . - age - indus - crim - chas, data = Boston)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -12.8917  -2.7329  -0.4988   1.8547  26.6433 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  35.459724   5.158054   6.875 1.87e-11 ***
# zn            0.041396   0.013737   3.013 0.002715 ** 
# nox         -15.502932   3.583879  -4.326 1.84e-05 ***
# rm            3.879580   0.414180   9.367  < 2e-16 ***
# dis          -1.451648   0.187926  -7.725 6.26e-14 ***
# rad           0.252412   0.061778   4.086 5.12e-05 ***
# tax          -0.012360   0.003427  -3.606 0.000342 ***
# ptratio      -0.968703   0.131248  -7.381 6.69e-13 ***
# black         0.010842   0.002705   4.008 7.06e-05 ***
# lstat        -0.555124   0.047699 -11.638  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 4.832 on 496 degrees of freedom
# Multiple R-squared:  0.7289,    Adjusted R-squared:  0.724 
# F-statistic: 148.2 on 9 and 496 DF,  p-value: < 2.2e-16
#let's compare the models obtained, in a structured way
anova(lm1, lm2, lm3, lm.red2, lm.red, lm.tot)
# > anova(lm1, lm2, lm3, lm.red2, lm.red, lm.tot)
# Analysis of Variance Table

# Model 1: medv ~ lstat
# Model 2: medv ~ lstat + rm
# Model 3: medv ~ lstat + rm + ptratio
# Model 4: medv ~ (crim + zn + indus + chas + nox + rm + age + dis + rad + 
#     tax + ptratio + black + lstat) - age - indus - crim - chas
# Model 5: medv ~ (crim + zn + indus + chas + nox + rm + age + dis + rad + 
#     tax + ptratio + black + lstat) - age - indus
# Model 6: medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
#     tax + ptratio + black + lstat
#   Res.Df   RSS Df Sum of Sq        F    Pr(>F)    
# 1    504 19472                                    
# 2    503 15439  1    4033.1 179.1055 < 2.2e-16 ***
# 3    502 13728  1    1711.3  75.9985 < 2.2e-16 ***
# 4    496 11581  6    2147.0  15.8913 < 2.2e-16 ***
# 5    494 11081  2     499.6  11.0932  1.94e-05 ***
# 6    492 11079  2       2.6   0.0573    0.9443    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# generalized linear model, classification, logistic
glm1 <- glm(default~balance, data=training.data, family="binomial")
# gives response variable scale, probabilities, sigmoid, without =response we get linear predictor scale 
pred.test.probs <- predict(glm1, test.data, type = "response")
# Confusion matrix  
table(test.data$default, pred.test.classes)
# Calculating the validation error rate (percentage of incorrectly classified samples) as an estimate of the test error rate
mean(pred.test.classes != test.data$default)
# more than one predictor
glm3 <- glm(default~.-income, family = "binomial", data = training.data) 
summary(glm3)

# FAlse without the grey little shadow (only blue line), true is with
ggplot(data=sim3, mapping=aes(x=x1,y=y))+
  geom_point(aes(color=as.factor(rep))) + 
  facet_wrap(~x2) + 
  geom_smooth(method = lm, se = FALSE)

train.data <- sample_frac(sim3, 0.7) # select 70% random samples
test.data <- setdiff(sim3,train.data)

sim3_pred_parz <- sim3 %>% data_grid(x1, x2) %>%
  gather_predictions(mod_parz_x1, mod_parz_x2)
  # to plot the models together

# Interaction
mod_interaction <- lm(y ~ x1 * x2,data=train.data)

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

# multiclass classification
mod_lin <- multinom(Index ~ Height + Weight, data=BMI_ds)
mod_non_lin <- multinom(Index ~ poly(Height,2) * Weight, data=BMI_ds) 
#let's force the real formula, where we know that ONLY then second power of Height is relevant
mod_non_lin2 <- multinom(Index ~ I(Height^2) * Weight, data=BMI_ds)


#let's remove the intercept : force it to pass from the origin of the axes, using the "-1" in the formula
mod_diam_1_bis <- lm(price ~ carat - 1, data=diamonds)