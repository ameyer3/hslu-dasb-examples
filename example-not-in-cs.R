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
View(DS) # opens window with table



data <- data.frame(a = 1:3, b = letters[1:3], c = Sys.Date() - 1:3)
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
V4_xx <- matrix(c(1,1,2,3,1,2,3,5,2,3,5,8),ncol=3)
cbind(V4_xx, c(9,9)) # needsto be multiple otherwise warning, but does work, just recycles

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
