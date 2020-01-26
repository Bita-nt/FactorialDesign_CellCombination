

data1 = read.csv(file.choose())
str(data1)
# 'data.frame':	16 obs. of  6 variables:
# $ A   : int  1 -1 1 1 -1 1 1 -1 -1 1 ...
# $ B   : int  -1 1 1 -1 -1 1 1 1 -1 1 ...
# $ C   : int  -1 -1 -1 -1 1 1 1 1 1 -1 ...
# $ D   : int  -1 -1 -1 1 1 1 -1 -1 -1 1 ...
# $ E   : int  -1 -1 1 1 1 1 -1 1 -1 -1 ...
# $ Cell: int  61 44 39 51 99 7 17 8 84 40 ...
View(data1)
data1$A = as.factor(data1$A)
data1$B = as.factor(data1$B)
data1$C = as.factor(data1$C)
data1$D = as.factor(data1$D)
data1$E = as.factor(data1$E)

# View(data1)
names(data1)
# [1] "A"    "B"    "C"    "D"    "E"    "Cell"


#Boxplots:
par(mfrow = c(1, 5))
boxplot(Cell ~ A, data = data1, main = "Effect of A", range = 0,
        xlab = "A", ylab = "Cell", las = 1)
boxplot(Cell ~ B, data = data1, main = "Effect of B", range = 0,
        xlab = "B", ylab = "Cell", las = 1)
boxplot(Cell ~ C, data = data1, main = "Effect of C", range = 0,
        xlab = "C", ylab = "Cell", las = 1)
boxplot(Cell ~ D, data = data1, main = "Effect of D", range = 0,
        xlab = "D", ylab = "Cell", las = 1)
boxplot(Cell ~ E, data = data1, main = "Effect of E", range = 0,
        xlab = "E", ylab = "Cell", las = 1)

# COMMENT: There is variability in the mean response at different levels
# of factors especially in B.



###### One-half fraction of 2^5 experiments:
install.packages('FrF2')
library(FrF2)

aliases(lm(Cell~A*B*C*D*E, data = data1))
# 
# A = B:C:D:E
# B = A:C:D:E
# C = A:B:D:E
# D = A:B:C:E
# E = A:B:C:D
# A:B = C:D:E
# A:C = B:D:E
# B:C = A:D:E
# A:D = B:C:E
# B:D = A:C:E
# C:D = A:B:E
# A:E = B:C:D
# B:E = A:C:D
# C:E = A:B:D
# D:E = A:B:C

#Hence the defining relation is obtained using the first row as I = ABCDE.
aliases(lm(Cell~A*B*C*D, data = data1))
# [1] no aliasing in the model


#project the fractional design to a full 2^4 factorial design with A,B,C,D.
#Then the factor E is aliased with the effect ABCD.
model1.1 = lm(Cell ~ A*B*C*D, data = data1)

DanielPlot(model1.1, half =TRUE, code = T, autolab = T, alpha = 0.05)
#autolab = T provides Lenth's method.
#COMMENT: Only effect B (actually aliased with ACDE) and BC (aliased with ADE)
# are significant by Lenth's method.



# To estimate the main effects, we have to assume that all the 
#four-factor effects are negligible.

model1.2 = lm(Cell~B+B:C, data = data1)
anova(model1.2)
# Analysis of Variance Table
# 
# Response: Cell
# Df Sum Sq Mean Sq F value    Pr(>F)    
#   B          1 8326.6  8326.6 110.438 2.089e-07 ***
#   B:C        2 2280.6  1140.3  15.124 0.0005251 ***
#   Residuals 12  904.8    75.4                      
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model1.3 = lm(Cell~A*B*C, data = data1)
anova(model1.3)
# Analysis of Variance Table
# 
# Response: Cell
# Df Sum Sq Mean Sq  F value    Pr(>F)    
# A          1  126.6   126.6   1.7624   0.22096    
# B          1 8326.6  8326.6 115.9487 4.873e-06 ***
#   C          1    0.6     0.6   0.0078   0.93165    
# A:B        1  126.6   126.6   1.7624   0.22096    
# A:C        1   76.6    76.6   1.0661   0.33202    
# B:C        1 2280.1  2280.1  31.7502   0.00049 ***
#   A:B:C      1    0.6     0.6   0.0078   0.93165    
# Residuals  8  574.5    71.8                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Seems consistent. Let's test the full and reduced models:

anova(model1.2, model1.3)

# Model 1: Cell ~ B + B:C
# Model 2: Cell ~ A * B * C
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     13 905.31                           
# 2      8 574.50  5    330.81 0.9213 0.5137
#'B is alised with a 4-factor effect ACDE, and BC is aliased with ADE.
#'If we assume that 3-factor effects and above are negligible, 
#'we can estimate the two effects.
#'


summary(model1.2)
# Call:
#   lm(formula = Cell ~ B + B:C, data = data1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -10.500  -6.625   1.000   3.875  15.500 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   59.250      4.342  13.647 1.14e-08 ***
#   B1           -21.750      6.140  -3.542  0.00405 ** 
#   B-1:C1        24.250      6.140   3.950  0.00193 ** 
#   B1:C1        -23.500      6.140  -3.827  0.00241 ** 
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.683 on 12 degrees of freedom
# Multiple R-squared:  0.9214,	Adjusted R-squared:  0.9018 
# F-statistic:  46.9 on 3 and 12 DF,  p-value: 6.674e-07



par(mfrow=c(1,2))
plot(model1.2, which = 1)
plot(model1.2, which = 2)


plot.design(Cell~A*B*C*D*E, data = data1, main = "Main Effects")


######################## part 2


data2 = read.csv(file.choose())
View(data2)


data2$A = as.factor(data2$A)
data2$B = as.factor(data2$B)
data2$C = as.factor(data2$C)
data2$D = as.factor(data2$D)
data2$E = as.factor(data2$E)

str(data2)

View(data2)
names(data2)



newdata = rbind(data1 , data2)
str(newdata)
# 'data.frame':	32 obs. of  6 variables:

par(mfrow = c(1, 5))
boxplot(Cell ~ A, data = newdata, main = "Effect of A", range = 0,
        xlab = "A", ylab = "Cell", las = 1)
boxplot(Cell ~ B, data = newdata, main = "Effect of B", range = 0,
        xlab = "B", ylab = "Cell", las = 1)
boxplot(Cell ~ C, data = newdata, main = "Effect of C", range = 0,
        xlab = "C", ylab = "Cell", las = 1)
boxplot(Cell ~ D, data = newdata, main = "Effect of D", range = 0,
        xlab = "D", ylab = "Cell", las = 1)
boxplot(Cell ~ E, data = newdata, main = "Effect of E", range = 0,
        xlab = "E", ylab = "Cell", las = 1)


# COMMENT: There is variability in the mean response at different levels
# of factor C more than others.

plot.design(Cell~A*B*C*D*E, data = newdata, main = "Main Effects")



alias(Cell ~ A*B*C*D*E, data = newdata)
# No aliasing in the model. 

alias(Cell ~ A*B*C*D, data = newdata)
# No aliasing in the model.

DanielPlot(lm(Cell~A*B*C*D*E,data = newdata),
           code = T, autolab = T, alpha = 0.05, half = T)


DanielPlot(lm(Cell~A*B*C,data = newdata), 
             code = T, autolab = T, alpha = 0.05, half = T)

#' I will project the factorial design to a full 2^3 factorial design
#' with A,B and C. 


anova(lm(Cell~A*B*C, data = newdata))
anova(lm(Cell~A*B*D, data = newdata))
anova(lm(Cell~B*C*D, data = newdata))
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# A          1   413.3   413.3   6.4712   0.01782 *  
#   B          1 13243.8 13243.8 207.3729 2.630e-13 ***
#   C          1     5.3     5.3   0.0827   0.77615    
# A:B        1     2.5     2.5   0.0396   0.84387    
# A:C        1   215.3   215.3   3.3709   0.07878 .  
# B:C        1  4301.3  4301.3  67.3500 2.003e-08 ***
#   A:B:C      1    30.0    30.0   0.4702   0.49945    
# Residuals 24  1532.7    63.9                       
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > anova(lm(Cell~A*B*D, data = newdata))
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# A          1   413.3   413.3  1.7034    0.2042    
# B          1 13243.8 13243.8 54.5877 1.252e-07 ***
#   D          1    69.0    69.0  0.2845    0.5987    
# A:B        1     2.5     2.5  0.0104    0.9195    
# A:D        1    57.8    57.8  0.2382    0.6300    
# B:D        1   108.8   108.8  0.4484    0.5095    
# A:B:D      1    26.3    26.3  0.1083    0.7449    
# Residuals 24  5822.8   242.6                      
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > anova(lm(Cell~B*C*D, data = newdata))
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# B          1 13243.8 13243.8 159.2239 4.386e-12 ***
#   C          1     5.3     5.3   0.0635    0.8032    
# D          1    69.0    69.0   0.8299    0.3714    
# B:C        1  4301.3  4301.3  51.7123 1.973e-07 ***
#   B:D        1   108.8   108.8   1.3078    0.2641    
# C:D        1    19.5    19.5   0.2348    0.6324    
# B:C:D      1     0.3     0.3   0.0034    0.9541    
# Residuals 24  1996.3    83.2                       
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


model2.2 = lm(Cell~A+B+B:C, data = newdata)
model2.4 = lm(Cell~A+B, data = newdata)
anova(model2.2, model2.4)
# Analysis of Variance Table
# 
# Model 1: Cell ~ A + B + B:C
# Model 2: Cell ~ A + B
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     27 1780.6                                  
# 2     29 6087.2 -2   -4306.6 32.651 6.209e-08 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model2.2 = lm(Cell~A+B+B:C, data = newdata)
model2.5 = lm(Cell~B+B:C, data = newdata)
anova(model2.2, model2.5)
# Analysis of Variance Table
# 
# Model 1: Cell ~ A + B + B:C
# Model 2: Cell ~ B + B:C
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     27 1780.6                              
# 2     28 2193.9 -1   -413.28 6.2668 0.01865 *
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#' Adding the block effect since the two data sets are not from one day.
Block = c(rep(1, 16), rep(2, 16))

model2.3 = lm(Cell~Block+A+B+B:C, data = newdata)

anova(model2.2, model2.3)
# Analysis of Variance Table
# 
# Model 1: Cell ~ A + B + B:C
# Model 2: Cell ~ Block + A + B + B:C
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     27 1780.6                           
# 2     26 1699.3  1    81.281 1.2436  0.275

#According to the anova test, blocking is not effective.

anova(model2.2)
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# A          1   413.3   413.3   6.2668   0.01865 *  
#   B          1 13243.8 13243.8 200.8218 5.053e-14 ***
#   B:C        2  4306.6  2153.3  32.6512 6.209e-08 ***
#   Residuals 27  1780.6    65.9                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


coefficients(model2.2)

par(mfrow=c(1,2))
plot(model2.2, which = 1)
plot(model2.2, which = 2)


model2.4 = lm(sqrt(Cell)~A+B+B:C, data = newdata)

anova(model2.4)
# Analysis of Variance Table
# 
# Response: sqrt(Cell)
# Df Sum Sq Mean Sq  F value    Pr(>F)    
# A          1  2.726   2.726   4.6075   0.04097 *  
#   B          1 77.460  77.460 130.9341 7.310e-12 ***
#   B:C        2 28.865  14.432  24.3954 8.884e-07 ***
#   Residuals 27 15.973   0.592                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

coefficients(model2.2)

par(mfrow=c(1,2))
plot(model2.2, which = 1)
plot(model2.2, which = 2)


####################### part 3
#Blocking in replicated factorial design (4 blocks, 2 replicates)
data3 = read.csv(file.choose())
View(data3)

data3$A = as.factor(data3$A)
data3$B = as.factor(data3$B)
data3$C = as.factor(data3$C)
data3$D = as.factor(data3$D)
data3$E = as.factor(data3$E)

x1=x2=x3=x4=x5=rep(1,64)
x1[data3$A == '-1'] = -1
x2[data3$B == '-1'] = -1
x3[data3$C == '-1'] = -1
x4[data3$D == '-1'] = -1
x5[data3$E == '-1'] = -1

ABCDE = x1*x2*x3*x4*x5
ABCD = x1*x2*x3*x4


Block = rep(1, 64)
Block[(data3$replicate == 1)&(ABCDE == -1)]=2
Block[(data3$replicate == 2)&(ABCD == 1)]=3
Block[(data3$replicate == 2)&(ABCD == -1)]=4
Block = as.factor(Block)

par(mfrow = c(1, 6))
boxplot(Cell ~ A, data = data3, main = "Effect of A", range = 0,
        xlab = "A", ylab = "Cell", las = 1)
boxplot(Cell ~ B, data = data3, main = "Effect of B", range = 0,
        xlab = "B", ylab = "Cell", las = 1)
boxplot(Cell ~ C, data = data3, main = "Effect of C", range = 0,
        xlab = "C", ylab = "Cell", las = 1)
boxplot(Cell ~ D, data = data3, main = "Effect of D", range = 0,
        xlab = "D", ylab = "Cell", las = 1)
boxplot(Cell ~ E, data = data3, main = "Effect of E", range = 0,
        xlab = "E", ylab = "Cell", las = 1)
boxplot(Cell ~ Block, data = data3, main = "Effect of Block", range = 0,
        xlab = "Block", ylab = "Cell", las = 1)

# COMMENT: There is variability in the mean response at different levels
# of factors especially in B.

#plot of main effects
plot.design(Cell~A*B*C*D*E, data = data3, main = "Main Effects")


model3 = lm(Cell~Block+A*B*C*D*E, data = data3)
anova(model3)
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Block      3  1116.2   372.1   4.1378  0.014738 *  
#   A          1  3234.8  3234.8  35.9752 1.602e-06 ***
#   B          1 30145.6 30145.6 335.2627 < 2.2e-16 ***
#   C          1   805.1   805.1   8.9543  0.005605 ** 
#   D          1    19.1    19.1   0.2129  0.647968    
# E          1     0.8     0.8   0.0085  0.927113    
# A:B        1   695.6   695.6   7.7365  0.009412 ** 
#   A:C        1   546.4   546.4   6.0766  0.019860 *  
#   B:C        1  4505.8  4505.8  50.1106 8.684e-08 ***
#   A:D        1   425.4   425.4   4.7310  0.037922 *  
#   B:D        1     0.0     0.0   0.0002  0.989573    
# C:D        1  1198.9  1198.9  13.3334  0.001021 ** 
#   A:E        1   244.1   244.1   2.7152  0.110191    
# B:E        1   199.5   199.5   2.2189  0.147127    
# C:E        1   489.5   489.5   5.4441  0.026776 *  
#   D:E        1    34.5    34.5   0.3839  0.540381    
# A:B:C      1    70.1    70.1   0.7801  0.384385    
# A:B:D      1   112.9   112.9   1.2555  0.271697    
# A:C:D      1   534.8   534.8   5.9474  0.021096 *  
#   B:C:D      1  1130.6  1130.6  12.5743  0.001350 ** 
#   A:B:E      1   446.3   446.3   4.9631  0.033813 *  
#   A:C:E      1    37.5    37.5   0.4172  0.523403    
# B:C:E      1   185.6   185.6   2.0646  0.161458    
# A:D:E      1     0.8     0.8   0.0085  0.927113    
# B:D:E      1    11.4    11.4   0.1267  0.724476    
# C:D:E      1     9.8     9.8   0.1086  0.744103    
# A:B:C:D    1     3.1     3.1   0.0348  0.853409    
# A:B:C:E    1     0.0     0.0   0.0002  0.989573    
# A:B:D:E    1    66.0    66.0   0.7342  0.398554    
# A:C:D:E    1    28.9    28.9   0.3213  0.575186    
# B:C:D:E    1    66.0    66.0   0.7342  0.398554    
# A:B:C:D:E  1    22.8    22.8   0.2534  0.618521    
# Residuals 29  2607.6    89.9                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MS of the block is smaller than MSE, so I remove the block effect.

model3_0 = lm(Cell~x1+x2+x3+x1*x2+x1*x3+x2*x3+x1*x4
              +x3*x4+x3*x5+x1*x3*x4+x2*x3*x4+x1*x2*x5, data = data3)
anova(model3_0)
# Analysis of Variance Table
# 
# Response: Cell
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# x1         1  3234.8  3234.8  34.0251 5.148e-07 ***
#   x2         1 30145.6 30145.6 317.0890 < 2.2e-16 ***
#   x3         1   805.1   805.1   8.4689 0.0055508 ** 
#   x4         1    19.1    19.1   0.2013 0.6557545    
# x5         1     0.8     0.8   0.0081 0.9288834    
# x1:x2      1   695.6   695.6   7.3171 0.0095452 ** 
#   x1:x3      1   546.4   546.4   5.7472 0.0206321 *  
#   x2:x3      1  4505.8  4505.8  47.3942 1.359e-08 ***
#   x1:x4      1   425.4   425.4   4.4745 0.0398505 *  
#   x3:x4      1  1198.9  1198.9  12.6106 0.0008981 ***
#   x3:x5      1   489.5   489.5   5.1490 0.0279949 *  
#   x2:x4      1     0.0     0.0   0.0002 0.9898268    
# x1:x5      1   244.1   244.1   2.5680 0.1158904    
# x2:x5      1   199.5   199.5   2.0986 0.1542151    
# x1:x3:x4   1   534.8   534.8   5.6250 0.0219476 *  
#   x2:x3:x4   1  1130.6  1130.6  11.8927 0.0012162 ** 
#   x1:x2:x5   1   446.3   446.3   4.6941 0.0354828 *  
#   Residuals 46  4373.2    95.1                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(model3_0)
# (Intercept)          x1          x2          x3          x4          x5       x1:x2 
# 48.421875   -7.109375  -21.703125    3.546875   -0.546875   -0.109375   -3.296875 
# x1:x3       x2:x3       x1:x4       x3:x4       x3:x5       x2:x4       x1:x5 
# -2.921875   -8.390625   -2.578125    4.328125    2.765625    0.015625   -1.953125 
# x2:x5    x1:x3:x4    x2:x3:x4    x1:x2:x5 
# 1.765625   -2.890625    4.203125   -2.640625 



par(mfrow=c(1,2))
plot(model3_0, which = 1)
plot(model3_0, which = 2)



###################### part 4
data4 = read.csv(file.choose())
View(data4)
str(data4)


data4$A = as.factor(data4$A)
data4$B = as.factor(data4$B)
data4$C = as.factor(data4$C)
data4$D = as.factor(data4$D)
data4$E = as.factor(data4$E)

x1=x2=x3=x4=x5=rep(1,320)
x1[data4$A == '-1'] = -1
x2[data4$B == '-1'] = -1
x3[data4$C == '-1'] = -1
x4[data4$D == '-1'] = -1
x5[data4$E == '-1'] = -1


#Boxplot
par(mfrow = c(1, 5))
boxplot(Cell ~ A, data = data4, main = "Effect of A", range = 0,
        xlab = "A", ylab = "Cell", las = 1)
boxplot(Cell ~ B, data = data4, main = "Effect of B", range = 0,
        xlab = "B", ylab = "Cell", las = 1)
boxplot(Cell ~ C, data = data4, main = "Effect of C", range = 0,
        xlab = "C", ylab = "Cell", las = 1)
boxplot(Cell ~ D, data = data4, main = "Effect of D", range = 0,
        xlab = "D", ylab = "Cell", las = 1)
boxplot(Cell ~ E, data = data4, main = "Effect of E", range = 0,
        xlab = "E", ylab = "Cell", las = 1)



model4 = lm(Cell ~ A*B*C*D*E, data = data4)
anova(model4)
# Analysis of Variance Table
# 
# Response: Cell
# Df Sum Sq Mean Sq  F value    Pr(>F)    
# A           1  11092   11092  84.0740 < 2.2e-16 ***
#   B           1 120979  120979 916.9804 < 2.2e-16 ***
#   C           1   1044    1044   7.9133 0.0052449 ** 
#   D           1    911     911   6.9070 0.0090465 ** 
#   E           1   1638    1638  12.4159 0.0004949 ***
#   A:B         1   2071    2071  15.6946 9.391e-05 ***
#   A:C         1   1088    1088   8.2453 0.0043892 ** 
#   B:C         1  31126   31126 235.9250 < 2.2e-16 ***
#   A:D         1    180     180   1.3643 0.2437528    
# B:D         1    905     905   6.8559 0.0093023 ** 
#   C:D         1    546     546   4.1386 0.0428320 *  
#   A:E         1   1428    1428  10.8241 0.0011264 ** 
#   B:E         1    118     118   0.8915 0.3458724    
# C:E         1   2091    2091  15.8492 8.691e-05 ***
#   D:E         1    650     650   4.9253 0.0272458 *  
#   A:B:C       1    135     135   1.0248 0.3122397    
# A:B:D       1    714     714   5.4120 0.0206930 *  
#   A:C:D       1   1911    1911  14.4848 0.0001726 ***
#   B:C:D       1     31      31   0.2369 0.6268501    
# A:B:E       1    800     800   6.0646 0.0143772 *  
#   A:C:E       1     38      38   0.2866 0.5928168    
# B:C:E       1      7       7   0.0546 0.8154538    
# A:D:E       1     24      24   0.1834 0.6687639    
# B:D:E       1     14      14   0.1032 0.7482815    
# C:D:E       1    171     171   1.2970 0.2557128    
# A:B:C:D     1     20      20   0.1516 0.6973051    
# A:B:C:E     1    405     405   3.0698 0.0808253 .  
# A:B:D:E     1    392     392   2.9683 0.0859849 .  
# A:C:D:E     1      4       4   0.0274 0.8686873    
# B:C:D:E     1     58      58   0.4381 0.5085679    
# A:B:C:D:E   1     88      88   0.6685 0.4142417    
# Residuals 288  37996     132                       
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


model4_2 = lm(Cell~x1+x2+x3+x4+x5+x1*x2+x1*x3+x2*x3+x2*x4+x3*x4+x1*x5+
              x3*x5+x4*x5+x1*x2*x4+x1*x3*x4+x1*x2*x5, data = data4)
anova(model4_2)
# Analysis of Variance Table
# 
# Response: Cell
# Df Sum Sq Mean Sq  F value    Pr(>F)    
# x1          1  11092   11092  84.7753 < 2.2e-16 ***
#   x2          1 120979  120979 924.6292 < 2.2e-16 ***
#   x3          1   1044    1044   7.9793 0.0050476 ** 
#   x4          1    911     911   6.9646 0.0087467 ** 
#   x5          1   1638    1638  12.5194 0.0004663 ***
#   x1:x2       1   2071    2071  15.8255 8.708e-05 ***
#   x1:x3       1   1088    1088   8.3140 0.0042175 ** 
#   x2:x3       1  31126   31126 237.8930 < 2.2e-16 ***
#   x2:x4       1    905     905   6.9131 0.0089962 ** 
#   x3:x4       1    546     546   4.1731 0.0419411 *  
#   x1:x5       1   1428    1428  10.9144 0.0010694 ** 
#   x3:x5       1   2091    2091  15.9814 8.053e-05 ***
#   x4:x5       1    650     650   4.9663 0.0265835 *  
#   x1:x4       1    180     180   1.3757 0.2417589    
# x2:x5       1    118     118   0.8989 0.3438376    
# x1:x2:x4    1    714     714   5.4571 0.0201451 *  
#   x1:x3:x4    1   1911    1911  14.6057 0.0001610 ***
#   x1:x2:x5    1    800     800   6.1152 0.0139548 *  
#   Residuals 301  39383     131                       
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Cell~x1+x2+x3+x4+x5+x1*x2+x1*x3+x2*x3+x2*x4+x3*x4+x1*x5+
  #              x3*x5+x4*x5+x1*x2*x4+x1*x3*x4+x1*x2*x5


coef(model4_2)
# (Intercept)          x1          x2          x3          x4          x5       x1:x2 
# 53.50000    -5.88750   -19.44375     1.80625    -1.68750    -2.26250    -2.54375 
# x1:x3       x2:x3       x2:x4       x3:x4       x1:x5       x3:x5       x4:x5 
# -1.84375    -9.86250    -1.68125     1.30625    -2.11250     2.55625     1.42500 
# x1:x4       x2:x5    x1:x2:x4    x1:x3:x4    x1:x2:x5 
# -0.75000     0.60625     1.49375    -2.44375    -1.58125 


par(mfrow=c(1,2))
plot(model4_2, which = 1)
plot(model4_2, which = 2)


plot.design(Cell~A*B*C*D*E, data = data4, main = "Main Effects")



install.packages("DAAG")
library(DAAG)
press(model1.2)
#[1] 1608.444
press(model2.4)
#[1] 7411.71
press(model3_0)
#[1] 8465.361
press(model4_2)
#[1] 44511.88

##First model is the best according to press residuals.

summary(model1.2)
# Residual standard error: 8.683 on 12 degrees of freedom
# Multiple R-squared:  0.9214,	Adjusted R-squared:  0.9018 
# F-statistic:  46.9 on 3 and 12 DF,  p-value: 6.674e-07

summary(model2.4)
# Residual standard error: 14.49 on 29 degrees of freedom
# Multiple R-squared:  0.6917,	Adjusted R-squared:  0.6704 
# F-statistic: 32.53 on 2 and 29 DF,  p-value: 3.892e-08

summary(model3_0)
# Residual standard error: 9.75 on 46 degrees of freedom
# Multiple R-squared:  0.9107,	Adjusted R-squared:  0.8778 
# F-statistic: 27.61 on 17 and 46 DF,  p-value: < 2.2e-16

summary(model4_2)
# Residual standard error: 11.44 on 301 degrees of freedom
# Multiple R-squared:  0.8199,	Adjusted R-squared:  0.8091 
# F-statistic: 76.13 on 18 and 301 DF,  p-value: < 2.2e-16

interaction.plot(data1$B, data1$C, data1$Cell, 
                 main = "Interaction effects of B and C")

