library(dplyr)
library(ggplot2)
library(ggeffects)
options(width = 200)
### ----------------- Read in data
df = read.csv("allotment_owners.csv")
colnames(df)

# Exclude any missing data
df <- na.omit(df)

### RESEARCH QUESTION ###
#1.	Is there a relationship between mental health and time spent in the nature? 

### Create collapsed MH variabe = to the rowMean
df <- df %>%
  mutate(MH_mean = rowMeans(across(starts_with("MH_")), na.rm = TRUE))

### Create a log-transformed versoin of hours spent in nature ###
df$D_hours_log = log1p(df$D_hours)

# Visual inspection of correlation
plot(df$D_hours, df$MH_mean)
plot(df$D_hours_log, df$MH_mean)
ggplot(df, aes(x = D_hours_log, y = MH_mean))+
    geom_smooth()
# Appears to be a non-linear relationship between log_hours spent in nature and MH.

# Lets fit a linear model to the data anyway, because why not lol.
mod_0 = lm(MH_mean ~ D_hours_log, data = df)
summary(mod_0)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.28565    0.08426   38.99  < 2e-16 ***
# D_hours_log  0.20909    0.03406    6.14 1.66e-09 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6153 on 513 degrees of freedom
# Multiple R-squared:  0.06845,   Adjusted R-squared:  0.06664 
# F-statistic:  37.7 on 1 and 513 DF,  p-value: 1.657e-09

### Lets try adding age to the model ###

mod_1 = lm(MH_mean ~ D_hours_log + D_Age, data = df)
summary(mod_1)
anova(mod_0, mod_1)
# Age significantly improves the model.

# Lets plot the marginal means from the model
pred <- ggpredict(mod_1, terms = "D_hours_log")
plot(pred)

### What about gender? ###
df$D_Gender = factor(df$D_Gender, levels = c("male", "female"))
mod_2 = lm(MH_mean ~ D_hours_log + D_Gender, data = df)
summary(mod_2)
anova(mod_0, mod_2)
# Gender does not improve model fit. 

### Is there an interaction between age and hours spent in nature? ###
mod_1_int <- lm(MH_mean ~ D_hours_log * D_Age,
                  data = df)
summary(mod_1_int) # apperas not

#### Lets check assumptions of linear regression ###
# We use the winning model: mod_1

# Normality of residuals
shapiro.test(residuals(mod_1)) # We can reject normality according to Wilks testw

# What about Q-Q plot?
qqnorm(residuals(mod_1)); qqline(residuals(mod_1), col = "red")
hist(residuals(mod_1))
## The residuals are rougly normal and symmetric around the central tendency, no severe vialotaion.

# The descriptive plotting we did revealed a non-linear relationship between the variables.
# Lets see if the residuals vs fitted reveal the same thing. 
plot(mod_1, which = 1) 
# Looks kinda non-linear to be honest. 


### Lets add a quadratic term to our model, i.e. extend mod_1 in the following way:
mod_1_quad <- lm(MH_mean ~ D_hours_log + I(D_hours_log^2) + D_Age,
               data = df)

summary(mod_1_quad )
anova(mod_1, mod_1_quad)
# The quadratic term is not helping with model fit.
plot(mod_1_quad, which = 1)




