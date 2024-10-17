# Assignment on Violence, using variables Incarceration (Prisoners) and Income
# https://vincentarelbundock.github.io/Rdatasets/doc/AER/Guns.html

# Setting directory
setwd("C:/Users/65828/OneDrive/Desktop/Guns R")
# Importing excel file
V1 <- read.csv("Guns.csv", sep=",", header=T)  # T = True
# Unclass and converting characters to factors
V1 <- as.data.frame(unclass(V1), stringsAsFactors = TRUE)
str(V1)
View(V1)

# Convert incarceration (prisoners) and income into categories ranking H,M,L by probability
# na.rm=T makes empty excel inputs turn up as NA
V1$prisoners_cat <- ifelse(V1$prisoners<quantile(V1$prisoners, probs=c(0.33), na.rm=T), "1_LOW(prisoners)", ifelse(V1$prisoners<quantile(V1$prisoners,probs = c(0.67), na.rm=T), "2_MEDIUM(prisoners)", "3_HIGH(prisoners)"))
V1$income_cat <- ifelse(V1$income<quantile(V1$income, probs=c(0.33), na.rm=T), "1_LOW(income)", ifelse(V1$income<quantile(V1$income,probs = c(0.67), na.rm=T), "2_MEDIUM(income)", "3_HIGH(income)"))
V1 <- as.data.frame(unclass(V1), stringsAsFactors = TRUE)
V1$prisoners_cat <- as.factor(V1$prisoners_cat)
V1$income_cat <- as.factor(V1$income_cat)
str(V1)
View(V1)

# 1 way ANOVA testing whether there are significant differences in violent crime rates (violent) across different incarceration levels (prisoners_cat)
# ANOVA is a statistical test used to assess the difference between the means of more than two groups
# Null hypothesis: No significant differences between Violent Crime Rates and the incarceration rate in the state in the previous year
# Alternate hypothesis: There are statistically significant differences between Violent Crime Rates and the incarceration rate in the state in the previous year
an1 <- aov(violent ~ prisoners_cat, data=V1)   #fitted model
summary(an1)
# since p-value = 2e-16 < 0.05, NULL IS REJECTED
# Indicates that violent crime rates differ across different incarceration levels


# Boxplot 
# Interpretation: 
# Median values from L < M < H. L with the lowest median and H with the highest median
# There is some overlap between all 3 boxes
boxplot(violent~prisoners_cat, data=V1, col="light blue", main="Box Plot",
        xlab="The incarceration rate in the state in the previous year", ylab="Violent Crime Rates")

# 2 way ANOVA testing main effects of prisoners_cat and income_cat on violent crime rates, as well as interaction effect between these 2 variables
an2 <- aov(violent~prisoners_cat + income_cat + prisoners_cat:income_cat, data=V1)
summary(an2)
# Null: There is no interaction effect between the incarceration rate and income level on violent crime rates
# Alternative: There is an interaction effect between incarceration rate and income level on violent crime rates
# p value < 0.05 for prisoners (main effect) and income (main effect), reject null
# p-value = 0.00456 < 0.05 for prisoners_cat:income_cat (interaction effect), reject null
# Conclusion: There is an interaction effect between Violent Crime Rates and the incarceration rate. Effect of incarceration rate on violent crime varies depending on income

# Interaction plot
# How violent crime rates vary across different levels of incarceration and income
interaction.plot(V1$prisoners_cat,V1$income_cat,V1$violent)

# Simple linear regression 
# Access the relationship between violent crime rates and incarceration rate (prisoners)
# Positive relationship between incarceration and rates violent crime rates
View(V1)
fit1 <- lm(violent~prisoners, data = V1)
summary(fit1)

# AB line
plot(violent~prisoners, main = "Regression Line", data = V1)
abline(fit1, col = "blue")# plot regression line y = bx + a
# limiting the line to make the graph look nicer :>
plot(violent~prisoners, main = "Regression Line", data = V1, xlim=c(0,1000), ylim=c(0,3000))
abline(fit1, col = "blue")# plot regression line y = bx + a

# Multiple linear regression
# regressing multiple variables on violent crime rates
summary(fit2 <- step(lm(violent ~ prisoners + income + afam + cauc + density + male + population, data=V1), direction="both"))
# best model is violent ~ prisoners + income + cauc + density + male + population, AIC = 12186.2

# Decision tree
install.packages("tree")
require(tree)  #load package

tree1 <- tree(violent ~ prisoners + income + afam + cauc + density + male + population, data=V1)
plot(tree1)
text(tree1)


# Chi Square
# Relationship between prisoners_cat and income_cat
# Create Contingency Table, Rows, Columns
V1_prisoners_income <- table(V1$prisoners_cat, V1$income_cat)
V1_prisoners_income
chisq.test(V1_prisoners_income)
# Null Hypothesis: There is no relationship between incarceration rates and income
# Alternative: There is a relationship between incarceration rates and income
# if p > 0.05, do NOT reject null
## Since p < 0.05, REJECT NULL
### Therefore there is a relationship between incarceration rates and income

# Observed values
chisq.test(V1_prisoners_income)$observed
# Expected Frequency
chisq.test(V1_prisoners_income)$expected


######### clear 
rm(list = ls()) #remove all the object from the workspace
dev.off() #clear any possible plots (device)
cat("\014") # ctrl+L clear console


