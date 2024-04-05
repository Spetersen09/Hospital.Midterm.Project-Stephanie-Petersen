hop=read.csv("hospitals.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
#PART ONE:
# A)To determine the size of the dataset
dim(hop)
# B) To get the name of the columns
colnames(hop)
# C) To Check the data types of each columns 
sapply(hop, class)
#D) To Identify missing values
summary(is.na(hop))
#E) Which Hospital has the lowest number of beds
hop%>%filter(Beds==min(Beds))
# F)Which hopsital has the lowest Expense?
hop%>%filter(Total.Expense==min(Total.Expense))
#G) How many hospital Deliver Babies?
hop%>%filter(Births>0)%>%nrow()
#H) Scatterplot number of bed vs total Expense
df=data_frame(hop %>% select(Total.Expense, Beds))
ggplot(df, aes(x = Beds, y = Total.Expense))+
  geom_point()
#I) Scatterplots Admissions vs Total Expense
ggplot(hop, aes(x = Admissions, y = Total.Expense)) +
  geom_point() 
#J) Scatterplot Bed vs Total Expense but only for hospital  that
# deliver babies
ggplot(hop%>% filter(Births>0), aes(x=Beds, y=Total.Expense))+
  geom_point()
# K) The relationship between the number of outpatient visit and the total of expense
ggplot(hop, aes(x = Outpatient.Visits, y = Total.Expense)) +
  geom_point()

#PART TWO:

#I Pie Chart
hop %>%
  summarise(Admissions = sum(Admissions), Personnel = sum(Personnel))%>%
  pivot_longer(cols = names(.))%>%
  ggplot(aes(x = " ", y = value, fill=name))+
  geom_col()+
  coord_polar("y", start = 0)


# II Bar Chart
hop %>%
  summarise(Admissions = sum(Admissions), Personnel = sum(Personnel)) %>%
  pivot_longer(cols = names(.))%>%
  ggplot(aes(x = "", fill = name))+
  geom_bar()

#III Line Chart
ggplot()+
  geom_line(hop = hop$Total.Expense, 
            mapping = aes(x = hop$Beds, y = hop$Total.Expense), color = 'pink')+
            geom_line(hop = hop$Payroll.Expense, mapping = aes(x = hop$Beds, 
            y = hop$Payroll.Expense), color='green')




#PART THREE
#II The dependent variable should be Total Expense. Choose an independent variable from one of the remaining attributes.
      #The remaining attributes consider I examined like Beds and their potential influence on the total expense which is the dependent variable in analysis
      model=lm(Total.Expense ~ Beds,hop)
      summary(model)

#iii What is the value of the R^2
      #The R-squared value is 0.6043

#iv. What does the R^2 measure in this case?
      #R-squared value of 0.6043 indicates that around 60.43% of the variation in total expenses can be explained by the number of bed in the linear regression model

#v. What are the p-values ? How many pvalues are reported, why ? What does each pvalue mean?
      #The p-value is 2.2e-16,we can then reject the null hypothesis and determine that Beta is not equal to zero in this case.

#vi. Explain R square and p-values.
    #R-Squared (R^2) measures the proportion of variation in Total Expense explained by Admissions.
    #P-Value asses the significance of coefficients; smaller value indicates stronger evidence against the null hypothesis

#vii. What would be the right attribute size (independent variable) that seems most appropriate to lead you in the expense range of $55–$75 million
    #The proper attribute size to fall within the expense range of 55-75 million dollars ranges from 65 to 84 beds.
      model=lm(Total.Expense ~ Beds,hop)
      predict(model, newdata = data.frame(Beds = c(66, 84)))
  
  
#PART FOUR
#i. The dependent variable should be Total Expense Choose two independent variables from one of the remaining attributes.
      #The two independent variable are Beds and Admissions, to predict the total Expense 
      multi_regression <- lm(Total.Expense ~ Beds + Admissions, data = hop)
      summary(multi_regression)
#ii. What is the value of the R^2?
      multi_regression <- lm(Total.Expense ~ Beds + Admissions, data = hop)
      r_squared <- summary(multi_regression)$r.squared
      print(r_squared)
  #The R-Squared value is 0.7398218
#iii. What does the R^2 measure in this case? 
      #An R-Squared value of 0.739218 means that approximately 73.92% of the variance in the total expense is explained by the bed and admission.
   
#iv. What are the pvalues ? How many pvalues are reported, why ? What does each pvalue mean?
      
      #There are 3 p-values bed p<2.2e-16, admissions p<2e-16 and for the intercept p=0.00296. These p-values indicate the statistical significance of the coefficients 
      #in the regression model. A smaller p-value suggests stronger evidence against the null hypothesis, implying that the independent variable has a significant effect
      #on the dependent variable (Total Expense). In this case, all p-values are extremely small, indicating a high level of significance.
  
  
#v. Explain R square, pvalues
      #R-squared value of 0.7398 indicates that approximately 73.98% of the variance in Total Expense is explained by Beds and Admissions.
      #A smaller p-value suggests stronger evidence against the null hypothesis, indicating that the coefficient is statistically significant. In the provided output, all p-values
      #are very small (close to zero), indicating high statistical significance for the coefficients of Beds and Admissions.
  
  
# OPTION A
      #After analyzing both simple and multivariate regression lines, I found that the line explains total expenses better, suggesting that both Beds and Admission 
      #influence expenses more than just Beds alone. Adding more variables might improve the explanation further. Once we determine the better regression model, we can use it to
      #see if facilities with smaller bed sizes fall within our expense target, suggesting that opting for the smaller option is preferable to stay within budget.


      