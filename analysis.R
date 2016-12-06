# read in the clean data
demographics <- read.csv('./data/clean_demographics.csv')
demo_cols <- c('state',
               'american.indian.alaskan',
               'asian.hawaiian.Native.Pacific.Islander.or.asian',
               'hispanic',
               'black',
               'white',
               'Total.number.of.students',
               'high.school',
               'student.support.services'
               )
edu_df <- read.csv('./data/clean_education_data.csv')
NAEP <- read.csv('./data/clean_outcome_variable2.csv')

# drop the empty rows in NAEP
NAEP <- NAEP[-c(52,53,54),]

totalclean <- merge(NAEP[,c('State', 'total_score')], edu_df, by.x='State', by.y='state')
totalclean <- merge(totalclean, demographics[,demo_cols], by.x='State', by.y='state')
#drop DC- i dont think this even worked though
totalclean<-totalclean[!(totalclean$State=="District of Columbia"), ]

df_without_state <- totalclean[,-1]

#transform (decided no sqrt)
hist(df_without_state$per_pupil_expenditure)
hist(log((df_without_state$per_pupil_expenditure)))
df_without_state$per_pupil_expenditure <- log((df_without_state$per_pupil_expenditure))

#transform
hist(df_without_state$employee_salaries)
hist(log(df_without_state$employee_salaries))
df_without_state$employee_salaries <- log(df_without_state$employee_salaries)

#tranny
hist(df_without_state$people_per_household)
hist(log(df_without_state$people_per_household))
df_without_state$gini_coef <- (log(df_without_state$people_per_household))
#tranny
hist(df_without_state$gini_coef)
hist(log(df_without_state$gini_coef))
df_without_state$gini_coef <- log(df_without_state$gini_coef)

#transform #MAYBE MORE XTREME
hist(df_without_state$total_support_services,  main = "Total Support Services Histogram", col= "light blue", xlab="Total Support Servies", ylab="Frequency")
hist(log(df_without_state$total_support_services),  main = "Logarithm Transformation of Total Support Services Histogram", col= "light green", xlab="Log Total Support Servies", ylab="Frequency")
df_without_state$total_support_service <- log(df_without_state$total_support_services)

hist(df_without_state$poverty_18_and_younger)
hist(sqrt(df_without_state$poverty_18_and_younger))
df_without_state$poverty_18_and_younger <- sqrt(df_without_state$poverty_18_and_younger)

hist(df_without_state$FTE_teachers)
hist(log(df_without_state$FTE_teachers))
df_without_state$FTE_teachers<- log(df_without_state$FTE_teachers)

#DROP MEDIAN INCOME
hist(df_without_state$median_income)

df_without_state <- df_without_state[,-7] # drop mean income
#clean_data <- clean_data[,-1] # drop state

######################################################################
# Lasso
library(lars)
model3 <- lm(total_score ~ .^2, data = df_without_state)
x <- model.matrix(model3)
y <- df_without_state$total_score
fit <- lars(x, y, type='lasso', normalize=TRUE)

thresh <-20
par(mar = c(20,4,4,2))
par(xpd=TRUE)
coef_ <- coef(fit)[thresh, (coef(fit)[thresh,]>0)|(coef(fit)[thresh,]<0)]
names <- colnames(x)[(coef(fit)[thresh,]>0)|(coef(fit)[thresh,]<0)]
n <- 1:length(coef_)
barplot(coef_, names.arg = n, col = 'blue',
        ylab = 'Coefficient Magnitude',
        xlab = 'Predictor',
        main = 'Plot of Coefficients from Lasso Regression on all\nmain predictors and their two way interaction terms')
i = 1
n = rep(NA, length(names))
for( name in names){
  if (i < 10){
    n [i] <- (paste(i,':  ', name,'\n'))
  }else{
    n [i] <- (paste(i,': ', name,'\n'))
  }
  i <- i + 1
}
legend(1, 
       -23,
       n, 
       pch=0, 
       col='blue',
       fill='blue',
       cex=1,
       text.width=15,
       text.font = 0.9)

plot(fit)
#Lasso
######################################################################


model <- lm(total_score ~ ., data=df_without_state)
summary(model)
plot(model)

model2 <- lm(total_score ~ poverty_18_and_younger
                            +FTE_teachers
                            +american.indian.alaskan
                            +asian.hawaiian.Native.Pacific.Islander.or.asian
                            +black
                            +student.support.services, data=df_without_state)
summary(model2)
plot(model2)
## Wisconsin has a big nfluence on cooks model. This obs is far from avr of covariates. 
#one covariate for this state is far from avr- why it 
#this obs has a 
#cooks dis: if x is far, and move y, huge change on slope. Not bad- just worth looking at
#WIsc has more impact than other states. barely outside red line 

#interaction model
model3 <- lm(total_score ~ (poverty_18_and_younger
             +FTE_teachers
             +american.indian.alaskan
             +asian.hawaiian.Native.Pacific.Islander.or.asian
             +black
             +student.support.services)^2, data=df_without_state)
summary(model3)
plot(model3)
#WE PICKED THE ONES THAT WERE MOST IMPORTANT, 

#Model from 
model5 <- lm(total_score ~ (people_per_household*
                            per_pupil_expenditure*
                              FTE_teachers+
                              gini_coef*
                              voucher+
                              poverty_18_and_younger*
                              total_support_service),
             data=df_without_state)
summary(model5)

plot(model5)

#AIC BIC below
#model 1 ((total model))
AICmodel1<-step(model,df_without_state,direction='backward')
#################   AIC=209.72    #################
#white, hisp, total # students, student support services, 
## Per pupil exp, am indian, black, employee salary, asian, people per house, 
#poverty 18

##NOT INCLUDED median income

AICmodel2<-step(model,df_without_state,direction='both')
#also 209.72
AICmodel2<-step(model,df_without_state,direction='forward')
#220- worse
###########################################################



BICmodel1<-step(model,df_without_state,direction='both',k=log(nrow(df_without_state)))
######### #BIC "Step:  AIC=220.84"  ################
#black, Per pupil exp, people per house, employee salaries, asian, poverty

#NOT INCLUDED
#white, hisp, total # students, student support services, 
#am indian, black, poverty 18

#question for Iav- 
#what does it mean to have sign or not sign, 
#and then have these be in the model or not


BICmodel1<-step(model,df_without_state,direction='forward',k=log(nrow(df_without_state)))
#254- worse

BICmodel1<-step(model,df_without_state,direction='backward',k=log(nrow(df_without_state)))
#220.84


#### MODEL 2


#AIC BIC below
AICmodel2.1<-step(model2,df_without_state,direction='backward')
#################   AIC=216    #################
#white, hisp, total # students, student support services, 
## Per pupil exp, am indian, black, employee salary, asian, people per house, 
#poverty 18

##NOT INCLUDED median income

AICmodel2.2<-step(model2,df_without_state,direction='both')
#216
AICmodel2.3<-step(model2,df_without_state,direction='forward')
#217
###########################################################

##Model 3

#AIC  below
AICmodel3.1<-step(model3,df_without_state,direction='backward')
#AIC=214 
AICmodel3.2<-step(model3,df_without_state,direction='both')
#214
AICmodel3.3<-step(model3,df_without_state,direction='forward')
#228





