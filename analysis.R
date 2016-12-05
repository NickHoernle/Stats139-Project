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
hist(clean_data$per_pupil_expenditure)
hist(log((clean_data$per_pupil_expenditure)))
clean_data$per_pupil_expenditure <- log((clean_data$per_pupil_expenditure))

#transform
hist(clean_data$employee_salaries)
hist(log(clean_data$employee_salaries))
clean_data$employee_salaries <- log(clean_data$employee_salaries)
#transform
hist(clean_data$total_support_services)
hist(log(clean_data$total_support_services))
clean_data$total_support_service <- log(clean_data$total_support_services)

#tranny
hist(clean_data$people_per_household)
hist(log(clean_data$people_per_household))
clean_data$gini_coef <- hist(log(clean_data$people_per_household))
#tranny
hist(clean_data$gini_coef)
hist(log(clean_data$gini_coef))
clean_data$gini_coef <- log(clean_data$gini_coef)


hist(clean_data$poverty_18_and_younger)
hist(sqrt(clean_data$poverty_18_and_younger))
clean_data$poverty_18_and_younger <- sqrt(clean_data$poverty_18_and_younger)

hist(clean_data$FTE_teachers)
hist(log(clean_data$FTE_teachers))
clean_data$FTE_teachers<- log(clean_data$FTE_teachers)

#DROP MEDIAN INCOME
hist(clean_data$median_income)







model <- lm(total_score ~ ., data=df_without_state)
summary(model)

model2 <- lm(total_score ~ poverty_18_and_younger
                            +FTE_teachers
                            +american.indian.alaskan
                            +asian.hawaiian.Native.Pacific.Islander.or.asian
                            +black
                            +student.support.services, data=totalclean)
summary(model2)

#interaction model
model3 <- lm(total_score ~ (poverty_18_and_younger
             +FTE_teachers
             +american.indian.alaskan
             +asian.hawaiian.Native.Pacific.Islander.or.asian
             +black
             +student.support.services)^2, data=totalclean)
summary(model3)


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











