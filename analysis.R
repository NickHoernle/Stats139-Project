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
# visulisations, variable transformations etc
# Kimia you can start exploring here

#ASSUMPTIONS:
#Linearity
pairs(demographics)

#observations:
### 1 per pupil exp v employee salaries: have far tail outliers worth looking into

### 1

#merge(NAEP, edu_df, by.x='State', by.y='state')
#LMs
#people per household is off x.1?
#expendature per student too. 

#outcome variable is mean score of reading + math
model1<-lm(totalclean$mean_avrg ~ totalclean$employee_salaries + totalclean$total_support_services + totalclean$studentsupportserv + totalclean$total_students)
summary(model1)
#nothing is signif 

#outcome variable is standarized mean score of reading + math
model2<-lm(totalclean$mean_st ~ totalclean$employee_salaries + totalclean$total_support_services + totalclean$studentsupportserv + totalclean$total_students)

summary(model2)
#total support services is significant, with + coefficient 

## look at as many as possible
labels(totalclean)
model1all<-lm(totalclean$mean_avrg ~ (totalclean$employee_salaries + totalclean$total_support_services + totalclean$studentsupportserv + totalclean$total_students + totalclean$X2_more_races + totalclean$black + totalclean$hispanic + totalclean$indig + totalclean$azn + totalclean$white)^2)

model2all<-lm(totalclean$mean_st ~ (totalclean$employee_salaries + totalclean$total_support_services + totalclean$studentsupportserv + totalclean$total_students + totalclean$X2_more_races + totalclean$black + totalclean$hispanic + totalclean$indig + totalclean$azn + totalclean$white).^2)

summary(model1all)
#

summary(model2all)
#

model2<-lm(totalclean$mean_avrg ~

#AIC Model
step_forward = step(model0, scope=list(upper=model1), direction="forward")
summary(step_forward)
extractAIC(step_forward)

#AIC BIC below
#model 1 (Mean of scores)
AICmodel1<-step(model1all,totalclean,direction='both')

BICmodel1<-step(model1all,totalclean,direction='both',k=log(nrow(totalclean)))

#model 2 standard mean scores
AICmodel2<-step(model2all,totalclean,,direction='both')

BICmodel2<-step(model2all,totalclean,direction='both',k=log(nrow(totalclean)))
#AIC tends to fit larger models than BIC (penalty term grows with n)