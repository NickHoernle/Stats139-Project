#> install.packages('readxl')

# Install these if you need, you might need to run above code
# to install the readxl package
library(readxl)    
library(plyr)

# helper to read sheets
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

# helper to get the corect numeric from factors
as.numeric.factor <- function(x) {as.numeric(gsub(",", "", as.character(x)))}

# read all the messy sheets into a vector
messy_sheets <- read_excel_allsheets("./data/project_data.xls")

other_data <- read.csv('./data/clean_new_data.csv')

# rename bad columns
messy_sheets <- rename(messy_sheets, c(
  "Poverty and MedianIncome" ="poverty_and_median_income",
  "People per Household"     ="people_per_household",  
  "Private Schools"          ="private_schools",
  "Freshman Graduation Rate" ="freshman_graduation_rate",
  "Per pupil expenditure"    ="per_pupil_expenditure",
  "Per pupil expenditure, etc."="more_per_pupil_expenditure", 
  "demographics and grades"  ="demographics_and_grades",
  "Gini Coefficient and voucher"="gini_coefficient_and_voucher"))


# clean up after the misery of excel
colnames(messy_sheets$poverty_and_median_income) <- messy_sheets$poverty_and_median_income[2, ]
messy_sheets$poverty_and_median_income <- messy_sheets$poverty_and_median_income[-c(1,2,55,56,57), ]

messy_sheets$people_per_household <- messy_sheets$people_per_household[-c(1), ]

colnames(messy_sheets$private_schools) <- messy_sheets$private_schools[3, ]
messy_sheets$private_schools <- messy_sheets$private_schools[-c(1,2,3,4,5,55,56,57), ]

# messy_sheets$more_per_pupil_expenditure <- messy_sheets$more_per_pupil_expenditure[-c(1), ]
messy_sheets$more_per_pupil_expenditure <- messy_sheets$more_per_pupil_expenditure[-1, ]
colnames(messy_sheets$more_per_pupil_expenditure) <- c(
  'state', "per_pupil_expenditure", "employee_salaries", "total_support_services", "total_number_of_employees" )

# colnames(messy_sheets$demographics_and_grades) <- messy_sheets$demographics_and_grades[1, ]
messy_sheets$demographics_and_grades <- messy_sheets$demographics_and_grades[-c(1,53,54,55,56,57,58,59,60), ]
colnames(messy_sheets$demographics_and_grades) <- c('state', colnames(messy_sheets$demographics_and_grades)[-1])

colnames(messy_sheets$gini_coefficient_and_voucher) <- messy_sheets$gini_coefficient_and_voucher[1, ]
messy_sheets$gini_coefficient_and_voucher <- messy_sheets$gini_coefficient_and_voucher[-c(1,53,54,55,56,57),c(1,2,3)]

#######################################################################
# run these commands to see what the data is about
# summary(messy_sheets$poverty_and_median_income) # note lot's of missing data here
# summary(messy_sheets$people_per_household) # clean
# summary(messy_sheets$private_schools) # not pretty but stuff is here
# summary(messy_sheets$per_pupil_expenditure) # missing data? This one is critical?
# summary(messy_sheets$more_per_pupil_expenditure) # this one is better
# summary(messy_sheets$demographics_and_grades) # look up these column names if you are confised
# summary(messy_sheets$gini_coefficient_and_voucher)

# head(messy_sheets$poverty_and_median_income)
# head(messy_sheets$people_per_household)
# head(messy_sheets$private_schools)
# head(messy_sheets$per_pupil_expenditure)
# head(messy_sheets$more_per_pupil_expenditure)
# head(messy_sheets$demographics_and_grades)
# head(messy_sheets$gini_coefficient_and_voucher)

# not sure what is plotting with these
# ignoring them
#summary(messy_sheets$freshman_graduation_rate)
#head(messy_sheets$freshman_graduation_rate)

####################################################################
# Define all of the variables that we want to use in our project here.
# Please make sure that the lengths etc are correct and the data are aligned
state                   <- messy_sheets$more_per_pupil_expenditure$state[-c(52,53,54,55,56)]
per_pupil_expenditure   <- as.numeric(messy_sheets$more_per_pupil_expenditure$per_pupil_expenditure[-c(52,53,54,55,56)])
employee_salaries       <- as.numeric(messy_sheets$more_per_pupil_expenditure$employee_salaries[-c(52,53,54,55,56)])
total_support_services  <- as.numeric(messy_sheets$more_per_pupil_expenditure$total_support_services[-c(52,53,54,55,56)])
people_per_household    <- as.numeric(messy_sheets$people_per_household$`People per Household`)
gini_coef               <- as.numeric(messy_sheets$gini_coefficient_and_voucher$`gini coefficient`)
voucher                 <- messy_sheets$gini_coefficient_and_voucher$`voucher?`
median_income           <- as.numeric(other_data$median_income)
poverty_18_and_younger  <- as.numeric(other_data$poverty_18_and_younger)
FTE_teachers            <- as.numeric.factor(other_data$FTE_teachers)

voucher[voucher=='yes'] <- 1
voucher[is.na(voucher)] <- 0
voucher                 <- as.factor(voucher)

clean_data <- data.frame(
  state,
  per_pupil_expenditure,
  employee_salaries,
  total_support_services,
  people_per_household,
  gini_coef,
  median_income,
  poverty_18_and_younger,
  FTE_teachers,
  voucher
  )

# this one is just a wealth of information, I didn't feel like copying all of the
# predictors across. We can join by state to joing the dataframes
# I think we will have a lot of good stuff here
demographics <-messy_sheets$demographics_and_grades

write.table(demographics, './data/clean_demographics.csv', sep = ',')
write.table(clean_data, './data/clean_education_data.csv', sep = ',')
