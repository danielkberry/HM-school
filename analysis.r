## load libraries
library(plyr)
library(reshape2)
library(stringr)

## load and aggregate

subject_data <- read.csv('2015-16_school_subject.csv', stringsAsFactors = FALSE)

accred_data <- read.csv('accreditation_2013_and_after_report.csv', stringsAsFactors = FALSE, skip = 3)

ethnicity_data <- read.csv('school_summaries_ethnicity.csv', stringsAsFactors = FALSE, skip = 4)

salary_data <- read.csv('salaries.csv',stringsAsFactors = FALSE)

vote_data <- read.csv('2012_general_results.csv', stringsAsFactors = FALSE)

subject_data_high <- subset(subject_data, School.Type == 'High')

ethnicity_data_high <- subset(ethnicity_data, Grade == '12')

vote_data_pres <- subset(vote_data, OfficeTitle == 'President and Vice President' & Party %in% c('Democratic','Republican'))
vote_temp <- melt(vote_data_pres, id.vars = c('LocalityName', 'Party'), measure.vars = 'TOTAL_VOTES')
vote_cast <- dcast(vote_temp, formula = LocalityName + Party ~ variable, fun.aggregate = sum)

vote_cast_2 <- dcast(vote_cast, LocalityName ~ Party, value.var = 'TOTAL_VOTES')
vote_cast_3 <- cbind(vote_cast_2, vote_cast_2[,2:3]/rowSums(vote_cast_2[,2:3]))
names(vote_cast_3) <- c(names(vote_cast_2), paste(names(vote_cast_2[,2:3]), '.pct', sep=''))

salary_data$FY.2014..Actual.Average.Teacher.Salary <- as.numeric(gsub('\\t|\\s|\\,',
                                                                      '',
                                                                      salary_data$FY.2014..Actual.Average.Teacher.Salary))

salary_data$FY.2015..Actual.Average.Teacher.Salary <- as.numeric(gsub('\\t|\\s|\\,',
                                                                      '',
                                                                      salary_data$FY.2015..Actual.Average.Teacher.Salary))

salary_data$FY.2016..Budgeted.Average.Teacher.Salary <- as.numeric(gsub('\\t|\\s|\\,',
                                                                      '',
                                                                      salary_data$FY.2016..Budgeted.Average.Teacher.Salary))

all_data <- merge(x = subject_data_high,
                  y = accred_data,
                  by.x = c('Div.Num','Sch.Num'),
                  by.y = c('Division.Number','School.Number'),
                  suffixes = c('.sdh','.acd'))

## note: we lose a few rows doing this join, not sure why. Some mismatch between school pass rates and accredidation data?

all_data <- merge(x = all_data,
                  y = ethnicity_data_high,
                  by.x = c('Div.Num', 'Sch.Num'),
                  by.y = c('Division.No.', 'School.No.'),
                  suffixes = c('.ad', '.ed'))

## losing some more rows here...
## why don't they use a consistant naming scheme across all the files? I mean damn, how many ways can one abbreviate "school number"?

all_data <- merge(x = all_data,
                  y = salary_data,
                  by.x = 'Div.Num',
                  by.y = 'Division',
                  suffixes = c('.ad', 'sd'))

## I don't think we lose any data here
all_data$Div.Name <- tolower(str_trim(all_data$Div.Name))
vote_cast_2$LocalityName <- tolower(vote_cast_2$LocalityName)

all_data <- merge(x = all_data,
                  y = vote_cast_2,
                  by.x = 'Div.Name',
                  by.y = 'LocalityName',
                  suffixes = c('.ad','.vc2'))

write.csv(all_data, 'all_data.csv')
