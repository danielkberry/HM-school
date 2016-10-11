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

truancy_data <- read.csv('truancy.csv', stringsAsFactors = FALSE)
truancy_data$X <- NULL
truancy_data$X.1 <- NULL

subject_data_high <- subset(subject_data, High.Grade == 12)

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
print(nrow(all_data))
## note: we lose a few rows doing this join, not sure why. Some mismatch between school pass rates and accredidation data?

all_data <- merge(x = all_data,
                  y = ethnicity_data_high,
                  by.x = c('Div.Num', 'Sch.Num'),
                  by.y = c('Division.No.', 'School.No.'),
                  suffixes = c('.ad', '.ed'))
print(nrow(all_data))
## losing some more rows here...
## why don't they use a consistant naming scheme across all the files? I mean damn, how many ways can one abbreviate "school number"?

all_data <- merge(x = all_data,
                  y = truancy_data,
                  by.x = c('Div.Num'),
                  by.y = c('Division.No'),
                  suffixes = c('.ad','.t'))
print(nrow(all_data))


all_data <- merge(x = all_data,
                  y = salary_data,
                  by.x = 'Div.Num',
                  by.y = 'Division',
                  suffixes = c('.ad', 'sd'))
print(nrow(all_data))
## I don't think we lose any data here
all_data$Div.Name <- tolower(str_trim(all_data$Div.Name))
vote_cast_3$LocalityName <- tolower(vote_cast_3$LocalityName)

all_data <- merge(x = all_data,
                  y = vote_cast_3,
                  by.x = 'Div.Name',
                  by.y = 'LocalityName',
                  suffixes = c('.ad','.vc2'))

## convert some cols to numeric
to_num_cols <- c(grep('Male|Female', names(all_data), value = TRUE), 'English', 'Science', 'Mathematics', 'History', 'Total..Full.time...Part.time.Students', 'Truancy.Count', 'GCI', grep('Pass', names(all_data), value = TRUE))

for (col in to_num_cols) {
    all_data[[col]] <- as.numeric(all_data[[col]])
}

to_pct_cols <- grep('Male|Female', names(all_data), value = TRUE)
for (col in to_pct_cols) {
    all_data[[paste0(col,'.pct')]] <- all_data[[col]]/all_data$Total..Full.time...Part.time.Students
}

## compute truancy percentage:
truancy_data_pct <- ddply(all_data,
                          .(Div.Name),
                          function(df) {
                              mean(df$Truancy.Count) / sum(df$Total..Full.time...Part.time.Students)
                          }
                          )

names(truancy_data_pct) <- c('Div.Name', 'Truancy.pct')

all_data <- merge(x = all_data,
                  y = truancy_data_pct,
                  by = 'Div.Name')

races <- c('Native.Hawaiian', 'White', 'Two.or.more.races', 'Hispanic', 'American.Indian', 'Black')
for (race in races) {
    race_cols <- grep('.pct',grep(race, names(all_data), value = TRUE), value = TRUE)
    all_data[[paste0(race,'.pct')]] <- rowSums(all_data[,race_cols])
}


model_data <- subset(all_data,
                     Subgroup == 'All Students' & Subject == 'Mathematics',
                     select = c('Div.Name',
                                'School.Name',
                                paste0(races,'.pct'),
                                grep('Pass', names(all_data), value = TRUE),
                                'School.Accreditation.Rating',
                                grep('English|Science|Mathematics|History', names(all_data), value = TRUE),
                                'Total..Full.time...Part.time.Students',
                                grep('Salary', names(all_data), value = TRUE),
                                'Democratic.pct',
                                'Truancy.pct'
                                ))

## school level predictors: race as pct, current and historical overall pass rate, accredidation rating, pass rates/accredidations in history, science, and english, mathematics accredidation

## district level predictors: teacher salaries, truancy rates, and pct democratic

write.csv(all_data, 'all_data.csv')
write.csv(model_data, 'model_data.csv')


