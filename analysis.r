## load libraries
library(plyr)
library(reshape2)
library(stringr)
library(mice)

## load and aggregate

subject_data <- read.csv('2015-16_school_subject.csv', stringsAsFactors = FALSE)

accred_data <- read.csv('accreditation_2013_and_after_report.csv', stringsAsFactors = FALSE, skip = 3)

ethnicity_data <- read.csv('school_summaries_ethnicity.csv', stringsAsFactors = FALSE, skip = 4)
ethnicity_data$School.No. <- gsub('\\,','',ethnicity_data$School.No.)
## NOTE: SOME SCHOOL IDS ARE WRONG, FIXING:
ethnicity_data$School.No.[ethnicity_data$School.No. == 260] <- 231

salary_data <- read.csv('salaries.csv',stringsAsFactors = FALSE)

vote_data <- read.csv('2012_general_results.csv', stringsAsFactors = FALSE)

truancy_data <- read.csv('truancy.csv', stringsAsFactors = FALSE)
truancy_data$X <- NULL
truancy_data$X.1 <- NULL
truancy_data <- truancy_data[1:123,]

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
                  suffixes = c('.sdh','.acd'),
                  all = TRUE)
print(nrow(all_data))
## note: we lose a few rows doing this join, not sure why. Some mismatch between school pass rates and accredidation data?

all_data <- merge(x = all_data,
                  y = ethnicity_data_high,
                  by.x = c('Div.Num', 'Sch.Num'),
                  by.y = c('Division.No.', 'School.No.'),
                  suffixes = c('.ad', '.ed'),
                  all = TRUE)
print(nrow(all_data))
## losing some more rows here...
## why don't they use a consistant naming scheme across all the files? I mean damn, how many ways can one abbreviate "school number"?

all_data <- merge(x = all_data,
                  y = truancy_data,
                  by.x = c('Div.Num'),
                  by.y = c('Division.No'),
                  suffixes = c('.ad','.t'),
                  all = TRUE)
print(nrow(all_data))

all_data <- merge(x = all_data,
                  y = salary_data,
                  by.x = 'Div.Num',
                  by.y = 'Division',
                  suffixes = c('.ad', 'sd'),
                  all = TRUE)
print(nrow(all_data))
## I don't think we lose any data here
all_data$Div.Name <- tolower(str_trim(all_data$Div.Name))
vote_cast_3$LocalityName <- tolower(vote_cast_3$LocalityName)

all_data <- merge(x = all_data,
                  y = vote_cast_3,
                  by.x = 'Div.Name',
                  by.y = 'LocalityName',
                  suffixes = c('.ad','.vc2'),
                  all = TRUE)

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
                  by = 'Div.Name',
                  all = TRUE)

races <- c('Native.Hawaiian', 'Asian','White', 'Two.or.more.races', 'Hispanic', 'American.Indian', 'Black')
for (race in races) {
    race_cols <- grep('.pct',grep(race, names(all_data), value = TRUE), value = TRUE)
    all_data[[paste0(race,'.pct')]] <- rowSums(all_data[,race_cols])
}


model_data <- subset(all_data,
                     Subgroup == 'All Students' & Subject == 'Mathematics',
                     select = c('Div.Name',
                                'Div.Num',
                                'School.Name',
                                'Sch.Num',
                                paste0(races,'.pct'),
                                grep('Pass', names(all_data), value = TRUE),
                                'School.Accreditation.Rating',
                                grep('English|Science|Mathematics|History', names(all_data), value = TRUE),
                                'Total..Full.time...Part.time.Students',
                                grep('Salary', names(all_data), value = TRUE),
                                'Democratic.pct',
                                'Truancy.pct'
                                ))



icmd <- model_data[!complete.cases(model_data),]

## school level predictors: race as pct, current and historical overall pass rate, accredidation rating, pass rates/accredidations in history, science, and english, mathematics accredidation

## district level predictors: teacher salaries, truancy rates, and pct democratic

## write.csv(all_data, 'all_data.csv')
## write.csv(model_data, 'model_data.csv')

## drop incomplete cases lol



######################
## COMPLETE POOLING ##
######################

summary(complete <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num, data = model_data))

print(car::vif(complete))

## lots of collinearity between salaries and racial pcts, taking some out:
model_data$Native.Hawaiian.pct <- NULL
model_data$American.Indian.pct <- NULL
model_data$Two.or.more.races.pct <- NULL
model_data$FY.2014..Actual.Average.Teacher.Salary <- NULL
model_data$FY.2015..Actual.Average.Teacher.Salary <- NULL
## also reducing to just last year's pass rate:
model_data$X2013.2014.Pass.Rate <- NULL
model_data$X2015.2016.Pass.Rate <- NULL
## and refitting:

summary(complete <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num -Div.Num, data = model_data))
print(car::vif(complete))

## still issues with collinearity, going to drop White.pct, leaving only .pct minority
model_data$White.pct <- NULL

summary(complete <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num -Div.Num, data = model_data))
print(car::vif(complete))

## probably doesn't make sense to include indicators for passing each subject as well as the pass rate for those subjects:

model_data$Met.English <- NULL
model_data$Met.History <- NULL
model_data$Met.Science <- NULL

## and refit: 
summary(complete <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num -Div.Num, data = model_data))
print(car::vif(complete))
## this model still satisfies project requirements I think

View(cor(model_data[,sapply(model_data, is.numeric)]))
## some correlations but nothing major. It's what you'd expect:
## pct democratic positively correlated to pct hispanic, black, salary, and school size
## mathematics highly correlated with last year's pass rate
## scores correlated with each other
## pct black/hispanic negatively correlated with school pass rate, pct asian positively correlated


require(ggplot2)
## some plots:
plot_vars <- c('Asian.pct', 'Hispanic.pct','Black.pct', 'X2014.2015.Pass.Rate', 'English','History', 'Science', 'Total..Full.time...Part.time.Students', 'FY.2016..Budgeted.Average.Teacher.Salary')
for (var in plot_vars) {
    print(ggplot(model_data, aes_string(x = var, y = 'Mathematics')) + geom_point() + geom_smooth(method = 'lm'))
}

library(lme4)
library(lattice)
################
## NO POOLING ##
################

## inspired by: http://stackoverflow.com/questions/11123147/dotplot-of-random-effects
caterpillar_plot <- function(model) {
    theRan <- ranef(model, condVar=TRUE)
    pv <- attr(theRan$Div.Num, "postVar")
    se <- pv[1, 1, ]
    theIntercepts <- theRan$Div.Num[, 1, drop=F]
    theFrame <- cbind(theIntercepts, se)
    names(theFrame)[1] <- "Intercept"
    theFrame$Low <- with(theFrame, Intercept - 2 * se)
    theFrame$High <- with(theFrame, Intercept + 2 * se)
    theFrame$Variable <- rownames(theFrame)
    freqs <- lapply(names(ranef(model)), function(x) cbind(ranef(model)[[x]], table(model.frame(model)[[x]])))[[1]]
    theFrame <- merge(x = theFrame, y = freqs, by.x = c('Intercept','Variable'), by.y = c('(Intercept)','Var1'))
    p <- ggplot(theFrame, aes(y=Intercept, x=Freq)) + geom_linerange(aes(ymin=Low, ymax=High), colour="black") + geom_point(, colour="blue")  + labs(y="Random Intercept", x = 'Number of Schools in District',title='Estimate +- SE')
    return(p)
}

theFrame <- c()
for (divnum in unique(model_data$Div.Num)) {
    divnum <- 1
    subdata <- model_data[model_data$Div.Num == divnum,]
    submodel <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num -Div.Num -FY.2016..Budgeted.Average.Teacher.Salary -Democratic.pct -Truancy.pct,
                   data = subdata)
    est <- summary(submodel)$coefficients[1,1]
    se <- summary(submodel)$coefficients[1,2]
    theFrame <- cbind(theFrame,c(est, est-2*se, est+2*se, sum(model_data$Div.Num == divnum)))  
}
names(theFrame) <- c('Intercept','Low','High','Freq')
p <- ggplot(theFrame, aes(y=Intercept, x=Freq)) + geom_linerange(aes(ymin=Low, ymax=High), colour="black") + geom_point(, colour="blue")  + labs(y="Random Intercept", x = 'Number of Schools in District',title='Estimate +- SE')

model_data$Div.Num

no_pooling <- lmer(Mathematics ~ Asian.pct +
                       Hispanic.pct +
                       Black.pct +
                       X2014.2015.Pass.Rate +
                       School.Accreditation.Rating +
                       English +
                       Met.Mathematics +
                       History +
                       Science +
                       Total..Full.time...Part.time.Students +
                       FY.2016..Budgeted.Average.Teacher.Salary +
                       Democratic.pct +
                       Truancy.pct +
                       (1 + FY.2016..Budgeted.Average.Teacher.Salary + Democratic.pct + Truancy.pct | Div.Name),
                   model_data)

## doesn't really converge:
no_pooling_2 <- lmer(Mathematics ~ Asian.pct +
                       Hispanic.pct +
                       Black.pct +
                       X2014.2015.Pass.Rate +
                       School.Accreditation.Rating +
                       English +
                       Met.Mathematics +
                       History +
                       Science +
                       Total..Full.time...Part.time.Students +
                       FY.2016..Budgeted.Average.Teacher.Salary +
                       (1 + FY.2016..Budgeted.Average.Teacher.Salary | Div.Name),
                   model_data)

install.packages('blme')
library(blme)

no_pooling_2 <- blmer(Mathematics ~ Asian.pct +
                       Hispanic.pct +
                       Black.pct +
                       X2014.2015.Pass.Rate +
                       School.Accreditation.Rating +
                       English +
                       Met.Mathematics +
                       History +
                       Science +
                       Total..Full.time...Part.time.Students +
                       FY.2016..Budgeted.Average.Teacher.Salary +
                       (1 + FY.2016..Budgeted.Average.Teacher.Salary | Div.Num),
                      model_data,
                      cov.prior = wishart)

np_plot <- caterpillar_plot(no_pooling_2)
plot(np_plot)
ggsave('caterpillar_np.png')

#####################
## PARTIAL POOLING ##
#####################

partial_pooling <- lmer(Mathematics ~ Asian.pct +
                       Hispanic.pct +
                       Black.pct +
                       X2014.2015.Pass.Rate +
                       School.Accreditation.Rating +
                       English +
                       Met.Mathematics +
                       History +
                       Science +
                       Total..Full.time...Part.time.Students +
                       FY.2016..Budgeted.Average.Teacher.Salary +
                       Democratic.pct +
                       Truancy.pct +
                       (1 | Div.Num),
                   model_data)
summary(partial_pooling)

pp_plot <- caterpillar_plot(partial_pooling)
plot(pp_plot)


### Multiple Imputation  ++ Training Validation Split

mice_hs <- mice(model_data)
model_data_mice <- complete(mice_hs)

split <- rbinom(nrow(model_data_mice),1,.8)
model_data_T <- model_data_mice[split*(1:length(split)),]
model_data_V <- model_data_mice[-(split*(1:length(split))),]

summary(complete_T <- lm(Mathematics ~ . -Div.Name -School.Name -Sch.Num, data = model_data_T))
print(car::vif(complete_T))

##R doesn't like the - notation for formatting models, have to fit it on partial dataframe
predict.lm(complete_T, newdata = model_data_V)

summary(complete_T_2 <- lm(Mathematics ~ ., data = model_data_T[,-c(1:4)]))
cpooling_pred <- predict.lm(complete_T_2, newdata = model_data_V[,-c(1:4)])

#Basic charts?
plot(model_data_V$Mathematics, cpooling_pred)
plot(model_data_V$Mathematics, (model_data_V$Mathematics-cpooling_pred))
sum((model_data_V$Mathematics-cpooling_pred)^2)

##No Pooling 
no_pooling_2_T <- blmer(Mathematics ~ Asian.pct +
                        Hispanic.pct +
                        Black.pct +
                        X2014.2015.Pass.Rate +
                        School.Accreditation.Rating +
                        English +
                        Met.Mathematics +
                        History +
                        Science +
                        Total..Full.time...Part.time.Students +
                        FY.2016..Budgeted.Average.Teacher.Salary +
                        (1 + FY.2016..Budgeted.Average.Teacher.Salary | Div.Num),
                      model_data_T,
                      cov.prior = wishart)

summary(no_pooling_2_T)
npooling_pred <- predict(no_pooling_2_T, newdata = model_data_V, allow.new.levels=T)

plot(model_data_V$Mathematics, npooling_pred)
plot(model_data_V$Mathematics, (model_data_V$Mathematics-npooling_pred))
sum((model_data_V$Mathematics-npooling_pred)^2)


##Partial Pooling
partial_pooling_T <- lmer(Mathematics ~ Asian.pct +
                          Hispanic.pct +
                          Black.pct +
                          X2014.2015.Pass.Rate +
                          School.Accreditation.Rating +
                          English +
                          Met.Mathematics +
                          History +
                          Science +
                          Total..Full.time...Part.time.Students +
                          FY.2016..Budgeted.Average.Teacher.Salary +
                          Democratic.pct +
                          Truancy.pct +
                          (1 | Div.Num),
                        model_data_T)

ppooling_pred <- predict(partial_pooling_T, newdata = model_data_V, allow.new.levels=T)

plot(model_data_V$Mathematics, ppooling_pred)
plot(model_data_V$Mathematics, (model_data_V$Mathematics-ppooling_pred))
sum((model_data_V$Mathematics-ppooling_pred)^2)
