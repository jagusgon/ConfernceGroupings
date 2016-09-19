# Use this script to attach people and companies, 'Outcome 2'

source('AddUnderscores.R')
source('SpreadResponses.R')

library(lsa)
library(xlsx)
source('GetMatches.R')
source('CompanyMatchesOutput.R')


path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
FileMS2 <- 'Milestone2utf8.csv'
File130916 <- 'SSL_Reg_13.09.16.csv'


Data <- read.csv(paste(datadir, File130916, sep = ''),
                 header = T, na.strings = '')
Data <- Data[rowSums(is.na(Data)) != ncol(Data),]

DF3 <- Data[,19:20]
#names(DF3)[c(2,4)] <- c("OtherCompany", "OtherValueInMeeting")

targets <- DF3[,1]
#table(is.na(targets$Industry.All))
#levels(targets$Industry.All)

#table(is.na(targets))

# Remove blank columns:
# removeBlanks <- function(df){
#         df[, colSums(is.na(df)) != nrow(df)]
# }

# Put underscores between words, but not between answers
targets <- AddUnderscores(as.data.frame(targets))
#Spread responses
tarSpread <- SpreadResponses(as.data.frame(targets))

# Tidy up column names. This is manual and is difficult. USE CARE!!!
# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '.*please.*select.*', 'NEED.*INFO',
           'NEED_INFO')
#Only works if there is an exact match, not for regex:
tarSpread <- tarSpread[, !names(tarSpread) %in% drops]
tarSpread$Utility_Company[tarSpread$Utility == 1] <- 1
tarSpread <- tarSpread[,names(tarSpread) != 'Utility']

users <- DF3[,2]
users <- AddUnderscores(as.data.frame(users))
#Sys.setlocale('LC_ALL','') 
usersSpread <- SpreadResponses(as.data.frame(users)) ###problem here!



#### Go to CleanNames.R here!!!!!!!!!!!!!!
source('CleanNames.R')

test <- '.*please.*select.*'
!grepl(test, names(tarSpread))
!grepl(drops, names(tarSpread))
test2 <- tarSpread[,!grepl(test, names(tarSpread))]
names(test2)

result <- names(tarSpread)
for(i in drops){
        print(i)
        result <- result[!grepl(i, result)]
        print(result)
}

grepl('na', names(tarSpread)) # 'na' is not just 'na', but anything contiaing 'na'

names(tarSpread) %in% 'Other'
test %in% names(tarSpread)
test
