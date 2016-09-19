library(fpc)
library(xlsx)
source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
FileMS2 <- 'Milestone2utf8.csv'
File130916 <- 'SSL_Reg_13.09.16.csv'
File190916 <- 'Registration19Sept.csv'

Data <- read.csv(paste(datadir, File190916, sep = ''),
                 header = T, na.strings = '')
Data <- Data[rowSums(is.na(Data)) != ncol(Data),]

# Changed from first version...now only grouping on interest in further learning
DF <- Data[,28:33]
#names(DF)[c(3,5)] <- c("OtherCompany", "OtherValueInMeeting")

removeBlanks <- function(df){
        df[, colSums(is.na(df)) != nrow(df)]
}

# remove columns that contain only NA
DF <- removeBlanks(DF)
# Put underscores between words, but not between answers
DF <- AddUnderscores(DF)
# Spread all answers into a binary dataframe
DFSpread <- SpreadResponses(DF)

# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '.*please.*select.*', 'NEED.*INFO',
           'NEED_INFO', '---_please_select_---', 'n/a', '-', '.', 'none',
           'X', 'None', 'N/a', 'No_interest', '%', 'n.a', 'tbc', 'n.a.', 'NA')
DFSpread <- DFSpread[ , !(names(DFSpread) %in% drops)]

Delegates <- Data[,c(5,6,8,9)] #FirstName, Surname, company, email

LearningsSpread <- cbind(Delegates, DFSpread)

write.xlsx(LearningsSpread, 'LearningsSpreadWithCompanies_M3.xlsx', row.names = F)
