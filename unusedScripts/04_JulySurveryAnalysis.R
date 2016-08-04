source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data2 <- read.csv(paste(datadir, "Smart_Summit_Exact_Data_Export.csv", sep = ''),
                  header = T)

DF2 <- Data2[,22:39]
names(DF2)[c(3,5)] <- c("OtherCompany", "OtherValueInMeeting")

removeBlanks <- function(df){
        df[, colSums(is.na(df)) != nrow(df)]
}

# remove columns that contain only NA
DF2 <- removeBlanks(DF2)
# Put underscores between words, but not between answers
DF2 <- AddUnderscores(DF2)
# Spread all answers into a binary dataframe
DF3 <- SpreadResponses(DF2)
# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
DF4 <- DF3[ , !(names(DF3) %in% drops)]
write.csv(DF4, paste(datadir, 'SmartSummitBinary.csv'))

names(DF2)
names(DF4)
