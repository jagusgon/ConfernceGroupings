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

# Split DF2 into users and item groups, aka users, target
userCompVars <- DF2[, c(4,5)]
targetCompVars <- DF2[, c(2,3)]

userComp <- SpreadResponses(userCompVars)
targetComp <- SpreadResponses(targetCompVars)
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
userComp <- userComp[, !names(userComp) %in% drops]
targetComp <- targetComp[, !names(targetComp) %in% drops]

names(userComp)
names(targetComp)

change <- targetCompVars[,2]
q <- '.*[Cc]onsult.*'
change <- gsub(q, 'consultancy', change)
table(grepl(q, userCompVars))
table(grepl(q, userCompVars[,2]))
test <- gsub(q, 'TEST', userCompVars[,2])
q <- '.*[Cc]harit.*'
change <- gsub(q, 'charity', change)
q <- '.*IoT.*'
change <- gsub(q, 'IoT', change)
q <- 'PR.*'
change <- gsub(q, 'PR', change)

targetCompVars
grepl(q, targetCompVars[,2])
table(grepl(q, targetCompVars[,2]))
table(grepl(q, userCompVars))


Consultancy <- grepl(q, targetCompVars[,2])
grep(q, targetCompVars[,2], value = T)
Consultancy <- gsub(q, 'consultancy', targetCompVars[,2])
table(Consultancy)
userCompNameVector <- c('Smart_Home',
                        'Utility',
                        'Telecom',
                        'Technology',
                        'Smart_Home',
                        'Insurance',
                        'Manufacturers',
                        'Government'
                        )
table(userComp$Smart_Home_Platform_Providers, userComp$`Smart_Home_OEM's`)
