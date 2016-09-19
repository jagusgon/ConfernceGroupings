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
userVars <- DF2[, c(1,4,5,12,13,14,15)]
targetVars <- DF2[, c(2,3,6,7,8,9,10,11)]

names(userVars)
names(targetVars)

userVarsClean <- userVars[,c(1,2,4,6,7)]
userVarsOthers <- userVars[,c(3,5)]
targetVarsClean <- targetVars[,c(1,3,5,7)]
targetVarsOthers <- targetVars[,c(2,4,6,8)]

# Spread all possible responses for each
users <- SpreadResponses(userVars)
usersClean <- SpreadResponses(userVarsClean)
usersOthers <- SpreadResponses(userVarsOthers)
targets <- SpreadResponses(targetVars)
targetsClean <- SpreadResponses(targetVarsClean)
targetsOthers <- SpreadResponses(targetVarsOthers)

# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
users <- users[, !names(users) %in% drops]
usersClean <- usersClean[, !names(usersClean) %in% drops]
targets <- targets[, !names(targets) %in% drops]
targetsClean <- targetsClean[, !names(targetsClean) %in% drops]

names(usersOthers)
tarnames <- names(targets)
q <- '[Cc]it'
grep(q, tarnames)

names(usersClean) %in% names(targetsClean)

names(usersClean); names(targetsClean)

# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
names(users) %in% drops
names(targets) %in% drops
users <- users[ , !(names(users) %in% drops)]
targets <- targets[, !(names(targets) %in% drops)]
write.csv(users, paste(datadir, 'usersBinary.csv'))
write.csv(targets, paste(datadir, 'targetsBinary.csv'))

sharedVars <- names(users)[names(users) %in% names(targets)]

colSums(users)
usersRed <- users[,colSums(users) > 1]
names(usersRed)
names(userVars)

test <- DF2[,c(1,4)]
test <- SpreadResponses(test)
names(test)
names(test) %in% names(targets)
names(users)
names(targets)
names(users)[5] <- "Utility"
table(users$Smart_Home_Platform_Providers ,users$`Smart_Home_OEM's`)
colSums(users)
