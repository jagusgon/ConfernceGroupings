# Use this script to attach people and companies, 'Outcome 2'

source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data <- read.csv(paste(datadir, "Smart_Summit_All_Data.csv", sep = ''),
                  header = T, na.strings = '')
Data <- Data[rowSums(is.na(Data)) != ncol(Data),]
DF3 <- Data[,19:22]
names(DF3)[c(2,4)] <- c("OtherCompany", "OtherValueInMeeting")

targets <- DF3[,1:2]
table(is.na(targets$Industry.All))
levels(targets$Industry.All)


# removeBlanks <- function(df){
#         df[, colSums(is.na(df)) != nrow(df)]
# }

# Put underscores between words, but not between answers
targets <- AddUnderscores(targets)
str(targets)
#Spread responses
tarSpread <- SpreadResponses(targets)
names(tarSpread)
# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
tarSpread <- tarSpread[, !names(tarSpread) %in% drops]
colSums(tarSpread)

users <- DF3[,3:4]
users <- AddUnderscores(users)
str(users)
usersSpread <- SpreadResponses(users)
names(usersSpread)
usersSpread <- usersSpread[, !names(usersSpread) %in% drops]

#Will remove the variables in users that do not appear in targets
usersSpread <- usersSpread[,-c(17, 18, 24:28)]

# Change names of users df
x <- names(tarSpread)
userNames <- x[c(1, 14, 15, 2, 
                 4, 8, 22, 7,
                 6, 24, 11, 12,
                 10, 3, 20, 23, 
                 19, 16, 18, 17, 5)]
# Make sure it worked
# for(i in 1:21){
#         print(userNames[i]); print(names(usersSpread)[i])
# }

# Get both dataframes to have the same attributes
names(usersSpread) <- userNames
tarSpread <- tarSpread[, names(tarSpread) %in% userNames]

#Get the columns the dataframes to be in the same order
# dim(tarSpread)
# dim(usersSpread)
# names(tarSpread)
# names(usersSpread)
 
index <- integer()
for(name in names(tarSpread)){
        index <- c(index, which(names(usersSpread) == name))
}

# This is the dataframe with all company varibles spread out
usersSpread <- usersSpread[, index]
