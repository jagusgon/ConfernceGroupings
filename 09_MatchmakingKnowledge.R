# Use this script to match attendees based on knowledge, '3.1'

source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data <- read.csv(paste(datadir, "Smart_Summit_All_Data.csv", sep = ''),
                  header = T, na.strings = '')
Data <- Data[rowSums(is.na(Data)) != ncol(Data),]

df <- Data[,23:35]
names(df)

targets <- df[,1:6]
users <- df[,8:13]
names(targets)
names(users)

# Put underscores between words, but not between answers
targets <- AddUnderscores(targets)
#Spread responses
tarSpread <- SpreadResponses(targets)
names(tarSpread)
# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
tarSpread <- tarSpread[, !names(tarSpread) %in% drops]
colSums(tarSpread)

users <- AddUnderscores(users)
usersSpread <- SpreadResponses(users)
usersSpread <- usersSpread[, !names(usersSpread) %in% drops]
# Tidy these due to bad formatting:
usersSpread$Robotics_and_AI[usersSpread$Robotics_and_AI_ == 1] <- 1
usersSpread <- usersSpread[,names(usersSpread) != 'Robotics_and_AI_']
africa <- 'Smart_technology_African_cities'
names(tarSpread)[34] <- africa
names(usersSpread)[25] <- africa

# Now remove the columns in users that do not appear in targets
names(tarSpread)
names(usersSpread)
names(usersSpread)[!names(usersSpread) %in% names(tarSpread)]
usersTidy <- usersSpread[,names(usersSpread) %in% names(tarSpread)]
names(tarSpread)
names(tarSpread)[!names(tarSpread) %in% names(usersTidy)]
tarTidy <- tarSpread[,names(tarSpread) %in% names(usersTidy)]

# Reorder the columns to match
index <- integer()
for(name in names(tarTidy)){
        index <- c(index, which(names(usersTidy) == name))
}
usersTidy <- usersTidy[, index]
identical(names(usersTidy), names(tarTidy))
