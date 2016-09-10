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
Data <- read.csv(paste(datadir, "Milestone2utf8.csv", sep = ''),
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
           'NEED_INFO', '---_please_select_---')
#Only works if there is an exact match, not for regex:
tarSpread <- tarSpread[, !names(tarSpread) %in% drops]
tarSpread$Utility_Company[tarSpread$Utility == 1] <- 1
tarSpread <- tarSpread[,names(tarSpread) != 'Utility']

users <- DF3[,2]
users <- AddUnderscores(as.data.frame(users))
#Sys.setlocale('LC_ALL','') 
usersSpread <- SpreadResponses(as.data.frame(users)) ###problem here!
names(usersSpread)[10] <- 'Smart_Home_OEM' ###Must verify this each time!
usersSpread$Distributors[usersSpread$Distributors_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'Distributors_']
usersSpread$Manufacturers[usersSpread$Manufacturers_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'Manufacturers_']
usersSpread$System_Integrators[usersSpread$System_Integrators_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'System_Integrators_']
usersSpread$Smart_Home_OEM[usersSpread$`Smart_Home_OEM's` == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != "Smart_Home_OEM's"]
usersSpread <- usersSpread[, names(usersSpread) != 
                                   'I_have_limited_knowledge_in_Smart_Home']
usersSpread <- usersSpread[, names(usersSpread) != 
                                   'Security_and_Privacy_in_the_Smart_Home']
usersSpread <- usersSpread[, !names(usersSpread) %in% drops]

tarSpread <- tarSpread[,sort(names(tarSpread))]
usersSpread <- usersSpread[,sort(names(usersSpread))]

#Will remove the variables in users that do not appear in targets
#usersSpread <- usersSpread[,-c(17, 18, 24:28)]

# Change names of users df
# x <- names(tarSpread)
# userNames <- x[c(1, 23, 21, 13, 
#                  4, 9, 3, 14,
#                  8, 12, 15, 7,
#                  9, 13, 10, 17, 
#                  6, 19, 16, 18, 
#                  11, 20, 2, )]
# userNames <- x[c(4, 23, 21, 13, 
#                  4, 9, 3, 14,
#                  8, 12, 15, 7,
#                  9, 13, 10, 17, 
#                  6, 19, 16, 18, 
#                  11, 20, 2, )]

# Make sure it worked
# for(i in 1:22){
#         print(names(tarSpread[i])); print(names(usersSpread)[i])
# }

# Get both dataframes to have the same attributes
names(usersSpread) <- names(tarSpread)
#tarSpread <- tarSpread[, names(tarSpread) %in% userNames]

#Get the columns the dataframes to be in the same order
# index <- integer()
# for(name in names(tarSpread)){
#         index <- c(index, which(names(usersSpread) == name))
# }
# 
# # This is the dataframe with all company varibles spread out
# usersSpread <- usersSpread[, index]


# make empty matrix for for values to be added
m <- matrix(0, nrow = nrow(usersSpread), ncol = nrow(tarSpread))
musers <- as.matrix(usersSpread)
mtargets <- as.matrix(tarSpread)

# This is a time-consuming step:
distanceToTargets <- function(user, targets = mtargets){
        distances <- numeric()
        for(i in 1:nrow(targets)){
                distances[i] <- cosine(user, targets[i,])
        }
        distances[is.na(distances)] <- 0
        distances[distances > 0] <- 1
        return(distances)
}

for(i in 1:nrow(musers)){
        m[i,] <- distanceToTargets(musers[i,])
}

mSum <- m + t(m)

# Generate list of matches for each user
L <- list()
for(i in 1:nrow(mSum)){
        x <- which(mSum[i,] > 1, arr.ind = T)
        y <- x[x != i]
        if(length(y) == 0){
                y <- 0
        }
        L[[i]] <- y
}

# Here thre is a list of matches. Now get that into suitable output
# This puts the list of names of matches column-wise. Each column is
# a delgate, with the rows being the matches. Not a great output.
DelegatesToMeet <- GetMatches(L, Data) # SLOW!!!

# This puts the data into a four-column dataframe, with the list of matches 
# as one \n-separated string in the fourth column.
CompanyMatchesOutput <- GetCompanyMatchesOutput(Data, DelegatesToMeet)

#write.csv(DelegatesToMeet, 'CompanyMatches.csv', row.names = F)

#write.xlsx(CompanyMatchesOutput, 'CompanyMatchesMS2.xlsx', row.names = F)

