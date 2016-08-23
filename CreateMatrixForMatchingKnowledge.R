library(lsa)
source('GetMatches.R')
source('CompanyMatchesOutput.R')

# make empty matrix for for values to be added
m <- matrix(0, nrow = nrow(usersSpread), ncol = nrow(tarSpread))
musers <- as.matrix(usersTidy)
mtargets <- as.matrix(tarTidy)

distanceToTargets <- function(user, targets = mtargets){
        distances <- numeric()
        for(i in 1:nrow(targets)){
                distances[i] <- cosine(user, targets[i,])
        }
        #This makes the outcome binary:
        distances[is.na(distances)] <- 0
        distances[distances > 0] <- 1
        return(distances)
}

# usersTidy[281,]
# tarTidy[248,]
# mSum[281,]
# musers[281,]
# x <- distanceToTargets(musers[281,])
# table(is.na(x))
# table(x)
# which(x == max(x))
# usersTidy[281,]
# tarTidy[296,]
# which(x < 0.15 & x > 0)
# 
# 


for(i in 1:nrow(musers)){
        m[i,] <- distanceToTargets(musers[i,])
}

mSum <- m + t(m)


# max(mSum)
# range(mSum)
# tail(mSum)
# hist(mSum)
# length(mSum[mSum > 0])
# 
# x <- which(mSum[1,] == max(mSum[1,]), arr.ind = T)
# x <- which(mSum[38,] > 1, arr.ind = T)
# head(x)

L <- list()
for(i in 1:nrow(mSum)){
        x <- which(mSum[i,] > 1, arr.ind = T)
        y <- x[x != i]
        if(length(y) == 0){
                y <- 0
        }
        L[[i]] <- y
}



# L is a list of matches
# Here there is a list of matches. Now get that into suitable output
# This puts the list of names of matches column-wise. Each column is
# a delgate, with the rows being the matches. Not a great output.
DelegatesToMeet <- GetMatches(L, Data)

# This puts the data into a four-column dataframe, with the list of matches 
# as one \n-separated string in the fourth column.
KnowledgeMatchesOutput <- GetCompanyMatchesOutput(Data, DelegatesToMeet)

write.xlsx(KnowledgeMatchesOutput, 'KnowledgeMatches.xlsx', row.names = F)

