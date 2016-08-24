library(lsa)
source('GetMatches.R')
source('CompanyMatchesOutput.R')

# make empty matrix for for values to be added
m <- matrix(0, nrow = nrow(usersSpread), ncol = nrow(tarSpread))
musers <- as.matrix(usersSpread)
mtargets <- as.matrix(tarSpread)

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
head(m)

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
DelegatesToMeet <- GetMatches(L, Data)

# This puts the data into a four-column dataframe, with the list of matches 
# as one \n-separated string in the fourth column.
CompanyMatchesOutput <- GetCompanyMatchesOutput(Data, DelegatesToMeet)

<<<<<<< HEAD
=======
#write.csv(DelegatesToMeet, 'CompanyMatches.csv', row.names = F)
>>>>>>> 110ba4002d579dc3b5f1b38bdb7cdad347db6140
write.xlsx(CompanyMatchesOutput, 'CompanyMatches.xlsx', row.names = F)

