library(lsa)

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

L[[1]]

# Put list into a dataframe
CompaniesToMeet <- data.frame(matrix(0, nrow = length(L), ncol = length(L)))
for(i in 1:length(L)){
        for(j in 1:length(L[[i]])){
                CompaniesToMeet[j,i] <- L[[i]][j]

        }
}

# Trim off the blank rows
CompaniesToMeet <- CompaniesToMeet[rowSums(CompaniesToMeet) > 0,]

# Info on the deligates
DeligatesToMeet <- data.frame(matrix(as.character(''), nrow = nrow(CompaniesToMeet),
                                     ncol = ncol(CompaniesToMeet)), stringsAsFactors = F)

names(DeligatesToMeet) <- 1:ncol(CompaniesToMeet)
Deligates <- Data3[,c(4:6, 8)]
# Put the deligates and affilations into a character vector
DeligatesList <- character()
for(i in 1:nrow(Deligates)){
        DeligatesList[i] <- paste(Deligates[i,1],
                                    Deligates[i,2],
                                    Deligates[i,3],
                                    ',',
                                    Deligates[i,4])
        DeligatesList[i] <- gsub(' , ', ', ', DeligatesList[i])
}

for(i in 1:length(names(DeligatesToMeet))){
        names(DeligatesToMeet)[i] <- paste(Deligates[i,1],
                                           Deligates[i,2],
                                           Deligates[i,3])
}

# Grab the names of deligate targets and fill into dataframe
for(i in 1:ncol(CompaniesToMeet)){
        for(j in 1:length(CompaniesToMeet[,i])){
                if(CompaniesToMeet[j,i] > 0){
                        Person <- CompaniesToMeet[j,i]
                        DeligatesToMeet[j,i] <- DeligatesList[Person]
                }
        }
}

for(i)

length(DeligatesList)

head(DeligatesToMeet[38])
temp
#lapply(L, write, 'test4.txt', append=T, sep=', ', ncolumns=1000)

str(L)
maxLength <- 0
for(i in 1:length(L)){
        if(length(L[[i]]) > maxLength){
                maxLength <- length(L[[i]])
        }
}


df <- data.frame(matrix(unlist(l), nrow=132, byrow=T))

test <- data.frame(matrix(unlist(L), ncol = 482, nrow = maxLength, byrow = F))
        