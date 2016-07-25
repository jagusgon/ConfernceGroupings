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
x <- which(mSum[1,] == max(mSum[1,]), arr.ind = T)
x <- which(mSum[38,] > 1, arr.ind = T)
head(x)

L <- list()
for(i in 1:nrow(mSum)){
        x <- which(mSum[i,] > 1, arr.ind = T)
        y <- x[x != i]
        L[[i]] <- y
}

lapply(L, write, 'test4.txt', append=T, sep=', ', ncolumns=1000)
