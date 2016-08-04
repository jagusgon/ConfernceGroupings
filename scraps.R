# Look at cosine distances
findFn(string = "cosine", maxPages = 2, sortby = "MaxScore")

#install.packages('lsa')
#install.packages('spatialEco')
sapply(L, class)
length(L[[204]])

L[122]
L[116]
usersSpread[420,]
tarSpread[46,]
cosine(musers[122,], mtargets[170,])

usersSpread[56,]
tarSpread[116,]
usersSpread[56,]
tarSpread[116,]

hist(summary(L)$Length)
usersSpread[481:486,]


m3 <- m + t(m)
which(m3 == max(m3), arr.ind = TRUE)
x <- which(m3 == 2, arr.ind = TRUE)
which(m3[1,] == 2, arr.ind = T)

for(i in 1:length(L)){
        print(length(L[[i]]))
}





test <- distanceToTargets(vec1)
table(test)
apply(m4, 1, sum)
m4

test <- apply(vec11, 1, distanceToTargets)
test <- mapply(distanceToTargets, vec11)
test[482,]
table(test[482,])




vec1 <- as.vector(usersSpread[482,], mode = 'numeric')
vec11 <- musers[482:483,]
distanceToTargets(vec1)
m[482,]


str(mtargets[1,])
str(mtargets[482,])

cosine(vec1, mtargets[1,])
cosine(vec1, mtargets[482,])


library(spatialEco)
vec1 = c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
cosine(vec1,vec2) 

result <- numeric()
for(i in 1:nrow(targetsTrim)){
        vec2 <- as.vector(targetsTrim[i,], mode = 'numeric')
        result[i] <- cosine(vec1, vec2)   
}

rowSums(usersSpread)
usersTrim <- usersSpread[rowSums(usersSpread) > 0,]
vec1 <- as.vector(usersTrim[1,], mode = 'numeric')
targetsTrim <- tarSpread[rowSums(tarSpread) > 0,]
vec2 <- as.vector(targetsTrim[1,], mode = 'numeric')
vec2a <- c(0, 0, 0, 1, 0, 0, 0, 1,  vec2[9:21])
cosine(vec1, vec2a)

table(result)
max(rowSums(usersTrim))
max(rowSums(targetsTrim))
vec1 <- as.vector(usersTrim[11,], mode = 'numeric')

m <- matrix(0, 5, 5)
m[1,] <- c(0, 0, 1, 0, 1)
m[2,] <- c(1, 0, 1, 1, 0)
m[3,] <- c(0, 1, 0, 0, 0)


t(m)
m3 <- m + t(m)
which(m3 == max(m3), arr.ind = TRUE)
x <- which(m3 == 2, arr.ind = TRUE)
which(m3[1,] == 2, arr.ind = T)

##########################################
names(Data3)
userNames
test <- Data3[, c(19,24)]
test <- SpreadResponses(test)
names(test)
x
test2 <- Data3[,c(20,24)]
test2 <- SpreadResponses(test2)
names(test2)


test <- CompaniesToMeet[,38]
x <- test[1]
print(DeligatesList[x])
test2 <- character()
for(i in 1:length(test)){
        if(test[i] > 0){
                temp <- test[i]
                test2[i] <- DeligatesList[temp]
        }
}
