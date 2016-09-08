Data$Surname[262]
testUser <- usersTidy[262,]

TestMatchStrength <- function(user, targetName){
        testUser <- usersTidy[user,]
        testTargetRow <- row.names(Data[Data$Surname == targetName,])
        print(testTargetRow)
        testTarget <- tarTidy[testTargetRow,]
        Result <- rbind(testUser, testTarget)
        
        count <- 0
        for(i in 1:ncol(Result)){
                if(Result[1,i] == Result[2,i]){
                        count <- count + 1
                }
        }
        print(count)
}

TestMatchStrength(262, 'Stacey')


testTargetRow <- row.names(Data[Data$Surname == 'Kelmendi',])
testTarget <- tarTidy[testTargetRow,]
Result <- rbind(testUser, testTarget)
colMeans(Result)

count <- 0
for(i in 1:ncol(Result)){
        if(Result[1,i] == Result[2,i]){
                count <- count + 1
        }
}

count
