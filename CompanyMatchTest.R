Data$Surname[1007]
testUser <- usersSpread[52,]
tarSpread[55,]


TestMatchStrength <- function(user, targetName){
        testUser <- usersSpread[user,]
        testTargetRow <- row.names(Data[Data$Surname == targetName,])
        testTargetRow <- testTargetRow[testTargetRow != 'NA']
        print(testTargetRow)
        testTarget <- tarSpread[testTargetRow,]
        Result <- rbind(testUser, testTarget)
        
        count <- 0
        for(i in 1:ncol(Result)){
                if(Result[1,i] == Result[2,i]){
                        count <- count + 1
                }
        }
        #print(Data$Like.to.Meet[user])
        #print(Data$Industry.All[Data$Surname == targetName])
        print(colMeans(Result))
}

TestMatchStrength(1007, 'Morton')
TestMatchStrength(78, 'Crossley')
Data[99,]
Data[1007,]

Data$Surname[958]
Data$Surname[78]




usersSpread[52,]
tarSpread[973,]
row.names(Data[Data$Surname == 'Lockwood',])


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
table(is.na(Data$Surname))
