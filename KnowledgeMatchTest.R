#Sys.setlocale('LC_ALL','') 

Data$Surname[994]
testUser <- usersTidy[262,]

TestMatchStrength2 <- function(user, targetSurname, targetFirstname){
        testUser <- usersTidy[user,]
        testTargetRow <- row.names(Data[Data$Surname == targetSurname &
                                                Data$First.Name == targetFirstname,])
        testTargetRow <- testTargetRow[testTargetRow != 'NA']
        print(testTargetRow)
        testTarget <- tarTidy[testTargetRow,]
        Result <- rbind(testUser, testTarget)
        
        count <- 0
        for(i in 1:ncol(Result)){
                if(Result[1,i] == Result[2,i] &
                   Result[1,i] == 1){
                        count <- count + 1
                }
        }
        print(count)
}

Data$Surname[935]
Data$Surname[1819]
TestMatchStrength2(935, 'Hayes', 'Ian')
TestMatchStrength2(1183, 'Shires', 'Sam')
TestMatchStrength2(1819, 'Ingold', 'Maria')
TestMatchStrength2(1819, 'Bron', 'Ceriel')


user <- 1183
targetSurname <- 'Bron'
targetFirstname <- 'Ceriel'


usersTidy[1183,]
tarTidy[263,]

# TestMatchStrength <- function(user, targetName){
#         testUser <- usersTidy[user,]
#         testTargetRow <- row.names(Data[Data$Surname == targetName,])
#         testTargetRow <- testTargetRow[testTargetRow != 'NA']
#         print(testTargetRow)
#         testTarget <- tarTidy[testTargetRow,]
#         Result <- rbind(testUser, testTarget)
#         
#         count <- 0
#         for(i in 1:ncol(Result)){
#                 if(Result[1,i] == Result[2,i]){
#                         count <- count + 1
#                 }
#         }
#         print(count)
# }
# TestMatchStrength(4, 'Shaw')
# Data$Surname[1183]
# TestMatchStrength(1183, 'Bron')
# TestMatchStrength(1183, 'Piva')
# TestMatchStrength(1183, 'Maclennan')
# 



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

Data$Surname[994]
grepl('*', as.character(Data$Surname[994]))
grepl('.*', 'Jon')
test <- as.character(Data$Surname[994], Encoding('UTF-8'))
x <- "fa\xE7ile"
Encoding(x)
Encoding(test)
Encoding(test) <- 'UTF-8'
x
test
Encoding()
Sys.getlocale()
Data2 <- read.csv(paste(datadir, "Milestone2test.csv", sep = ''),
                 header = T, na.strings = '')
Data2 <- Data2[rowSums(is.na(Data2)) != ncol(Data2),]
Data2$Surname[994]


#should be 'native.enc'?
getOption('encoding')
