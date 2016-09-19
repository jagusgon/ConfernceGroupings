TestKnowledgeMatches <- function(){
        userName <- readline('What is the name of the user? (f, l)  ')
        targetName <- readline('What is the name of the target? (f l)  ')
        
        userNameSplit <- unlist(strsplit(userName, ' '))
        targetNameSplit <- unlist(strsplit(targetName, ' '))
        
        userNumber <- row.names(Data[Data$First.Name == userNameSplit[1] &
                Data$Surname == userNameSplit[2],])
        targetNumber <- row.names(Data[Data$First.Name == targetNameSplit[1] &
                                             Data$Surname == targetNameSplit[2],])
        
        a <- as.vector(usersTidy[userNumber,], mode = 'numeric')
        b <- as.vector(tarTidy[targetNumber,], mode = 'numeric')
        
        return(cosine(a, b))
}
