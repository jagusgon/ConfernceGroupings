AddUnderscores <- function(x){
        for(i in names(x)){
                x[,i] <- gsub(" ", "_", x[,i])
                x[,i] <- gsub(",_", ", ", x[,i])
        }
        return(x)
}

