path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data2 <- read.csv(paste(datadir, "Smart_Summit_Exact_Data_Export.csv", sep = ''),
                  header = T)

DF2 <- Data2[,22:39]
names(DF2)[c(3,5)] <- c("OtherCompany", "OtherValueInMeeting")

removeBlanks <- function(df){
        df[, colSums(is.na(df)) != nrow(df)]
}
DF2 <- removeBlanks(DF2)
DF2 <- AddUnderscores(DF2)

head(DF2)

str(DF2)
names(DF2)
NamesVector <- names(DF2)
