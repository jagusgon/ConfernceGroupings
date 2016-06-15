path <- getwd()
datadir <- paste(path, '/data/', sep = '')

DataTemp <- read.csv(paste(datadir, "Registrations_160611.csv", sep = ''), 
                     header = T)
Data <- DataTemp[,17:21]

#Take away spaces, and put them back behind commas
for(i in names(Data)){
        print(i)
        Data[,i] <- gsub(" ", "_", Data[,i])
        Data[,i] <- gsub(",_", ", ", Data[,i])
}

table(Data$I.Represent.A)
table(Data$Other) #Not useful
table(Data$I.want.to.Meet)
table(Data$Other.copy) #Not useful

#Take the following columns and spread them into binary variables
all_EventsOfInterest <- unique(c(
        unlist(strsplit(as.character(Data$Event.of.Interest), ", ")),
        unlist(strsplit(as.character(Data$I.want.to.Meet), ", "))))
Data$id <- 1:nrow(Data)

library(plyr)

DF <- ddply(Data, .(id), function(x)
        table(factor(unlist(strsplit(paste(x$Event.of.Interest, 
                                           x$I.want.to.Meet), ", ")),
                            levels = all_EventsOfInterest)))

#Add back the participant's industry
DF$ClientIndustry <- Data$I.Represent.A

#Put NAs in to clientIndustry where they haven't responded
DF$ClientIndustry <- gsub("^---.*", NA, DF$ClientIndustry)
DF$ClientIndustry <- as.factor(DF$ClientIndustry)

write.csv(DF, "DataBinary.csv", row.names = F)




######################
#playing
a <- c("Industrial Internet", "Ind, Int")
gsub("[ ] & [^, ]", "", a)


# sample from Stackoverflow
age <- c(24, 28, 44, 55, 53)
ethnicity <- c("ngoni", "bemba", "lozi tonga", "bemba tonga other", "bemba tongi")
ethnicity_other <- c(NA, NA, "luvale", NA, NA) 
df <- data.frame(age, ethnicity, ethnicity_other)

str(df$ethnicity)
df$ethnicity <- as.character(df$ethnicity)
df$ethnicity_other <- as.character(df$ethnicity_other)
all_ethnicities <- unique(c(
        unlist(strsplit(df$ethnicity, " ")),
        unlist(strsplit(df$ethnicity_other, " "))
        ))

df$id <- 1:nrow(df)

library(plyr)

ddply(df, .(id), function(x)
        table(factor(unlist(strsplit(paste(x$ethnicity, x$ethnicity_other), " ")),
                     levels = all_ethnicities)))
