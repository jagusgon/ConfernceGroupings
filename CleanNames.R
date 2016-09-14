names(usersSpread)
usersSpread <- usersSpread[,sort(names(usersSpread))]
names(usersSpread)

names(usersSpread)[19] <- 'Smart_Home_OEM' ###Must verify this each time!
names(usersSpread)[20] <- 'Smart_Home_OEMs'
usersSpread$Distributors[usersSpread$Distributors_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'Distributors_']
usersSpread$Manufacturers[usersSpread$Manufacturers_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'Manufacturers_']
usersSpread$System_Integrators[usersSpread$System_Integrators_ == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != 'System_Integrators_']
usersSpread$Smart_Home_OEM[usersSpread$`Smart_Home_OEM's` == 1] <- 1
usersSpread <- usersSpread[, names(usersSpread) != "Smart_Home_OEMs"]
usersSpread <- usersSpread[, names(usersSpread) != 
                                   'I_have_limited_knowledge_in_Smart_Home']
usersSpread <- usersSpread[, names(usersSpread) != 
                                   'Security_and_Privacy_in_the_Smart_Home']
usersSpread <- usersSpread[, !names(usersSpread) %in% drops]

tarSpread <- tarSpread[,sort(names(tarSpread))]
usersSpread <- usersSpread[,sort(names(usersSpread))]

CombineSimilarColumns <- function(df, a, b){
        # Takes entries from column b and puts them into col a, then removes
        # col b. Use this when two columns have slightly different names but
        # tell the same information.
        df[[a]][df[[b]] == 1] <- 1
        df <- df[, names(df) != b]
        return(df)
}


a <- 'Insurance_Companies'
b <- 'Insurance_Company'
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- 'Construction_Companies'
b <- 'Construction_Company'
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])


a <- 'Manufacturers'
b <- 'Manufacturer'
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- 'Retailers'
b <- 'Retailer'
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- "Smart_Home_OEM"
b <- "Smart_Home_OEM's"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- "Smart_Home_Platform_Providers"
b <- "Smart_Home_Platform_Provider"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- "Technology_Providers/Vendors"
b <- "Technology_Provider/Vendor"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])

a <- "Telecom_Operators"
b <- "Telecom_Operator"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])
a <- "Transport_Companies"
b <- "Transport_Company"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])
a <- "Utility_Companies"
b <- "Utility_Company"
table(tarSpread[[a]])
table(tarSpread[[b]])
tarSpread <- CombineSimilarColumns(tarSpread, a, b)
table(tarSpread[[a]])
table(tarSpread[[b]])
names(tarSpread)

names(usersSpread)
a <- "Smart_Home_OEM"
b <- "Smart_Home_OEM’s"
table(usersSpread[[a]])
table(usersSpread[[b]])
usersSpread <- CombineSimilarColumns(usersSpread, a, b)
table(usersSpread[[a]])
table(usersSpread[[b]])

for(i in 1:length(names(usersSpread))){
        print(names(usersSpread[i]))
        print(names(tarSpread[i]))
}

#Will remove the variables in users that do not appear in targets
#usersSpread <- usersSpread[,-c(17, 18, 24:28)]

# Change names of users df
# x <- names(tarSpread)
# userNames <- x[c(1, 23, 21, 13, 
#                  4, 9, 3, 14,
#                  8, 12, 15, 7,
#                  9, 13, 10, 17, 
#                  6, 19, 16, 18, 
#                  11, 20, 2, )]
# userNames <- x[c(4, 23, 21, 13, 
#                  4, 9, 3, 14,
#                  8, 12, 15, 7,
#                  9, 13, 10, 17, 
#                  6, 19, 16, 18, 
#                  11, 20, 2, )]

# Make sure it worked
# for(i in 1:22){
#         print(names(tarSpread[i])); print(names(usersSpread)[i])
# }
