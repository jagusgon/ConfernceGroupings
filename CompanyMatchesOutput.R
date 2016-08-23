library(xlsx)
# write.xlsx(new_text, "mydata.xlsx")
# 
# new_text <- paste(test, collapse = '\n')
# write.xlsx(new_text, 'testNew.xlsx', row.names = F)
# 
# secondNew_text <- data.frame(FirstName = 'Jon',
#                              LastName = 'Leslie',
#                              Matches = new_text)
# secondNew_text <- rbind(secondNew_text, c('Mark', 'Johnson', new_text))
# write.xlsx(secondNew_text, 'testNewSecond.xlsx', row.names = F)

TextVector <- c("Name1", "Name2", "Name3")
new_text <- paste(TextVector, collapse = '\n')

thirdNew_text <- data.frame(matrix(ncol = 3, nrow = 2))
names(thirdNew_text) <- c('FirstName', 'SecondName', 'Matches')
thirdNew_text[1,] <- c('Jon', 'Leslie', new_text)
thirdNew_text[2,] <- c('Mark', 'Johnson', new_text)
write.xlsx(thirdNew_text, 'testNewThird.xlsx', row.names = F)

Delegates <- Data[,c(5,6,9)] #FirstName, Surname, email
CompanyMatchesOutput <- data.frame(matrix(ncol = 4, nrow = nrow(Delegates)))
names(CompanyMatchesOutput) <- c('FirstName', 'Surname', 'email', 'Matches')
names(Data)

head(DelegatesToMeet)
test <- DelegatesToMeet[,482] # TRIM OFF THE BLANK ROWS
testCollapse <- paste(test, collapse = '\n')
thirdNew_text[1,] <- c('Jon', 'Leslie', testCollapse)
thirdNew_text[2,] <- c('Mark', 'Johnson', testCollapse)
write.xlsx(thirdNew_text, 'testNewFourth.xlsx', row.names = F)
