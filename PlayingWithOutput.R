dim(DelegatesToMeet)
test <- DelegatesToMeet[,482]
test <- test[test != '']
test[1]
t2 <- paste(test[1], test[2], sep = \n)
t2
t3 <- writeLines(c(test[1], test[2]))
t3
t4 <- cat(c(test[1], test[2], sep = '\n'))
t5 <- c(test[1], test[2])
cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))
t6 <- paste(test, collapse = '\n')
t6
t7 <- paste(test, collapse = '\n')
t7a <- strsplit(t7, '\n')
t8 <- capture.output(test, collapse = '\n')
library(gtools)
?capture.output
loop.text <- capture.output( for(i in 1:10) cat("i=",i,"\n") )
loop.text
library(stringr)
str_c(test, collapse = '\n')

t9 <- write.csv(paste(writeLines(test)), 'test.csv')
write.csv(cat(paste(test, '\n', collapse = '')), 'test.csv')
t9 <- cat(paste(test, '\n', collapse = ''))
t9 <- capture.output(cat(paste(test, '\n', collapse = '')))
out <- capture.output(paste(test, '\n',collapse = ''))
cat('my title', out, file = 'test2.txt')

methods(format)
cat(paste(test, '\n', collapse = ''), file = 'test.csv')
cat(test, colla)
cat(paste(test, collapse = ''), file = 'test.csv')
str(L)
test2 <- L[482]
write.table(test2, 'test2.txt')

test
T1 <- paste(test, sep = '__', collapse = ',CHAR(13),')
write.csv(T1, "test.csv")

TextVector <- c("Name1", "Name2", "Name3")
paste(TextVector, collapse = '\n')
T2 <- cat(paste(TextVector, collapse = '\n'))

new_text <- paste(TextVector, collapse = '\n')
#write.csv(x = new_text, file = 'somefile.csv', row.names = F)
library(xlsx)
write.xlsx(new_text, "mydata.xlsx")

new_text <- paste(test, collapse = '\n')
write.xlsx(new_text, 'testNew.xlsx')
