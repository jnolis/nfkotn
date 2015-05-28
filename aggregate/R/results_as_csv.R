x <- data.frame(question=character(0),answer=character(0),value=numeric(0))

for(i in sample(1:length(questions))){
  #answers <- sapply(results,function(x) gsub("[^[:print:]]","",toupper(x$value[[i]])))
  answers <- sapply(results,function(x) gsub("[^[:alnum:]]","",toupper(x$value[[i]])))
  answers <- answers[!is.element(answers,c("SKIP", ""))]
  answers2 <- data.frame(table(answers))
  
  answers2 <- rename(answers2,answer=answers,value=Freq)
  answers2 <- answers2[order(answers2$value,decreasing=TRUE),]
  answers2$question = questions[i]
  answers2$question_number = i
  answers2$answer_number = 1:nrow(answers2)
  answers2 <- answers2[,c("question_number","question","answer_number","answer","value")]
  x <- rbind_list(x,answers2)
}

write.csv(x,"answers.csv")

