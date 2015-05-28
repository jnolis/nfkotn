require(dplyr)

final_answers_raw <- read.csv("game/data/answers.csv")

if(max(summarise(group_by(final_answers_raw,question_number),
  distinct_questions = length(unique(question)))$distinct_questions) > 1){
  stop("Some of the question Numbers have different actual questions!")
}

final_answers_temp <- split(final_answers_raw,final_answers_raw$alt_qn)

final_answers <- lapply(final_answers_temp,function(x){
#   if(sum(x$value) > 100 || any(x$value < 2)){
#     stop("Invalid values")
#   }
  y <- list()
  y$question <- as.character(x$question[1])
  y$answers <- x[,c("answer","value")]
  if(nrow(y$answers) > 1 && any(y$answers$value[1:nrow(y$answers)-1]-y$answers$value[2:nrow(y$answers)] < 0)){
    stop("Answers are not in descending order for a question")
  }
  if(nrow(y$answers) > 8){
    #stop("Can't have more than 8 answers to a question")
    y$answers <- y$answers[1:8,]
  }
  y$answers$number <- 1:nrow(y$answers)
  return(y)
})

rm(final_answers_temp,final_answers_raw)
saveRDS(final_answers,"game/data/final_answers.rds")