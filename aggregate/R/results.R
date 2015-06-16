require(knitr)

require(dplyr)
result_files <- list.files("aggregate/data/user_info")
results <- list()
for(file in result_files){
  user_id <- substring(file,1,nchar(file)-4)
  results[[user_id]] <- readRDS(file.path("aggregate","data","user_info",file))
} 

ids <- readRDS("aggregate/data/ids.rds")
questions <- readRDS("aggregate/data/questions.rds")

knit2pdf("aggregate/R/results.Rnw")