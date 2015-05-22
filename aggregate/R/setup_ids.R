require(yaml)
require(knitr)

config <- yaml.load_file("aggregate/data/config.yaml")
if(config$num_characters > 6) stop("At most six characters allowed")
forbidden_characters <- c("i","l","o")
allowed_characters <- setdiff(letters,tolower(forbidden_characters))

combinations <- as.integer(length(allowed_characters)^config$num_characters)
temp_num_ids <- sample.int(combinations,config$num_cards,replace=FALSE)
ids <- rep("",config$num_cards)
for(i in 1:config$num_characters){
  divisor <- length(allowed_characters)^(config$num_characters-i)
  ids <- paste0(ids,allowed_characters[floor(temp_num_ids/divisor)+1])
  temp_num_ids <- temp_num_ids - as.integer(floor(temp_num_ids/divisor)*divisor)
}

saveRDS(ids,"aggregate/data/ids.rds")


knit2pdf("aggregate/R/handout.Rnw")