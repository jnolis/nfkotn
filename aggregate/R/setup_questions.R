require(yaml)
require(knitr)

config <- yaml.load_file("aggregate/data/config.yaml")

questions <- config$questions
saveRDS(questions,"aggregate/data/questions.rds")
