
library(shiny)

ids <- readRDS("data/ids.rds")
questions <- readRDS("data/questions.rds")

convert_input_to_list <- function(input){
  input_parameters <- names(input)
  result <- list()
  if(length(input_parameters) > 0){
    for(i in seq(1,length(input_parameters),1)){
      temp_list <- list()
      temp_list[[input_parameters[i]]] <- input[[input_parameters[i]]]
      result <- c(result,temp_list)
    }
    return(result)
  } else {
    return(list())
  }
}

shinyServer(function(input, output,session) {
  
  user_id <- reactiveValues()
  user_answers <- reactiveValues(is_done=FALSE)
  user_file <- reactiveValues()
  
  get_id_from_url <- observe({
    session$clientData$url_search
    isolate({
      temp_id <- parseQueryString(session$clientData$url_search)$id
      if(!is.null(temp_id) && is.element(tolower(temp_id),ids)){
        user_id$value <- tolower(temp_id)
        initialize_user_answers()
      }
    })
  })
  
  initialize_user_answers <- reactive({
    user_file$value <- paste0("data/user_info/",user_id$value,".rds")
    if(file.exists(user_file$value)){
      temp_read <- readRDS(user_file$value)
      user_answers$value <- temp_read$value
      user_answers$is_question_complete <- temp_read$is_question_complete
      user_answers$question_order <- temp_read$question_order
      user_answers$current_question <- temp_read$current_question
    }  else  {
      user_answers$value <- rep("", length(questions))
      user_answers$is_question_complete <- rep(FALSE, length(questions))
      user_answers$question_order <- sample(length(questions))
      user_answers$current_question <- 1
    }
    user_answers$is_done <- all(user_answers$is_question_complete)
    user_answers$ip <- session$request$REMOTE_ADDR
  })
  
  output$title_box <- renderUI({
    if(is.null(user_id$value)) return(NULL)
    h4(paste0("Logged in as: ",user_id$value))
  })
  
  last_login <- reactive({
    input$process_answer
    value <- NULL
    isolate({
      if(is.null(user_id$value) && !is.null(input$id) && input$id != ""){
        if(is.element(tolower(input$id),ids)) {
          user_id$value <- tolower(input$id)
          initialize_user_answers()
        } else {
          value <- tolower(input$id)
        }
      }
    })
    value
  })
  
  output$error_box <- renderUI({
    if(!is.null(last_login())){
      box(background="red",HTML(paste0(last_login(), " is an invalid code")))
    } else { 
      NULL
    }
  })
  
  output$amount_complete <- renderUI({
    if(is.null(user_id$value)) NULL
    else {
      string <- paste0(sum(user_answers$is_question_complete)," out of ",length(user_answers$value)," complete")
      tags$b(em(string))
    }
  })
  
  output$question_box <- renderUI({
    answer_is_processed()
    user_id$value
    isolate({
      
      if(is.null(user_id$value)){
        textInput("id",label="Login ID",value=ifelse(is.null(input$id),"",input$id))
      } else {
        question_idx_left <- which(!user_answers$is_question_complete)
        if(length(question_idx_left) == 0){
          user_answers$is_done <- TRUE
          h4("You have answered all of the questions, thanks for helping!") 
        } else {
          label <- questions[user_answers$question_order[user_answers$current_question]]
          list(textInput("answer",label=label,value=""),em("(If you don't have an answer write \"SKIP\")"))
        }
      }
    })
  })
  
  answer_is_processed <- reactive({
    input$process_answer
    isolate({
      if(!is.null(input$process_answer) && input$process_answer > 0 &&
           !is.null(user_id$value) && !is.null(input$answer) && input$answer != ""){
        user_answers$is_question_complete[user_answers$question_order[user_answers$current_question] ]<- TRUE
        user_answers$value[user_answers$question_order[user_answers$current_question]] <- input$answer
        user_answers$current_question <- user_answers$current_question + 1
        saveRDS(convert_input_to_list(user_answers),user_file$value)
      }
    })
  })
  
  output$ui_pa <- renderUI({
    if(is.null(user_answers$is_done) || !user_answers$is_done){
      actionButton("process_answer",label="Go")
    } else {
      NULL
    }
  })
})