
library(shiny)
library(shinydashboard)
final_answers <- readRDS("data/final_answers.rds")
final_answers <- c(list(list(question="Are you ready to play?",
  answers=data.frame(answer=c("Yes","Maybe","No"),value=c(90,8,2),number=c(1,2,3),stringsAsFactors=FALSE))),
  final_answers)
start_question <- 1
shinyServer(function(input, output,session) {
  state <- reactiveValues(
    question_number = start_question,
    question = final_answers[[start_question]],
    is_shown = rep(FALSE,nrow(final_answers[[start_question]]$answers)),
    score = c(red=0,yellow=0),
    misses = 0)
  
  next_question <- observe({
    if(!is.null(input$next_question) && input$next_question > 0){
      isolate({
        state$question_number <- state$question_number + 1
        state$question <- final_answers[[state$question_number]]
        state$is_shown <- rep(FALSE,nrow(final_answers[[state$question_number]]$answers))
        state$misses <- 0
      })
    }
  })
  output$scores <- renderUI({
    fluidRow(
      box(background="red",h1(paste0("Red Score: ",state$score["red"]))),
      box(background="yellow",h1(paste0("Yellow Score: ",state$score["yellow"])))
    )
  })
  
  output$board <- renderUI({    
    inner_board <- mapply(function(is_shown,answer,value,number){
      if(is_shown){
        p(h1(span(paste0(number,": ",toupper(answer)," - ",value),style="color:green")))
      } else {
        p(h1(paste0(number,": ANSWER")))
      }
    }, 
      state$is_shown,
      state$question$answers$answer,
      state$question$answers$value,
      state$question$answers$number,SIMPLIFY=FALSE)
    if(length(inner_board) > 4){
      inner_board <- list(do.call(column,c(inner_board[1:4],list(width=6))),
        do.call(column,c(inner_board[-(1:4)],list(width=6))))
    } else {
      inner_board <- list(do.call(column,c(inner_board[1:4],list(width=6))))
    }
    miss <- h3(paste0("Misses: ",paste0(rep("X",state$misses),collapse=" ")))
    list(
      box(h2(state$question$question),width=12,height=120), 
      box(inner_board,width=12,height=300),
      box(miss,width=12,height=80))
  })
  
  check_observer_1 <- observe({ num <- 1
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_2 <- observe({ num <- 2
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_3 <- observe({ num <- 3
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_4 <- observe({ num <- 4
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_5 <- observe({ num <- 5
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_6 <- observe({ num <- 6
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_7 <- observe({ num <- 7
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  check_observer_8 <- observe({ num <- 8
    if(!is.null(input[[paste0("reveal_",num)]]) && input[[paste0("reveal_",num)]] > 0 && isolate(length(state$is_shown) >= num)) isolate({ state$is_shown[num] <- TRUE })
  })
  
  miss_observer <- observe({
    if(!is.null(input$miss) && input$miss > 0){
      isolate({state$misses <- state$misses + 1})
    }
  })
  
  points_yellow_observer <- observe({
    if(!is.null(input$award_yellow) && input$award_yellow > 0){
      isolate({
        state$score[["yellow"]] <- state$score[["yellow"]]  + sum(state$question$answers$value[state$is_shown])*input$multiplier
        state$is_shown <- rep(TRUE,length(state$is_shown))
      })
      
    }
  })
  
  points_red_observer <- observe({
    if(!is.null(input$award_red) && input$award_red > 0){
      isolate({
        state$score[["red"]] <- state$score[["red"]]  + sum(state$question$answers$value[state$is_shown])*input$multiplier
        state$is_shown <- rep(TRUE,length(state$is_shown))
      })
      
    }
  })
  
  output$controls <- renderUI({
    list(fluidRow(
      actionButton("award_red","Points to Red"),p(),
      actionButton("award_yellow","Points to Yellow"),p(),
      actionButton("next_question","Next Question"),p(),
      numericInput("multiplier","Multiplier",value=1,min=1),p(),width=12),p(),
    
      fluidRow(
        column(lapply(1:4,function(i)actionButton(paste0("reveal_",i),paste0("Rev-",i))),width=4),
        column(lapply(5:8,function(i)actionButton(paste0("reveal_",i),paste0("Rev-",i))),width=4)),
    fluidRow(p(),actionButton("miss","Wrong Answer")))
  })
  
})
