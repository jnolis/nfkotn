library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidRow(
      box(
        title="Nerd Feud with King of the Nerds!",
        background = "blue",
        HTML("Log in using the code on the paper you received. Answer as many questions as you'd like!"),
        uiOutput("title_box")
      ),
      uiOutput("error_box"),
      box(
        background="green",
        uiOutput("question_box"),
        uiOutput("ui_pa"),
        uiOutput("amount_complete"))
    )
  )
)
