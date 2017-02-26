#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Coursera Data Science Capstone: Course Project",
                   tabPanel("Predict The Next Word",
                            HTML("<strong>Author: Manojkumar Parmar</strong>"),
                            br(),
                            HTML("<strong>Date: 26 February 2017</strong>"),
                            br(),
                            img(src = "./headers.png"),
                            # Sidebar
                            sidebarLayout(
                                    sidebarPanel(
                                            helpText("Enter a partially complete sentence to begin the next word prediction"),
                                            textInput("ipString", "Enter a partial sentence here",value = ""),
                                            checkboxInput("twtMode", "Activate Twitter Mode" , value = F),
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                    ),
                                    mainPanel(
                                            h2("Predicted Next Word"),
                                            verbatimTextOutput("prediction"),
                                            strong("Sentence Input:"),
                                            tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                            textOutput('text1'),
                                            br(),
                                            strong("Note:"),
                                            tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                            textOutput('text2')
                                    )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                                    img(src = "./headers.png"),
                                    includeMarkdown("about.md")
                            )
                   )
)
)
