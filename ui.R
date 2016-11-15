
library(shiny)
require(mvtnorm)
require(pander)
require(markdown)
require(stringr)
source('source-code/functions.R')

shinyUI(navbarPage(a("New Window", href = "https://shiny.roche.com/public/manserp1/sample-size-eval/"),

# Main Tab ---------------------
  tabPanel("Main", 

# side panel ---------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
    
      tabsetPanel(type = "tabs", 
# full study tab ---------------------------
                  tabPanel("Full Study",
                           
                           fluidRow(
                             column(6, # cv
                                    numericInput("cv", label = h4("CV"), value = 0.7, step = .01, min = 0, max = 99)
                             ),
                             column(6, # out
                                    numericInput("dropout", label = h4("Dropout"), value = .25, min = 0, max = 1, step = 0.01)
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    textInput('n', label = h4("N per arm"), value = "70:100:10, 130")
                                    ),
                             
                             column(6, # alpha
                                    numericInput("alpha", label = h4("Significance Level"), value = 0.2, min = 0, max = 1, step = 0.01)
                               )
                             ),
                           br(),br(),
                   
                           fluidRow(
                             column(6, # tarpp
                                    numericInput("tarpp", label = h4("Target PP"), value = .25, min = 0, max = 1, step = 0.05)
                             ),
                             column(6, # minpp
                                    numericInput("minpp", label = h4("Minimum PP"), value = .15, min = 0, max = 1, step = 0.05)
                             )
                           ),
                           
                           fluidRow(
                             column(6, # tarpp
                                    numericInput("th.go", label = h4("Go Threshold"), value = .25, min = 0, max = 1, step = 0.01)                             
                             ),
                             column(6, # minpp
                                    numericInput("th.stop", label = h4("Stop Threshold"), value = .15, min = 0, max = 1, step = 0.01)                             
                             )
                           ),
                           
                           actionButton(inputId = 'recomp', label = 'Compute')
                           
                           ), 
                  
# interim tab -----------------
                  tabPanel("Interim",
                           
                           # add interim
                           radioButtons("add.interim", label = "Include Interim?", selected = "no", inline = TRUE,
                                        choices = c("Yes" = "yes", "No" = "no")),
                           br(),
                           
                           fluidRow(
                             column(6, #interim dropout rate
                                    numericInput("dropout.int", label = "Dropout at Interim", value = .25, min = 0, max = 1, step = 0.01)
                             ),
                             column(6, # dropout
                                    numericInput("int.corr", label = "Correlation with final read out", value = 0.5, min = 0, max = 1, step=0.01)
                             )
                           ),
                           br(),br(),

                           
                                    numericInput("cv.int", label = h4("CV"), value = 1.7, step = .01, min = 0, max = 99),
                        
                         
                           
                           fluidRow(
                             column(6,
                                    numericInput('n.int', label = h4("N per arm (Single Value)"), value = 50)
                             ),
                             
                             column(6, # alpha
                                    numericInput("alpha.int", label = h4("Significance Level"), value = 0.2, min = 0, max = 1, step = 0.01)
                             )
                           ),
                           br(),br(),
                           
                           fluidRow(
                             column(6, # minpp
                                    numericInput("th.stop.int", label = h4("Interim Stop Threshold"), value = .3, min = 0, max = 1, step = 0.01)
                             ),
                             column(6, # tarpp
                                    numericInput("th.go.int", label = h4("Interim Go Threshold"), value = .4, min = 0, max = 1, step = 0.01)
                             )
                           ),
                           
                           actionButton(inputId = 'recomp2', label = 'Compute')
                           
                           )
                  
      )
       
  ),

# main panel ----------------------------------------------------------------------
    mainPanel(
      fluidRow(
        column(4, textInput(inputId = "study.title", label = "Study Title", value = "Example Study")),
        column(4, textInput(inputId = "study.author", label = "Author", value = "John Q Statsman")),
        column(4, br(), downloadButton(outputId = "ss.rept", label = "Download Powerpoint"))),
      tags$hr(),
      
      
      uiOutput("interim.title"),
      uiOutput("interim.table"),
      h3("Full Study"),
      uiOutput("htmltable"),
      uiOutput("joint.fp.tbl")
    )
  )
  ),
  

# Options Tab ------------------
  tabPanel("Options",
           
     fluidRow(
       column(6, 
              radioButtons("shading.switch", label="Shading",                                 # sidedness
                           choices = list("On" = 1, "Off" = 2), selected=1),
              
              numericInput("power.ctrl", label = "Target Power", value = .8, min = 0, max = 1,  step = 0.05),
              
              numericInput("fp.ctrl", label = "Target False Positive Rate", value = .1, min = 0, max = 1,  step = 0.05),
              
              numericInput("fn.ctrl", label = "Target False Negative Rate", value = .2, min = 0, max = 1,  step = 0.05),
              
              tags$hr(),
              
              numericInput("nsim", label = h4("N Simulations used for FP/FN Rates"), value = 1e5, min = 0, max = Inf, step = 100) ),
       
       column(6,
              h4("Show Columns"),
              radioButtons("show.mdd", label = "MDD", choices = c(Yes="yes", No="no"), selected = "yes", inline = TRUE),
              radioButtons("show.es80", label = "ES80", choices = c(Yes="yes", No="no"), selected = "yes", inline = TRUE),
              tags$hr(),
              radioButtons("show.tpp.pow", label = "Power at Target PP", choices = c(Yes="yes", No="no"), selected = "yes", inline = TRUE),
              radioButtons("show.mpp.pow", label = "Power at Minimum PP", choices = c(Yes="yes", No="no"), selected = "no", inline = TRUE),
              tags$hr(),
              radioButtons("show.fpr.stop", label = "False Positive Rate at Stop Threshold", choices = c(Yes="yes", No="no"), selected = "yes", inline = TRUE),
              radioButtons("show.fn.stop", label = "False Negative Rate at Stop Threshold", choices = c(Yes="yes", No="no"), selected = "yes", inline = TRUE),
              tags$hr(),
              radioButtons("show.fpr.go", label = "False Positive Rate at Go Threshold", choices = c(Yes="yes", No="no"), selected = "no", inline = TRUE),
              radioButtons("show.fn.go", label = "False Negative Rate at Go Threshold", choices = c(Yes="yes", No="no"), selected = "no", inline = TRUE)
              )
       
       )
           
         

  )
     
  
))

