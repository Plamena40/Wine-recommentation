library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyverse)
library(caret)
library(pROC)
library(ROCR)
library(ROSE)
library(ResourceSelection)
library(PresenceAbsence)
library(shinythemes)
library(bslib)
library(MLeval)
library(shiny)
library(shinyjs)
library(rsconnect)
library(boot)
library(glmnet)


df <- read.table("data/wine.data", sep = ",", header = FALSE)
names(df) <- c("Class", "Alcohol", "Malicacid", "Ash", "Alcalinity_of_ash", 
               "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols",
               "Proanthocyanins", "Color_intensity", "Hue", "0D280_0D315_of_diluted_wines",
               "Proline")

df$Class <- factor(make.names(df$Class))
df$Wine <- ifelse(df$Class == "X1", "Barolo",
           ifelse(df$Class == "X2", "Grignolino",
           ifelse(df$Class == "X3", "Barbera", NA)))

df$Flavanoids_cat <- ifelse(df$Flavanoids > median(df$Flavanoids), 1, 0)
df$Color_intensity_cat <- ifelse(df$Color_intensity > median(df$Color_intensity), 1, 0)

load("mod1.RData")


# Theme set up
theme_hi <- bs_theme(
  bg = "#722F37",
  fg = "white",
  base_font = font_google("Tangerine"),
  code_font = font_google("Tangerine"),
  primary = "#D9B38C", 
  secondary = "#9C6D5E"
)

# UI set up
ui <- fluidPage(
  theme = theme_hi,
  tags$style(HTML("
    body {
      background-color: rgba(114, 47, 55, 0.85);
      font-size: 42px;
    }
    .sidebar {
      background-color: rgba(105, 36, 44, 0.85);
      font-size: 42px;
    }
    .main-panel {
      background-color: #ffffff;
      font-size: 42px;
    }
    .btn-primary {
      background-color: #D9B38C;
      border-color: #D9B38C;
      font-size: 22px;
    }
   .btn-primary:hover {
      background-color: #B88F5A;
      border-color: #B88F5A;
      font-size: 22px;
    }
    .btn-primary:active {
      background-color: #B88F5A;
      border-color: #B88F5A;
      font-size: 22px;
      border-width: 4px;
    }
    h1 {
      color: #581845;
      font-size: 56px;
      font-weight: bold;
    }
    h3, h4 {
      color: #D9B38C;
      font-size: 42px;
    }
    .table {
      background-color: rgba(255, 255, 255, 0.8);
      color: #722F37;
      font-size: 42px;
    }
    h4 {
      color: white; 
    }
    h5 {
      color: white; 
      font-size: 20px;
    }
    .ui-slider {
      font-size: 32px
    }
    .ui-slider .ui-slider-handle {
      height: 40px;
      width: 40px;
      background: #D9B38C;
      border-radius: 50%;
    }
    .footer-text {
      position: fixed;
      bottom: 10px;
      right: 10px;
      color: white;
      font-size: 20px;
      z-index: 1000;
    }
  ")),
  
  h1("WineMatch: Your personalized wine pairing guide"),
  h3("Wine Preferences"),
  sidebarLayout(
    sidebarPanel(
      width = 12,
      sliderInput("predictor1", "Alcohol (%):",
                  min = 11.03, max = 14.83, value = 12, step = 0.01),
      h4("Color Intensity"),
      actionButton("predictor2_1", "Low", class = "btn-primary"),
      actionButton("predictor2_2", "High", class = "btn-primary"),
      br(),
      h4("Flavanoids"),
      actionButton("predictor3_1", "Low", class = "btn-primary"),
      actionButton("predictor3_2", "High", class = "btn-primary")
    ),
    mainPanel(
      fluidRow(
        column(8, 
               h3("Wine Recommendation"),
               tableOutput("predictionTable")
        ),
        column(4, 
               h4(textOutput("wineName")),  
               imageOutput("wineImage"),
        )
      )
    )
  ),
  tags$div("Created by Plamena P. Powla", class = "footer-text")
)

## Server set up
server <- function(input, output, session) {
  
  predictor2_val <- reactiveVal(1)
  predictor3_val <- reactiveVal(1)
  shinyjs::useShinyjs()
  
  observeEvent(input$predictor2_1, {
    predictor2_val(0) 
    shinyjs::toggleClass(selector = "#predictor2_1", class = "active-btn")
  })
  
  observeEvent(input$predictor2_2, {
    predictor2_val(1)
    shinyjs::toggleClass(selector = "#predictor2_2", class = "active-btn") 
  })
  
  observeEvent(input$predictor3_1, {
    predictor3_val(0) 
    shinyjs::toggleClass(selector = "#predictor3_1", class = "active-btn")
  })
  
  observeEvent(input$predictor3_2, {
    predictor3_val(1) 
    shinyjs::toggleClass(selector = "#predictor3_2", class = "active-btn")
  })
  
  reactivePrediction <- reactive({
    new_data <- data.frame(
      Alcohol = input$predictor1,
      Color_intensity_cat = predictor2_val(),
      Flavanoids_cat = predictor3_val() 
    )

    pred_probs <- predict(mod1, new_data, type = "prob")
    
    # Normalizing to ensure probabilities sum to 1. 
    # this may be the case due to rounding
    pred_probs <- pred_probs/rowSums(pred_probs)
    pred_probs*100
  })
  
    output$predictionTable <- renderTable({
    Probabilities <- reactivePrediction()
    
    Probabilities <- data.frame(
      Barbera = paste0(round(Probabilities$Barbera, 1), "%"),
      Barolo = paste0(round(Probabilities$Barolo, 1), "%"),
      Grignolino = paste0(round(Probabilities$Grignolino, 1), "%")
    )
    Probabilities
  })
  
  output$wineName <- renderText({
    Probabilities <- reactivePrediction()
    max_prob <- max(Probabilities)
    
    if (max_prob == Probabilities$Barbera) {
      "Barbera"
    } else if (max_prob == Probabilities$Barolo) {
      "Barolo"
    } else {
      "Grignolino"
    }
  })
  
  output$wineImage <- renderImage({
    Probabilities <- reactivePrediction()
    max_prob <- max(Probabilities)
    
    if (max_prob == Probabilities$Barbera) {
      img_src <- "www/Barbera.jpg"
    } else if (max_prob == Probabilities$Barolo) {
      img_src <- "www/Barolo.jpg"
    } else {
      img_src <- "www/Grignolino.jpg"
    }

    list(src = img_src, height = 150, width = "auto")
  }, deleteFile = FALSE)
}


shinyApp(ui = ui, server = server)
