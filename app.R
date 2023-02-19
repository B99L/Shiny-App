library(shinythemes)
library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(stringr)
library(dtplyr)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Badevio - MedInfo"),
  sidebarPanel(
    tags$hr(),
    uiOutput(outputId = 'drug'),
    uiOutput(outputId = 'Seq'),
    uiOutput(outputId = 'Altersbereich'),
    uiOutput(outputId = 'sex'),
    uiOutput(outputId = 'showTab1'),
    
  ),
  mainPanel(
    tableOutput("Meldungen"),
    tableOutput("Sequenz"),
    tableOutput("Indikationen"),
    tableOutput("Reaktionen"),
    plotOutput("Therapie"),
    plotOutput("Outcome"),
    plotOutput("Altersgruppe"),
    plotOutput("Therapielaenge")
    
  )
)

server <- function(input, output, session) {
  rl <- reactiveValues()
  observe({
    dat <-
      readRDS(file = "C:/Users/nevio/OneDrive - ZHAW/Dokumente/GitHub/pm2/new_data.rds")
    rl$dat <- dat
    
  })
  
  output$drug <- renderUI({
    selectInput(
      inputId = "drug",
      label = "Medikament",
      choices = c(unique(rl$dat$drugname)),
      multiple = FALSE,
      selectize = FALSE
    )
  })
  
  output$Seq <- renderUI({
    numericInput(
      "Seq",
      label = "Sequenz",
      value = 1,
      min = min(rl$dat$Sequenz),
      max = max(rl$dat$Sequenz)
    )
  })
  
  output$Altersbereich <- renderUI({
    sliderInput(
      "Altersbereich",
      label = "Altersbereich",
      min = 0,
      max = 100,
      value = c(40, 60)
    )
  })
  output$sex <- renderUI({
    checkboxGroupInput(
      "sex",
      label = "Geschlecht",
      choices = c('M', 'F'),
      inline = FALSE
    )
  })
  
  output$showTab1 <- renderUI({
    actionButton(inputId = "showTab1", label = "Update")
  })
  
  Tab1 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      group_by(Quartal) %>%
      summarise(Meldungen = n())
    
  })
  output$Meldungen <-
    renderTable({
      Tab1()
    }, striped = T, bordered = T, rownames = F, colnames = T)
  
  
  
  
  Tab2 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      group_by(Sequenz) %>%
      summarise(Meldungen = n())
    
  })
  output$Sequenz <-
    renderTable({
      Tab2()
    } , striped = T, bordered = T, rownames = F, colnames = T)
  
  
  
  
  
  
  Plot1 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      ggplot(aes(x = periode)) +
      geom_histogram(fill = 'orange') +
      labs(title = "Verteilung der Therpielänge",
           x = "Tage")
    
    
    
  })
  
  output$Therapie <- renderPlot({
    Plot1()
  })
  
  
  
  
  
  Tab3 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      filter(!is.na(Indikation)) %>%
      group_by(Indikation) %>%
      summarise(Meldungen = n()) %>%
      slice_max(order_by = Meldungen, n = 10)
    
    
  })
  output$Indikationen <-
    renderTable({
      Tab3()
    } , striped = T, bordered = T, rownames = F, colnames = T)
  
  
  
  
  Tab4 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      filter(!is.na(Reaktion)) %>%
      group_by(Reaktion) %>%
      summarise(Meldungen = n()) %>%
      slice_max(order_by = Meldungen, n = 10)
    
    
  })
  output$Reaktionen <-
    renderTable({
      Tab4()
    } , striped = T, bordered = T, rownames = F, colnames = T)
  
  
  
  Plot5 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      filter(!is.na(start_dt)) %>%
      filter(is.na(end_dt)) %>%
      filter(!is.na(outc_cod)) %>%
      ggplot(aes(x = outc_cod, fill = outc_cod)) +
      geom_bar() +
      labs(title = "Verteilung des Outcomes",
           x = "Outcome") +
      coord_flip()
    
    
    
  })
  output$Outcome <- renderPlot({
    Plot5()
  })
  
  Plot6 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      filter(!is.na(age_grp)) %>%
      filter(!age_grp == "") %>%
      ggplot(aes(x = age_grp, fill = age_grp)) +
      geom_bar() +
      labs(title = "Meldungen nach Altergsgruppe",
           x = "Altersgruppe") +
      coord_flip()
    
    
    
  })
  output$Altersgruppe <- renderPlot({
    Plot6()
  })
  
  Plot7 <- eventReactive(eventExpr = input$showTab1, {
    rl$dat %>%
      filter(drugname == input$drug) %>%
      filter(age >= input$Altersbereich[1] &
               age <= input$Altersbereich[2]) %>%
      filter(Sequenz %in% input$Seq) %>%
      filter(sex %in% input$sex) %>%
      filter(!is.na(periode)) %>%
      filter(!periode == "") %>%
      filter(periode > 0) %>%
      filter(!is.na(age_grp)) %>%
      filter(!age_grp == "") %>%
      ggplot(aes(y = periode, x = age_grp, fill = age_grp)) +
      geom_boxplot() +
      labs(title = "Therapielänge nach Altersgruppe",
           x = "Altersgruppe", y = "Therapielaenge")
  
  })
  output$Therapielaenge <- renderPlot({
    Plot7()
  })
  
}

shinyApp(server = server, ui = ui)