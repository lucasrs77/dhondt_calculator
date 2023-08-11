library(shiny)
library(DT)
library(rsconnect)
library(shinythemes)
library(shi18ny)




ui <- fluidPage(theme = shinytheme("cerulean"),
                
        useShi18ny(),
        langSelectorInput("lang", position = "right"),
                
  titlePanel(ui_("Calculadora de asignación de escaños utilizando el método D'hondt")),
  sidebarLayout(
    sidebarPanel(
      h4("Información Básica"),
      textInput("table_title", "Título de la tabla:", value = "Seat Allocation Results"),
      numericInput("num_parties", "Número de partidos:", value = 4, min = 3, max = 10, step = 1),  # Added dynamic input
      
      fluidRow(
        column(6,
               numericInput("seats_total", "Total de bancas:", value = 10)
        ),
        column(6,
               numericInput("total_votes", "Total de votos validos:", value = 200000)
        )
      ),
      
      # Loop through the selected number of parties and create input fields
      uiOutput("party_details"),
      
      actionButton("calculate_button", "Calcular"),
      
      h4("Suma de porcentaje:"),
      verbatimTextOutput("percentage_sum"),
      
    ),
    mainPanel(
      DTOutput("result_table"),
      verbatimTextOutput("warning_text"),
       downloadLink('downloadData', 'Download'),
      
      tags$div(
        class = "citation",
        HTML("If you use this app in your work, please consider citing it as: <br> Lucas Rodriguez Saa (2023). <i>Calculadora de escaños D'hondt</i>. [Shiny App]. Retrieved from [https://lucasrs77.shinyapps.io/dhondt_calculator/]")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  i18n <- list(
    defaultLang = "es",
    availableLangs = c("en", "es")
  )
  
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector = TRUE)
  

  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  
  observeEvent(input$num_parties, {
    output$party_details <- renderUI({
      party_ui <- lapply(1:input$num_parties, function(i) {
        fluidRow(
          column(6,
                 textInput(paste0("party", i, "_label"), paste0("Partido ", i, " Label:"), value = paste0("Partido ", i)),
                 tags$script(paste0("$('#party", i, "_label').attr('maxlength','25');"))
          ),
          column(6,
                 numericInput(paste0("votes_party", i, "_percentage"), paste0("Porcentaje de votos:"), value = 0, min = 0, max = 100, step = 0.01)
          )
        )
      })
      do.call(tagList, party_ui)
    })
  })
  
  dhondt_allocation <- function(votes, total_seats) {
    num_parties <- length(votes)
    seats <- rep(0, num_parties)
    
    for (i in 1:total_seats) {
      div <- votes / (seats + 1)
      max_idx <- which.max(div)
      seats[max_idx] <- seats[max_idx] + 1
    }
    
    return(seats)
  }
  
  seats_allocation <- eventReactive(input$calculate_button, {
    total_votes <- input$total_votes
    num_parties <- input$num_parties
    parties_label <- sapply(1:num_parties, function(i) input[[paste0("party", i, "_label")]])
    parties_percentage <- sapply(1:num_parties, function(i) input[[paste0("votes_party", i, "_percentage")]])
    
    # Filter parties with percentage >= 3%
    valid_parties <- parties_label[parties_percentage >= 3]
    valid_percentages <- parties_percentage[parties_percentage >= 3]
    
    # Calculate votes for each party based on percentage and total votes
    valid_votes <- round((valid_percentages / 100) * total_votes, 0)
    
    # Allocate seats using D'Hondt method
    seat_allocation <- dhondt_allocation(valid_votes, input$seats_total)
    
    # Create the result data frame including all parties
    all_parties <- parties_label
    all_percentages <- parties_percentage
    all_votes <- round((all_percentages / 100) * total_votes, 0)
    all_seats <- ifelse(all_percentages >= 3, seat_allocation, 0)
    
    result_df <- data.frame(Partido = all_parties, 
                            Porcentaje = all_percentages,
                            Votos = all_votes,
                            Bancas = all_seats)
    
    return(result_df)
  })
  
  
  output$result_table <- renderDT({
    result_df <- seats_allocation()
    datatable(result_df[, 1:4], rownames = FALSE, caption = input$table_title)
  })
  
  output$warning_text <- renderText({
    "Nota: El cálculo de asignación se realiza solo para listas con un \n3% o más de votos válidos."
  })
  
  output$percentage_sum <- renderText({
    total_percentage <- sum(sapply(1:input$num_parties, function(i) input[[paste0("votes_party", i, "_percentage")]]))
    paste0(total_percentage, "%")
  })
  
  # download button
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('seat_allocation-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      result_df <- rbind(seats_allocation(), citation_lines)
      table_title <- input$table_title
      
      # Create a character vector with citation details
      citation_lines <- c(
        "",
        "Citation: Lucas Rodriguez Saa (2023). Calculadora de escaños D'hondt Shiny App. 
        URL: https://lucasrs77.shinyapps.io/dhondt_calculator/")
      
      # Write the table data to CSV
      write.csv(result_df, con, row.names = FALSE)
      }
   )
}

shinyApp(ui, server)



