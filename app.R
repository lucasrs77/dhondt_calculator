library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Seat Allocation Calculator using D'Hondt Method"),
  sidebarLayout(
    sidebarPanel(
      h4("Basic Information"),
      textInput("table_title", "Title for Table:", value = "Seat Allocation Results"),
      numericInput("num_parties", "Number of Parties:", value = 4, min = 3, max = 10, step = 1),  # Added dynamic input
      
      fluidRow(
        column(6,
               numericInput("seats_total", "Total Number of Seats:", value = 10)
        ),
        column(6,
               numericInput("total_votes", "Total Valid Votes:", value = 200000)
        )
      ),
      
      # Loop through the selected number of parties and create input fields
      uiOutput("party_details"),
      
      actionButton("calculate_button", "Calculate"),
      
      h4("Percentage Sum:"),
      verbatimTextOutput("percentage_sum")
    ),
    mainPanel(
      DTOutput("result_table"),
      verbatimTextOutput("warning_text")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$num_parties, {
    output$party_details <- renderUI({
      party_ui <- lapply(1:input$num_parties, function(i) {
        fluidRow(
          column(6,
                 textInput(paste0("party", i, "_label"), paste0("Party ", i, " Label (max 25 chars):"), value = paste0("Party ", i)),
                 tags$script(paste0("$('#party", i, "_label').attr('maxlength','25');"))
          ),
          column(6,
                 numericInput(paste0("votes_party", i, "_percentage"), paste0("Percentage of Votes:"), value = 0, min = 0, max = 100, step = 0.01)
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
    
    result_df <- data.frame(Party = all_parties, 
                            Percentage = all_percentages,
                            Votes = all_votes,
                            Seats = all_seats)
    
    return(result_df)
  })
  
  output$result_table <- renderDT({
    result_df <- seats_allocation()
    datatable(result_df[, 1:4], rownames = FALSE, caption = input$table_title)
  })
  
  output$warning_text <- renderText({
    "Note: Allocation calculation is performed only for parties with 3% or more votes."
  })
  
  output$percentage_sum <- renderText({
    total_percentage <- sum(sapply(1:input$num_parties, function(i) input[[paste0("votes_party", i, "_percentage")]]))
    paste0(total_percentage, "%")
  })
}

shinyApp(ui, server)



