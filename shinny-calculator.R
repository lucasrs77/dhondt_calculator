## rearrage basic information
## random numbers button

## hola

# Load the required libraries
library(shiny)
library(DT)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Seat Allocation Calculator using D'Hondt Method"),
  sidebarLayout(
    sidebarPanel(
      h4("Basic Information"),
      textInput("table_title", "Title for Table:", value = "Seat Allocation Results"),  # Moved to Basic Information
      fluidRow(
        column(6,
               numericInput("seats_total", "Total Number of Seats:", value = 10)
        ),
        column(6,
               numericInput("total_votes", "Total Valid Votes:", value = 200000)
        )
      ),       
      h4("Party 1 Details"),
      fluidRow(
        column(6,
               textInput("party1_label", "Label (max 25 chars):", value = "Party 1"),
               tags$script("$('#party1_label').attr('maxlength','25');")
        ),
        column(6,
               numericInput("votes_party1_percentage", "Percentage of Votes:", value = round(30, 2), min = 0, max = 100, step = 0.01)
        )
      ),
      
      h4("Party 2 Details"),
      fluidRow(
        column(6,
               textInput("party2_label", "Label (max 25 chars):", value = "Party 2"),
               tags$script("$('#party2_label').attr('maxlength','25');")
        ),
        column(6,
               numericInput("votes_party2_percentage", "Percentage of Votes:", value = round(20, 2), min = 0, max = 100, step = 0.01)
        )
      ),
      
      h4("Party 3 Details"),
      fluidRow(
        column(6,
               textInput("party3_label", "Label (max 25 chars):", value = "Party 3"),
               tags$script("$('#party3_label').attr('maxlength','25');")
        ),
        column(6,
               numericInput("votes_party3_percentage", "Percentage of Votes:", value = round(25, 2), min = 0, max = 100, step = 0.01)
        )
      ),
      
      h4("Party 4 Details"),
      fluidRow(
        column(6,
               textInput("party4_label", "Label (max 25 chars):", value = "Party 4"),
               tags$script("$('#party4_label').attr('maxlength','25');")
        ),
        column(6,
               numericInput("votes_party4_percentage", "Percentage of Votes:", value = round(25, 2), min = 0, max = 100, step = 0.01)
        )
      ),
      
      actionButton("calculate_button", "Calculate"),
      
      h4("Percentage Sum:"),  # Moved to Basic Information
      verbatimTextOutput("percentage_sum")  # Moved to Basic Information
    ),
    mainPanel(
      DTOutput("result_table"),
      verbatimTextOutput("warning_text")
    )
  )
)

# Define the server for the Shiny app
server <- function(input, output) {
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
    parties_label <- c(input$party1_label, input$party2_label, input$party3_label, input$party4_label)
    parties_percentage <- c(input$votes_party1_percentage, input$votes_party2_percentage, input$votes_party3_percentage, input$votes_party4_percentage)
    
    # Check if total percentage exceeds 100 or is less than 100
    total_percentage <- sum(parties_percentage)
    if (total_percentage > 100) {
      legend_text <- "Total percentage of parties exceeds 100%."
      result_df <- data.frame(Party = character(0), Percentage = numeric(0), Votes = numeric(0), Seats = numeric(0))
    } else {
      legend_text <- ""
      if (total_percentage < 100) {
        # Create a new party "Others" with remaining percentage
        parties_label <- c(parties_label, "Others")
        parties_percentage <- c(parties_percentage, round(100 - total_percentage, 2))
        parties_votes <- round((parties_percentage / 100) * total_votes)
      } else {
        # Calculate votes for each party based on percentage and total votes
        parties_votes <- round((parties_percentage / 100) * total_votes, 0)
      }
      
      # Allocate seats using D'Hondt method
      seat_allocation <- dhondt_allocation(parties_votes, input$seats_total)
      
      # Create the result data frame
      result_df <- data.frame(Party = parties_label, 
                              Percentage = parties_percentage,
                              Votes = parties_votes,
                              Seats = seat_allocation)
    }
    
    return(list(result_df, legend_text))
  })
  
  output$result_table <- renderDT({
    result_df <- seats_allocation()[[1]]
    datatable(result_df[, 1:4], rownames = FALSE, caption = input$table_title)
  })
  
  output$warning_text <- renderText({
    seats_allocation()[[2]]
  })
  
  output$percentage_sum <- renderText({
    total_percentage <- sum(c(input$votes_party1_percentage, input$votes_party2_percentage, 
                              input$votes_party3_percentage, input$votes_party4_percentage))
    paste0(total_percentage, "%")
  })
}

# Run the Shiny app
shinyApp(ui, server)
