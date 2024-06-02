library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)


ui <- fluidPage(
  product_ui("product_r"),
  dataTableOutput("data")
)

server <- function(input, output, session) {
  r <- reactiveValues(product_data = data.frame(
    product_name = character(),
    product_category = character(),
    product_subcategory = character(),
    product_is_favorite = logical(),
    stringsAsFactors = FALSE
  ))
  
  product_server("product_r", r)
  
  output$data <- renderDataTable({
    req(nrow(r$product_data) > 0)
    datatable(r$product_data)
  })
}

shinyApp(ui, server)