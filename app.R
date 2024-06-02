
# Libraries ---------------------------------------------------------------

library(shiny)
library(reactable)
library(dplyr)
library(tidyr)
library(bs4Dash)
library(shinyWidgets)
library(stringr)
library(tibble)


# Data --------------------------------------------------------------------

product_df <- tibble(p_name = NULL, type = NULL, subtype = NULL,
                     is_favorite = NULL)



# Header ------------------------------------------------------------------


HEADER = dashboardHeader(
  title = "Grocery Journal"
)


# Sidebar -----------------------------------------------------------------

SIDEBAR = dashboardSidebar(
  id = "sidebar",
  dashboardBrand(
    title = "Grocery Journal",
    image = "https://www.academiedugout.fr/images/8479/1200-auto/brocolis_000.jpg"
  ),
  sidebarMenu(
    id = "sidebarMenu",
    menuItem( text = "Product", 
              tabName = "p_register",
              icon = icon("box-open")),
    menuItem("Grocery",
             tabName = "g_register",
             icon = icon("basket-shopping")),
    menuItem("Analysis",
             tabName = "analysis",
             icon = icon("chart-line"))
  )
  )


# Body --------------------------------------------------------------------

BODY  = dashboardBody(
  tabItems(
    tabItem( tabName = "p_register",
             h2("Product registration"),
             h4("Register a new product to use it to register your grocery shopping latter"),
             br(),
             br(),
             box(width = 12,
                  fluidRow(
                   column(6,
                          product_ui("add_product")
                          ),
                   column(6,
                          actionBttn(
                            inputId = "exclude_product",
                            label = "Exclude product",
                            icon = icon("minus")
                          ))
                 ),
                 hr(),
                 br(),
                 reactableOutput("products_table")
                 
                 )
             ),
    tabItem( tabName = "g_register"),
    tabItem( tabName = "analysis")
  )
)

# UI ----------------------------------------------------------------------

UI = dashboardPage(
  header = HEADER,
  sidebar = SIDEBAR,
  body = BODY,
  controlbar = dashboardControlbar(),
  title = "Grocery journal"
)

# Server ------------------------------------------------------------------

SERVER <- function(input, output, session){
  
  # reactive values
  
  r <- reactiveValues(product_data = tibble(
    product_name = character(),
    product_category = character(),
    product_subcategory = character(),
    product_is_favorite = logical(),
    timestamp = ymd_hms(character())
  ))
  
  # Module
  
  product_server("add_product", r)
  # Events
  
  # output
  output$products_table <- renderReactable({
    req(nrow(r$product_data) > 0)
    r$product_data |>
      reactable()
  })
  
}


# App ---------------------------------------------------------------------


shinyApp(ui = UI, server = SERVER)