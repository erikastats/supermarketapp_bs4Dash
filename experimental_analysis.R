
# libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(echarts4r)
library(stringr)
# library(plotly)

# Import data -------------------------------------------------------------

grocery_table <- read_rds("./Data/grocery_table.rds")
product_table <- read_rds("./Data/product_table.rds")

# Data cleaning -----------------------------------------------------------

grocery_table <- grocery_table |>
  mutate(product_discount = product_discount |>
           replace_na(0),
         value_unit_disc = product_value - product_discount,
         value_total = value_unit_disc*product_quantity
         )


product_table <- product_table  |>
  mutate(product_name_clean = case_when(
    product_name %in% c("FROZEN BLUEBERRIES", "BLUEBERRIES") ~ "BLUEBERRY",
    product_name == "PRINGLES" ~ "CRISPS",
    product_name == "MONSTER 500ML" ~ "ENERGY DRINK",
    .default = product_name )
  ) |>
  distinct(product_name, product_category,
           product_subcategory ,.keep_all = TRUE)


grocery <-  grocery_table |>
  left_join(product_table) |>
  mutate(product_name = product_name_clean) |>
  select(-product_name_clean)

product_table <- product_table |> 
              mutate(product_name = product_name_clean) |>
              distinct(product_name, product_category,
                       product_subcategory ,.keep_all = TRUE) |>
              select(-product_name_clean)
grocery_table <- grocery |>
  select( grocery_day, supermarket_name, product_name, product_category,
          product_value, product_quantity, product_discount, last_update_grocery,
          g_id, value_total)
  
# write_rds(product_table |> 
#             mutate(product_name = product_name_clean) |>
#             distinct(product_name, product_category,
#                      product_subcategory ,.keep_all = TRUE) |>
#             select(-product_name_clean), "./Data/product_table.rds")


# grocery |>
#   select( grocery_day, supermarket_name, product_name, product_category,
#           product_value, product_quantity, product_discount, last_update_grocery,
#           g_id, value_total) |>
#   write_rds("./Data/grocery_table.rds")

# Data grouped ------------------------------------------------------------

# total by day and supermarket 
grocery_supermarket_group <- grocery |> 
  group_by(grocery_day, supermarket_name) |>
  summarise(total_day  = value_total |> sum()) 

# total by day supermarket and category
grocery_category_group <- grocery |>
  group_by(grocery_day, supermarket_name,
           product_category, product_subcategory) |>
  summarise(total_day = value_total |> sum())


# descriptive analysis ---------------------------------------------------


grocery_supermarket_group |> 
  ungroup() |>
  mutate(Year = grocery_day |> year(),
         Month = grocery_day |> month()) |>
  e_charts(grocery_day) |>
  e_line(total_day) |>
  e_title("Total Value of groceries per Day") |>
  e_tooltip(trigger = "axis") |>
  e_datazoom(type = "slider") 

grocery |>
  group_by(grocery_day) |>
  summarise(total_day = value_total |> sum()) |>
  plot_ly(x = ~grocery_day, y = ~total_day,
          type = 'scatter', mode = 'lines') 
  

grocery |>
  group_by(grocery_day) |>
  summarise(total_day = value_total |> sum()) |> 
  e_charts(grocery_day) |> 
  e_calendar(range = "2018") |> 
  e_heatmap(values, coord_system = "calendar") |> 
  e_visual_map(max = 30) |> 
  e_title("Calendar", "Heatmap")
