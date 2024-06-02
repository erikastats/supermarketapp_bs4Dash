product_category <- tibble(
  category = c("Fresh Food"),
  subcategory = c("Fresh fruit", "Fresh Vegetables",
                  "Saladas & Dips", "Milk", "Yoghurt", 
                  "Chilled Desserts", "Fresh Meals, Pizza, Pasta & Garlic Bread",
                  "Savoury Pastry", "Juices & Smoothies")
) |> 
  bind_rows(
    tibble(
      category = c("Bakery"),
      subcategory = c("In Store Bakery", "Bread", "Croissant & Pastries", 
                      "Cakes & Pies", "Doughnuts, Muffins & Cookies")
    )
  ) |>
  bind_rows(tibble(
    category = c("Food Cupboard"),
    subcategory = c("Tins & Packets", "Cooking Ingredients", "Dried pasta, Rice, Noodles",
                    "Cooking Sauces", "Condiments & Table Sauces", "Pickles, Chutneys & Olives",
                    "Bakins, Desserts & Spreads", "Biscuits", "Chocolate", 
                    "Sweets, Mints & Chewing Gum", "Sugar & Sweeteners", "Crisps, nuts and Popcorn",
                    "Breakfast Cereals", "Tea & Coffee", "Hot Chocolate & Malted Drinks" )
  )) |>
  bind_rows(tibble(
    category = c("Drinks"),
    subcategory = c("Water", "Adult soft Drinks", "Juice", "Mixers, Tonic and Soda Water",
                    "Sports and Energy Drinks", "Beer & Cider", "Wine")
  )) |>
  bind_rows(tibble(
    category = c("Health & Beauty"),
    subcategory = c("Haircare", "Oral Care", "Feminine Care",
                    "Beauty & Skincare", "Health Care", "Toiletries", 
                    "Deodorants & Body Sprays", "Cosmetics", "Eye Care", 
                    "Medicine")
  ) ) |>
  bind_rows(tibble(
    category = c("Househould"),
    subcategory = c("Cleaning", "Dishwashers & Washing up", "Food Storage", 
                    "Home Fragrance", "Household Sundries", "Laundy", 
                    "Toilet Paper, Kitchen Rolls & Tissues", "Cleanning Tools & Gloves")
  )) |>
  bind_rows(tibble(
    category = c("Home & Living"),
    subcategory = c("Paperchase", "Bathroom Accessories & Towels", "Batteries", "Cooking & Dining",
                    "Garden & Outdoor", "Flowers", "Electrical",
                    "Stationery, Arts & Crafts"
                   ) )
  ) |>
  bind_rows(tibble(
    category = c("Frozen Food"),
    subcategory = c("Frozen Ice Cream & Desserts", "Frozen Fruit & Ice Cubes",
                    "Frozen Meals, Pizza", "Frozen Chips, Potatoes & Onion Rings",
                    "Frozen Vegetables")
  ))