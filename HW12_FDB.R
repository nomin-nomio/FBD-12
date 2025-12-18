# 1. Install packages
install.packages("readxl")
install.packages("tidyverse") # Only run once
library(tidyverse)
library(readxl)
library(writexl)
install.packages("shiny")
library(shiny)

# 2. Data import
# ./, ../, 
bikes_tbl <- read_excel("./bikes.xlsx") # fast key: alt+-
bikeshops_tbl <- read_excel("./bikeshops.xlsx")
orderlines_tbl <- read_excel("./orderlines.xlsx")

# Examine data:
bikes_tbl
head(bikes_tbl)

# Import csv file:
bike_orderlines_tbl <- read_csv("./bike_orderlines.csv")

# Joining data: 
orderlines_bikes_tbl <- left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_bikeshops_joined <- left_join(orderlines_bikes_tbl, bikeshops_tbl, 
                                              by = c('customer.id' = 'bikeshop.id'))


# %>% is called pipe: fast key: ctl + shift + m 
bike_orderlines_bikeshops_joined <- left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id")) %>% 
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Wrangling data: decompose description into three columns: category.1, category.2 and frame.material
bike_orderlines_wrangled_tbl <- bike_orderlines_bikeshops_joined %>% 
  separate(description, 
           into = c('category.1', 'category.2', 'frame.material'), 
           sep  = ' - ') %>% 
  separate(location, 
           into = c('city', 'state'), 
           sep  = ', ',
           remove = FALSE) %>%
  mutate(total.price = price * quantity) %>% 
  # Reorganize columns                                  
  select(-...1, -location) %>% 
  # Reorder columns                                  
  select(contains('date'), contains('id'), 
         contains('order'), 
         quantity, price, total.price, 
         everything()) %>% 
  # Rename columns
  rename(order_date = order.date) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
# save the file as RDS
saveRDS(bike_orderlines_wrangled_tbl, './bike_orderlines.rds')

# dplyr and tidyr
# pull() vs. select()
bike_orderlines_wrangled_tbl %>% 
  # select(total_price)
  pull(total_price) %>% 
  mean()
# select_if
bike_orderlines_wrangled_tbl %>% 
  # select_if(is.character)
  select_if(is.numeric)

# arrange() and desc()
bikes_tbl %>% 
  select(model, price) %>% 
  arrange(desc(price))

# filter()  
bikes_tbl %>% 
  select(model, price) %>% 
  filter(price > mean(price))

bikes_tbl %>% 
  select(model, price) %>% 
  filter((price > 5000) & (price < 10000)) %>%    
  arrange(desc(price))

bikes_tbl %>% 
  select(model, price) %>% 
  filter(price > 6000, 
         model %>% str_detect("Supersix"))

# Filtering one or more conditions using == and %in%
bike_orderlines_wrangled_tbl %>% 
  filter(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")) %>% 
  View()

# slice()
bikes_tbl %>% 
  arrange(desc(price)) %>% 
  # slice(1:5)
  slice((nrow(.)-4):nrow(.))

# distinct(): extract unique values from data
bike_orderlines_wrangled_tbl %>% 
  distinct(category_1, category_2) %>% 
  View()

# mutate(): add new columns in our data
bike_orderlines_wrangled_tbl %>% 
  mutate(total_price_log = log(total_price)) %>% 
  mutate(total_price_sqrt = total_price^0.5) %>% 
  View()

# Binning with ntile()
bike_orderlines_wrangled_tbl %>% 
  mutate(total_price_binned = ntile(total_price, 3)) %>% 
  View()

# case_when(): provide flexible conditions for grouping (binning)
bike_orderlines_wrangled_tbl %>% 
  mutate(total_price_binned = ntile(total_price, 3)) %>% 
  mutate(total_price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium", 
    TRUE ~ "Low"
  )) %>% 
  View()

# Grouping and summarizing with group_by() and summarize()
bike_orderlines_wrangled_tbl %>% 
  summarise(revenue = sum(total_price))

bike_orderlines_wrangled_tbl %>% 
  group_by(category_1) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  arrange(desc(revenue))

# 
bike_orderlines_wrangled_tbl %>%  
  group_by(category_1, category_2, frame_material) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  arrange(desc(revenue))

# summarize_all()


# Q1: What are the unique categories of products? 
bike_orderlines_wrangled_tbl %>% distinct(category_1)
bike_orderlines_wrangled_tbl %>% distinct(category_2)
bike_orderlines_wrangled_tbl %>% distinct(frame_material)

# Q2: Which product categories have the largest sales? 
# category_1
bike_orderlines_wrangled_tbl %>% 
  select(category_1, total_price) %>% 
  group_by(category_1) %>% 
  summarise(sales = sum(total_price)) %>%
  ungroup() %>% 
  rename(`Primary Category` = category_1, 
         Sales = sales) %>% 
  # format dollars
  mutate(Sales1 = Sales %>% scales::dollar())


# lubridate package
library(lubridate)
# Generate total sales by year
# check the data structure
str(bike_orderlines_wrangled_tbl)

bike_sales_y <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>% 
  # change order_date into ymd format
  mutate(order_date = ymd(order_date)) %>% 
  mutate(year = year(order_date)) %>% 
  # group by year
  group_by(year) %>% 
  summarise(sales = sum(total_price)) %>% 
  ungroup()

# Compute total sales by month and year

bike_orderlines_wrangled_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(order_date = ymd(order_date)) %>% 
  mutate(year  = year(order_date),
         month = month(order_date, label = TRUE)) %>% 
  group_by(year, month) %>% 
  summarise(sales = sum(total_price)) %>% 
  ungroup()

# Time series aggregation: floor_date
# floor_date() is to reduce a date to the nearest unit. 
# similar to ceiling_date()
bike_sales_m <- bike_orderlines_wrangled_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(order_date = ymd(order_date)) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month) %>% 
  summarise(sales = sum(total_price))

# Measure the change such as annual change in numbers or percentage
# dplyr::lag()
# lag is to align past observations with future observations
bike_sales_y %>% 
  mutate(sales_lag_1 = lag(sales_year, n = 1)) %>% 
  # How to handle the NA values? 
  # Replace NA with sales in year 2011
  mutate(sales_lag_1 = case_when(
    is.na(sales_lag_1) ~ sales_year,
    TRUE ~ sales_lag_1
  )) %>% 
  # calculate difference and percentage change by year
  mutate(diff_1 = sales - sales_lag_1) %>% 
  mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
  mutate(pct_diff_1_chr = scales::percent(pct_diff_1))

# Calculate monthly change
# Try to write a function to do the same job again!
calculate_pct_diff <- function(data){
  
  data %>%
    mutate(sales_lag_1 = lag(sales, n = 1)) %>% 
    # How to handle the NA values? 
    # Replace NA with sales in year 2011
    mutate(sales_lag_1 = case_when(
      is.na(sales_lag_1) ~ sales,
      TRUE ~ sales_lag_1
    )) %>% 
    # calculate difference and percentage change by year
    mutate(diff_1 = sales - sales_lag_1) %>% 
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
    mutate(pct_diff_1_chr = scales::percent(pct_diff_1))
  
}

calculate_pct_diff(bike_sales_m)

# dplyr::first()
bike_sales_y %>% 
  mutate(sales_2011 = first(sales))
# dplyr::cumsum()
bike_sales_y %>% 
  mutate(cumulative_sales = cumsum(sales))

# library(ggplot2)  
# Try to generate a plot of total sales by year
bike_sales_y %>% 
  # prepare a clean canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  geom_point(size = 5) + 
  geom_line(linewidth = 2) + 
  geom_smooth(method =  "lm", formula = 'y ~ x', se = FALSE) +
  # formatting y axis to numbers
  # change y axis starting from zero
  expand_limits(y = c(0, 20e6)) + 
  scale_colour_continuous(low = "red", high = "black", 
                          labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) + 
  labs(
    title    = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x        = "year", 
    y        = "Sales (Millions)",
    color    = "Rev ($M)",
    caption  = "Total sales from 2011 to 2015"
  )

# Bar plot
# Generate total sales by category 2
revenue_by_category2_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(category_2, total_price) %>% 
  group_by(category_2) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup()
# bar plot
revenue_by_category2_tbl %>% 
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(desc(revenue))) %>% 
  ggplot(aes(category_2, revenue)) +
  geom_col(fill = "blue")+
  coord_flip()

# Scatter plot
order_value_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(order_id, order_line, total_price, quantity) %>% 
  group_by(order_id) %>% 
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>% 
  ungroup()

order_value_tbl %>% 
  ggplot(aes(x = total_quantity, y = total_price)) +
  geom_point(alpha = 0.5, size = 2)



# Histogram / density plot
## Great for inspecting the distribution of a variable
## Goal: Distribution of unit price of bicycles

bike_orderlines_wrangled_tbl %>% 
  distinct(model, price) %>% 
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "blue", color = "white")

## Histrogram including model and frame material information
library(tidyquant)
bike_orderlines_wrangled_tbl %>% 
  distinct(price, model, frame_material) %>% 
  ggplot(aes(price, fill = frame_material)) +
  geom_histogram() + 
  
  facet_wrap(~ frame_material, ncol = 1) + 
  
  scale_fill_tq() + 
  theme_tq()

## Generate density plot
bike_orderlines_wrangled_tbl %>% 
  distinct(price, model, frame_material) %>% 
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5)+
  scale_fill_tq() + 
  theme_tq()


# Box plot
## Great for comparing distribution
unit_price_by_cat2_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(category_2, model, price) %>% 
  distinct() %>% 
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))


## Generate box plot
unit_price_by_cat2_tbl %>% 
  ggplot(aes(category_2, price)) + 
  geom_boxplot() + 
  
  coord_flip()+
  theme_tq()

# Adding text and labels
## geom_text() and geom_label

revenue_by_year <- bike_orderlines_wrangled_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(year = year(order_date)) %>% 
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>% 
  ungroup() 

## Add text to bar chart
revenue_by_year %>% 
  ggplot(aes(year, revenue)) + 
  geom_col(fill = "blue") + 
  
  geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")), 
            vjust = 1.5, color = "white") + 
  
  geom_label(label = "Major Demand This Year",
             vjust = -0.5, 
             size = 5) + 
  expand_limits(y = 2e7)

# Lollipop chart: Top N customers 

# Q1: How much purchasing power is in top 5 or 10 customers?
# Goal: Visualize top N customers in terms of revenue, 
# include cumulative percentage.
n <- 10

# Q2: Heatmaps 
# Show details in 3 dimensions
# To find specific customers that have purchasing preferences
# Goal: Visualize heatmap of proportion of sales by secondary product
topN_customers <- bike_orderlines_wrangled_tbl %>% 
  select(bikeshop_name, total_price) %>% 
  mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump(n = n, w = total_price)) %>% 
  group_by(bikeshop_name) %>% 
  summarize(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  
  mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>% 
  mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after =0)) %>% 
  arrange(desc(bikeshop_name)) %>% 
  
  # Visualize the data
  mutate(revenue_text = scales::dollar(revenue)) %>% 
  # Cummulative percent
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>% 
  mutate(cum_pct_txt = scales::percent(cum_pct)) %>% 
  # Generate rank column         
  mutate(rank = row_number()) %>% 
  mutate(rank = case_when(
    rank == max(rank) ~NA_integer_, 
    TRUE ~ rank
  )) %>% 
  # label text "Rank" and rank number 
  mutate(label_text = str_glue("Rank: {rank}\nRev {revenue_text} \ncum {cum_pct_txt}"))


# Generate plot
topN_customers %>% 
  ggplot(aes(revenue, bikeshop_name)) +
  
  geom_segment(aes(xend = 0, yend = bikeshop_name))+
  geom_point(aes(size = revenue)) + 
  geom_label(aes(label = label_text), hjust = "inward")

# Generate heatmaps 
# 
pct_sales_by_customer_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(bikeshop_name, category_1, category_2, quantity) %>% 
  group_by(bikeshop_name, category_1, category_2) %>% 
  summarise(total_qty = sum(quantity)) %>% 
  ungroup() %>% 
  group_by(bikeshop_name) %>% 
  mutate(pct = total_qty / sum(total_qty)) %>% 
  ungroup() %>% 
  mutate(bikeshop_name  = as.factor(bikeshop_name) %>% fct_rev()) %>% 
  mutate(bikeshop_name_num = as.numeric(bikeshop_name))

# Data visualization
pct_sales_by_customer_tbl %>% 
  ggplot(aes(category_2, bikeshop_name)) +
  geom_tile(aes(fill = pct)) + 
  # add text to blocks
  geom_text(aes(label = scales::percent(pct)), size = 3)+
  
  # formatting the style like color 
  scale_fill_gradient(low = "white", high = "black") + 
  facet_wrap(~ category_1) + 
  labs(
    title = "Heatmap of purchasing habits", 
    x     = "Bike type (category 2)", 
    y     = "Customer"
    
  )
#
pct_sales_by_customer_tbl %>% 
  
  ggplot(aes(category_2, bikeshop_name)) +
  
  # Geometries
  geom_tile(aes(fill = pct)) +
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.01L)), size = 3) +
  
  facet_wrap(~ category_1, scales = "free_x") +
  
  # formatting
  scale_fill_gradient(low = "white", high = palette_light()[1]) +
  
  labs(
    title = "Heatmaps of Purchasing Habits", 
    x     = "Bike Type (Category 2)", 
    y     = "Customer", 
    caption = str_glue(
      "Customers that prefer Road: Ann Arbor, Austin Cruisers, & Indianapolis
          
           Customers that prefer Mountain: Ithaca Mountain, Pittsburgh Mountain, & Tampa 29ers")
  ) + 
  
  theme_tq() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none", 
    plot.title = element_text(face = "bold"), 
    plot.caption = element_text(face = "bold.italic")
  )

# text manipulation using stringr
## str_detect()
bikes_tbl %>% 
  select(model) %>% 
  mutate(supersix = model %>% str_detect("Supersix") %>% as.numeric()) %>% 
  mutate(black    = model %>% str_detect("Black") %>% as.numeric())

## Concatenation: str_c()  
order_id <- 1
order_line <- 1
str_c("Order Line: ", order_id, ".", order_line)

## str_glue(): Concatenates text using quoted text 
## with brackets containing R code
## Example: str_glue("Some text {some_var}")
str_glue("Order Line: {order_id}.{order_line}")
## Example
bike_orderlines_tbl %>% 
  select(bikeshop_name, order_id, order_line) %>% 
  mutate(purchase_statement = str_glue(
    "Order Line: {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}"
  ) %>% as.character())

# Separating text
## tidyr::separate() and str_split()
bikes_tbl %>% select(description) %>% 
  separate(col = description, 
           into = c("category_1", "category_2", "frame_material"),
           sep  = " - ", 
           remove = FALSE)

# str_trim(), str_replace()

# Data function: feature engineering
# Goal: simplify the text feature engineering steps to convert model to funciton
## correct the "Utegra" to "Ultegra"
## correct the "CAAD Disc Ultegra" to "CAAD12 Disk Ultegra"
## correct the "Supersix Evo Hi-Mod Utegra" to "Supersix Evo Hi-Mod Ultegra" 

test <- bikes_tbl %>% select(model) %>% 
  # fix typo
  mutate(model = case_when(
    model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra", 
    model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
    model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra", 
    TRUE ~ model)
  ) %>% 
  # separating using spaces
  separate(col = model, 
           into = str_c("model_", 1:7), 
           sep = " ", 
           remove = FALSE, 
           fill = "right"
  ) %>% 
  mutate(model_base = case_when(
    # Fix Supersix Evo
    str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
    # Fix Beast of the East
    str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
    # Fix Bad Habit
    str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "), 
    # Fix Fat CAAD Bikes
    str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "), 
    # Fix Scalpel 29
    str_detect(str_to_lower(model_1), "29") ~ str_c(model_1, model_2, sep = " "),
    # catch all
    TRUE ~ model_1
  )
  ) %>% 
  
  # Get "tier" feature
  mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>% 
  # Remove unnecessary columns
  select(-matches("model_[0-9]")) %>% 
  # filter data with "black", "hi_mod", "team", "red", "ultegra", "dura_ace", "disc"
  # create flags using str_detect()
  mutate(black  = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
         red    = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
         hi_mod = model_tier %>% str_to_lower() %>% str_detect("hi_mod") %>% as.numeric(),
         team   = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
         ultegra = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
         dura_ace = model_tier %>% str_to_lower() %>% str_detect("dura_ace") %>% as.numeric(),
         disc    = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric())

# Create a function to simplify the process for use in the future
data <- bikes_tbl

separate_bike_model <- function(data, append = TRUE) {
  # Append
  if (!append){
    data <- data %>% select(model)
  }
  
  
  output_tbl <- data %>% select(model) %>% 
    # fix typo
    mutate(model = case_when(
      model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra", 
      model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
      model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra", 
      TRUE ~ model)
    ) %>% 
    # separating using spaces
    separate(col = model, 
             into = str_c("model_", 1:7), 
             sep = " ", 
             remove = FALSE, 
             fill = "right"
    ) %>% 
    mutate(model_base = case_when(
      # Fix Supersix Evo
      str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
      # Fix Beast of the East
      str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
      # Fix Bad Habit
      str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "), 
      # Fix Fat CAAD Bikes
      str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "), 
      # Fix Scalpel 29
      str_detect(str_to_lower(model_1), "29") ~ str_c(model_1, model_2, sep = " "),
      # catch all
      TRUE ~ model_1
    )
    ) %>% 
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>% 
    # Remove unnecessary columns
    select(-matches("model_[0-9]")) %>% 
    # filter data with "black", "hi_mod", "team", "red", "ultegra", "dura_ace", "disc"
    # create flags using str_detect()
    mutate(black  = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
           red    = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
           hi_mod = model_tier %>% str_to_lower() %>% str_detect("hi_mod") %>% as.numeric(),
           team   = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
           ultegra = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
           dura_ace = model_tier %>% str_to_lower() %>% str_detect("dura_ace") %>% as.numeric(),
           disc    = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric())
  
  return(output_tbl)
  
} 

separate_bike_model(data)
library(packrat)
library(rsconnect)
library(knitr)

server <- function(input, output, session) {

}

shinyApp(ui = ui, server = server)  # ← This MUST be the last line

install.packages("remotes")  # хэрвээ суулгагдаагүй бол
remotes::install_github("posit-dev/publisher")

# remotes package-ийг суулгах
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# devtools шаардлагатай бол суулгана
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Posit Publisher-г GitHub-аас суулгах
remotes::install_github("posit-dev/publisher")
