library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(shinyjs)
library(arules)
library(arulesViz)
library(stringr)
library(tidyr)
library(ggplot2)
library(forecast)
library(Rcpp)

#please modify this line to your own path if you want to run the app
setwd("/Users/engkahhui/Downloads/AdvancedR/project/RecipeExplorer")
sourceCpp("src/timeseries.cpp")
source("R/recipe_analyzer.R")

rec1pes <- readRDS("data/df_converted_rec2.rds")


rec1pes <- rec1pes %>%
  mutate(
    Region = case_when(
      # Italy
      grepl("italian|pasta|risotto|pizza|tiramisu",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "Italy",

      # France
      grepl("french|quiche|ratatouille|souffle|brie",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "France",

      # USA
      grepl("cajun|bbq|tex-mex|new england|californian",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "USA",

      # UK/Ireland
      grepl("scone|fish and chips|roast beef|irish stew",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "UK/Ireland",

      # Mediterranean
      grepl("hummus|falafel|tzatziki|gyro",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "Mediterranean",

      # Eastern Europe
      grepl("borscht|pierogi|goulash|bulgarian|meatball soup",
            paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
            ignore.case = TRUE) ~ "Eastern Europe",

      # Other Regions
      TRUE ~ "Other Regions"
    )
  )

generate_forecast <- function(data, months = 6) {
  tryCatch({
    # 1. Rcpp preprocess
    ts_data <- preprocess_dates(
      data$DatePublished[order(data$DatePublished)],
      months
    )

    # 2. Exception value process
    if(nrow(ts_data) > 10) {
      max_val <- quantile(ts_data$n, 0.99)
      ts_data$n[ts_data$n > max_val] <- max_val
    }

    # 3. ARIMA
    if(nrow(ts_data) >= 12) {
      ts_obj <- ts(ts_data$n, frequency = 12)
      fit <- auto.arima(ts_obj)
      forecast <- forecast(fit, h = months)
      return(list(data = ts_data, forecast = forecast))
    }

    return(NULL)
  }, error = function(e) {
    message("predict failed：", e$message)
    return(NULL)
  })
}



# Convert each recipe's ingredients list into transactions
list_of_ingredients <- strsplit(rec1pes$RecipeIngredientParts, ",\\s*")  # ingredients are comma-separated
transactions_recipes <- as(list_of_ingredients, "transactions")

# Convert recipe data into transactions
recipes_to_transactions <- function() {
  if (!exists("rec1pes")) {
    stop("rec1pes not found. Please ensure your recipe data is loaded.")
  }

  # Properly split ingredients into lists
  ingredient_lists <- strsplit(rec1pes$RecipeIngredientParts, ",\\s*")

  # Clean ingredients as a critical step!
  ingredient_lists <- lapply(ingredient_lists, function(x) {
    x <- trimws(x)          # Remove whitespace
    x <- x[x != ""]         # Remove empty items
    x <- x[!is.na(x)]       # Remove NA values
    unique(x)               # Remove duplicates
  })

  # Remove empty recipes
  ingredient_lists <- ingredient_lists[sapply(ingredient_lists, length) > 0]

  # Name the lists with RecipeId
  names(ingredient_lists) <- rec1pes$RecipeId[seq_along(ingredient_lists)]

  # Convert to transactions
  transactions <- as(ingredient_lists, "transactions")
  return(transactions)
}



ui <- page_sidebar(
  theme = bs_theme(version = 5, primary = "#3498db"),
  title = span(icon("utensils"), "Recipe Explorer"),
  fillable = TRUE,  # Makes the main content area fill available space
  fillable_mobile = TRUE,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Fix datatable name display */
      #filtered_table td {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 300px;
      }
      .analytics-container {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 15px;
        margin: 20px 0;
      }

      .text-muted {
        color: #7f8c8d;
        font-size: 0.9em;
        margin-top: 8px;
      }
      /* Ensure proper hover for clickable rows */
      #filtered_table tr.clickable-row:hover {
        background-color: #f5f5f5 !important;
      }

      /* Force name column to show full text on hover */
      #filtered_table td:first-child:hover {
        overflow: visible;
        white-space: normal;
        position: relative;
        z-index: 100;
        background: white;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }

      /* Remove all grid behavior */
      .full-width-container {
        flex: 0 0 100% !important;
        max-width: 100% !important;
        width: 100% !important;
        margin-left: 0 !important;
      }

      /* Override bslib grid classes */
      .bslib-grid-item, .g-col-sm-4, .g-col-sm-8 {
        flex: 0 0 100% !important;
        max-width: 100% !important;
      }

      /* Remove default padding */
      .html-fill-container {
        padding: 0 !important;
      }

      @media (min-width: 992px) {
      .main-content-fluid {
      margin-left: 300px; /* Matches sidebar width */
        }
      }

      @media (max-width: 991px) {
      .main-content-fluid {
      margin-left: 0;
        }
      }

        .clickable-name {
        color: #3498db;
        text-decoration: none;
        transition: all 0.2s ease;
      }

      .clickable-name:hover {
        text-decoration: underline;
        color: #2874a6;
      }

      #filtered_table tbody tr:hover {
        background-color: #f5f5f5 !important;
      }

      /* Make main content fluid */
      .main-content-fluid {
        width: 100%;
        max-width: none;
        padding: 20px;
        margin-left: 0;
      }

      /* Remove default padding from bslib */
      .bslib-page-sidebar > .main {
        padding-left: 0;
        padding-right: 0;
      }

      /* Keep your existing styles */
      .recipe-card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      /* Menu card styling */
      .btn-custom {
      border: 1px solid #ddd !important;
      margin-bottom: 10px;
      text-align: left;
      padding: 10px 15px;
      border-radius: 5px;
      transition: all 0.3s ease;
      }

      .btn-custom:hover {
      border-color: #3498db !important;
      background-color: #f8f9fa;
      }

      /* Recipe dashboard cards */
      .recipe-card {
      border: 1px solid #e0e0e0;
      border-radius: 8px;
      padding: 15px;
      margin: 10px 0;
      transition: all 0.3s ease;
      cursor: pointer;
      }

      .recipe-card:hover {
      border-color: #3498db;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }

      /* Recommended recipe cards */
      .recommended-recipe {
      border: 1px solid #e0e0e0;
      border-radius: 8px;
      padding: 15px;
      margin: 10px 0;
      transition: all 0.3s ease;
      cursor: pointer;
      }

      .recommended-recipe:hover {
      border-color: #3498db;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }

      #get-recipes-container {
       margin-bottom: 20px;
      padding-bottom: 20px;
      border-bottom: 1px solid #eee;
      }

      .clickable-recipe {
        cursor: pointer;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        transition: all 0.3s ease;
        position: relative;
        z-index: 1;
      }

      .clickable-recipe:hover {
        border-color: #3498db;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        transform: translateY(-2px);
      }

        .clickable-recipe-name {
        color: #3498db !important;
        cursor: pointer !important;
        text-decoration: none !important;
        transition: all 0.2s ease !important;
      }
      .clickable-recipe-name:hover {
        color: #2874a6 !important;
        text-decoration: underline !important;
      }
      #filtered_table tbody tr:hover {
        background-color: #f5f5f5 !important;
      }

      /* Dashboard specific styles */
      .dashboard-recipe {
        background: white;
        margin-bottom: 15px;
      }

      /* Recommendations specific styles */
      .recommended-recipe {
        background: #f8f9fa;
      }

    ")),
    tags$script(HTML("
      $(document).on('click', '.clickable-recipe', function() {
        var recipeId = $(this).data('recipeid');
        Shiny.setInputValue('recipe_clicked', recipeId, {priority: 'event'});
      });
    "))
  ),

  sidebar = sidebar(
    width = 250,
    # Keep all your original buttons
    actionButton("recipes_btn", "Browse Recipes", class = "btn-custom"),
    actionButton("calories_btn", "Calories Calculator", class = "btn-custom"),
    actionButton("show_filters", "Advanced Filters", class = "btn-custom")
  ),

  # Main content layout
  layout_columns(
    col_widths = c(4, 8),

    mainPanel(
      # Right main content
      class = "main-content-fluid",
      style = "padding: 20px;",
      # Recommendations section (always visible at top)
      conditionalPanel(
        condition = "output.show_recommendations",

        div(class = "recipe-card",
            h2("Recipe Recommendations"),
            textInput("ingredient_search", "What ingredients do you have?",
                      placeholder = "e.g., chicken, garlic"),
            div(id = "get-recipes-container",
                actionButton("find_recipes", "Get Recommendations", class = "btn-primary")
            ),
            uiOutput("recommendations")
        ),

        div(class = "analytics-container",
            div(class = "recipe-card",
                h4(icon("chart-line"), "Publication Trends & Forecast"),
                plotOutput("time_series_plot", height = 280),
                p(class = "text-muted", "Grey area shows 95% confidence interval")
            ),
            div(class = "recipe-card",
                h4(icon("map"), "Regional Cuisine Distribution"),
                plotOutput("region_plot", height = 280),
                p(class = "text-muted", "Top 10 regions by recipe count")
            )
        ),

        # Main content area (below recommendations)
        div(id = "main-content-container",
            uiOutput("main_content")
        )
      ),
      conditionalPanel(
        condition = "output.show_recipe_detail",
        div(class = "recipe-card",
            actionButton("back_btn", "Back to List",
                         class = "btn-custom",
                         style = "margin-bottom: 15px;"),
            uiOutput("recipe_detail")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Control which panel to show
  output$show_recommendations <- reactive({
    is.null(selected_recipe())
  })
  outputOptions(output, "show_recommendations", suspendWhenHidden = FALSE)

  output$show_recipe_detail <- reactive({
    !is.null(selected_recipe())
  })
  outputOptions(output, "show_recipe_detail", suspendWhenHidden = FALSE)

  # Render the detailed recipe view
  output$recipe_detail <- renderUI({
    req(selected_recipe())
    recipe <- selected_recipe()

    tagList(
      h3(recipe$Name),
      div(class = "recipe-meta",
          p(icon("star"), paste("Rating:", recipe$AggregatedRating)),
          p(icon("comments"), paste("Reviews:", recipe$ReviewCount)),
          p(icon("clock"), paste("Cook Time:", recipe$CookTime, "minutes")),
          p(icon("fire"), paste("Calories:", recipe$Calories, "kcal"))
      ),
      div(class = "recipe-description",
          h4("Description:"),
          p(recipe$Description),
          h4("Ingredients:"),
          p(recipe$RecipeIngredientParts),
          h4("Instructions:"),
          p(recipe$RecipeInstructions)
      )
    )
  })

  # Handle back button
  observeEvent(input$back_btn, {
    selected_recipe(NULL)
  })

  # Show modal when filters button clicked
  observeEvent(input$show_filters, {
    showModal(modalDialog(
      title = "Advanced Recipe Filters",
      numericInput("max_cal", "Max Calories", 500, min = 100, max = 2000),
      numericInput("min_rating", "Min Rating", 4, min = 1, max = 5, step = 0.5),
      numericInput("max_time", "Max Cook Time (mins)", 60, min = 5, max = 240),
      DTOutput("filtered_table"),
      footer = modalButton("Close"),
      size = "l"  # large size
    ))
  })

  min_supp <- 0.01
  min_conf <- 0.5
  min_lift <- 1

  rules_result <- reactiveVal(NULL)
  all_recipes <- reactiveVal(rec1pes)
  current_rules <- reactiveVal(NULL)
  user_ingredients <- reactiveVal(NULL)

  # Generate rules when app starts
  transactions <- recipes_to_transactions()
  rules <- apriori(transactions,
                   parameter = list(
                     supp = min_supp,
                     conf = min_conf,
                     minlen = 2
                   ))
  if(length(rules) > 0) {
    rules <- subset(rules, lift > min_lift)
  }
  current_rules(rules)

  observeEvent(input$table_recipe_clicked, {
    req(input$table_recipe_clicked)

    # Close the modal if it's open
    if (!is.null(input$show_filters)) {
      removeModal()
    }

    # Set the selected recipe
    selected_recipe(rec1pes[rec1pes$RecipeId == input$table_recipe_clicked, ])

    # Scroll to top
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })

  # code for dashboard
  selected_page <- reactiveVal("Recipes")
  selected_recipe <- reactiveVal(NULL)
  last_view <- reactiveVal(1)

  observeEvent(input$rules_btn, {
    selected_page("Rules")
    selected_recipe(NULL)
  })

  observeEvent(input$recipes_btn, {
    selected_page("Recipes")
    selected_recipe(NULL)
  })

  observeEvent(input$calories_btn, {
    selected_page("Calories Calculator")
    selected_recipe(NULL)
  })

  observeEvent(input$select, {
    last_view(input$select)
  })

  observeEvent(input$recipe_clicked, {
    req(input$recipe_clicked)
    selected_recipe(rec1pes[rec1pes$RecipeId == input$recipe_clicked, ])
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })


  find_related_recipes <- function(ingredients) {
    req(current_rules())
    rules_df <- as(current_rules(), "data.frame")

    # Find matching rules
    matched_rules <- rules_df %>%
      filter(grepl(paste(ingredients, collapse="|"), rules)) %>%
      arrange(desc(lift))

    if(nrow(matched_rules) == 0) return(NULL)

    # Extract top ingredients from rules
    top_ingredients <- matched_rules %>%
      head(3) %>%
      pull(rules) %>%
      str_extract_all("\\{([^}]+)\\}") %>%
      unlist() %>%
      gsub("\\{|\\}", "", .) %>%
      unique()

    # Remove the original search ingredients
    top_ingredients <- setdiff(top_ingredients, ingredients)

    # Find matching recipes
    all_recipes() %>%
      filter(grepl(paste(top_ingredients, collapse="|"), RecipeIngredientParts)) %>%
      arrange(desc(AggregatedRating), desc(ReviewCount)) %>%
      head(5)
  }

  # ADD THIS OUTPUT RENDER:
  output$recommendations <- renderUI({
    req(user_ingredients())
    recipes <- find_related_recipes(user_ingredients())

    if(is.null(recipes)) {
      return(div(class = "alert alert-info",
                 "No recommendations found. Try different ingredients!"))
    }

    tagList(
      h4(icon("lightbulb"),
         paste("Recommended because you searched for:",
               paste(user_ingredients(), collapse = ", "))),
      lapply(1:nrow(recipes), function(i) {
        div(class = "recipe-card clickable-recipe",
            `data-recipeid` = recipes$RecipeId[i],
            h5(recipes$Name[i]),
            p(icon("star"), paste("Rating:", recipes$AggregatedRating[i])),
            p(icon("clock"), paste("Cook Time:", recipes$CookTime[i], "mins")),
            p(icon("fire"), paste("Calories:", recipes$Calories[i], "kcal"))
        )
      })
    )
  })

  output$main_content <- renderUI({
    if (!is.null(selected_recipe())) {
      recipe <- selected_recipe()
      div(class = "recipe-card",
          actionButton("back_btn", "Back to List", class = "btn-custom", style = "margin-bottom: 15px;"),
          h3(recipe$Name),
          div(class = "recipe-meta",
              p(paste("⭐ Rating:", recipe$AggregatedRating)),
              p(paste("💬 Reviews:", recipe$ReviewCount)),
              p(paste("⏱️ Cook Time:", recipe$CookTime, "minutes")),
              p(paste("🔥 Calories:", recipe$Calories, "kcal"))
          ),
          div(class = "recipe-description",
              h4("Description:"),
              p(recipe$Description),
              h4("Instructions:"),
              p(recipe$RecipeInstructions)
          )
      )
    } else if (selected_page() == "Recipes") {
      tagList(
        h2("Discover Recipes"),
        p("Browse our collection of delicious recipes:"),
        selectInput(
          "select",
          "Sort by:",
          choices = list("Most Reviewed" = 1,
                         "Lowest Calories" = 2,
                         "Quickest to Make" = 3),
          selected = last_view()
        ),
        uiOutput("recipes_output")
      )
    } else if (selected_page() == "Calories Calculator") {
      div(class = "recipe-card",
          h2("Daily Calorie Needs Calculator"),
          numericInput("weight", "Your Weight (kg):", 70, min = 30, max = 200),
          numericInput("height", "Your Height (cm):", 170, min = 100, max = 250),
          numericInput("age", "Your Age:", 30, min = 10, max = 100),
          selectInput("activity", "Activity Level:",
                      choices = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")),
          radioButtons("gender", "Gender:", c("Male", "Female")),
          actionButton("calculate", "Calculate", class = "btn-custom"),
          br(), br(),
          uiOutput("calorie_result")
      )
    } else if (selected_page() == "Rules") {
      tagList(
        div(class = "recipe-card",
            h2("Recipe Recommendations"),
            textInput("ingredient_search", "What ingredients do you have?",
                      placeholder = "e.g., chicken, garlic"),
            actionButton("find_recipes", "Get Recommendations", class = "btn-primary"),
            uiOutput("recommendations")
        )
      )
    }
  })

  output$calorie_result <- renderUI({
    req(input$calculate)
    if (input$gender == "Male") {
      bmr <- 88.362 + (13.397 * input$weight) + (4.799 * input$height) - (5.677 * input$age)
    } else {
      bmr <- 447.593 + (9.247 * input$weight) + (3.098 * input$height) - (4.330 * input$age)
    }
    activity_factor <- switch(input$activity,
                              "Sedentary" = 1.2,
                              "Lightly Active" = 1.375,
                              "Moderately Active" = 1.55,
                              "Very Active" = 1.725)
    calories <- round(bmr * activity_factor)
    div(
      h3("Your Estimated Daily Calorie Needs:"),
      h2(paste(calories, "kcal per day")),
      p("This is an estimate based on the Harris-Benedict equation.")
    )
  })

  output$recipes_output <- renderUI({
    if (input$select == 1) {
      top_recipes <- rec1pes[order(-rec1pes$ReviewCount, -rec1pes$AggregatedRating), ][1:5, ]
      label_suffix <- paste("💬", top_recipes$ReviewCount, "reviews | ⭐", top_recipes$AggregatedRating)
    } else if (input$select == 2) {
      top_recipes <- rec1pes[order(rec1pes$Calories, -rec1pes$AggregatedRating), ][1:5, ]
      label_suffix <- paste("🔥", top_recipes$Calories, "kcal | ⭐", top_recipes$AggregatedRating)
    } else if (input$select == 3) {
      valid_recipes <- rec1pes[rec1pes$CookTime >= 5, ]
      top_recipes <- valid_recipes[order(valid_recipes$CookTime, -valid_recipes$AggregatedRating), ][1:5, ]
      label_suffix <- paste("⏱️", top_recipes$CookTime, "mins | ⭐", top_recipes$AggregatedRating)
    }

    div(
      lapply(seq_len(nrow(top_recipes)), function(i) {
        recipe <- top_recipes[i, ]
        div(class = "clickable-recipe recipe-card dashboard-recipe",
            `data-recipeid` = recipe$RecipeId,
            h4(recipe$Name),
            p(label_suffix[i])
        )
      })
    )
  })

  observe({
    lapply(seq_len(nrow(rec1pes)), function(i) {
      observeEvent(input[[paste0("recipe_", i)]], {
        if (input$select == 1) {
          selected_recipe(rec1pes[order(-rec1pes$ReviewCount, -rec1pes$AggregatedRating), ][i, ])
        } else if (input$select == 2) {
          selected_recipe(rec1pes[order(rec1pes$Calories, -rec1pes$AggregatedRating), ][i, ])
        } else if (input$select == 3) {
          valid_recipes <- rec1pes[rec1pes$CookTime >= 5, ]
          selected_recipe(valid_recipes[order(valid_recipes$CookTime, -valid_recipes$AggregatedRating), ][i, ])
        }
      })
    })
  })

  observeEvent(input$back_btn, {
    selected_recipe(NULL)
    updateSelectInput(session, "select", selected = last_view())
  })

  # Initialize analyzer
  analyzer <- RecipeAnalyzer$new(rec1pes)

  # Reactive filtered data
  filtered_data <- reactive({
    req(input$max_cal, input$min_rating, input$max_time)
    analyzer$get_healthy_recipes(input$max_cal, input$min_rating) %>%
      filter(CookTime <= input$max_time) %>%
      mutate(RecipeId = RecipeId)
  })

  # Then define the output that uses filtered_data
  output$filtered_table <- renderDT({
    # Ensure RecipeId is included in the data
    display_data <- filtered_data() %>%
      select(Name, Calories, AggregatedRating, CookTime, RecipeId)

    datatable(
      display_data,
      selection = 'none',
      class = 'row-hover',
      rownames = FALSE,
      escape = FALSE,  # Important for HTML rendering
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20),
        dom = 'ltp',
        columnDefs = list(
          list(
            targets = 0,  # Name column
            render = JS(
              "function(data, type, row, meta) {
              return '<a class=\"clickable-recipe-name\" data-id=\"' + row[4] + '\" style=\"color: #3498db; cursor: pointer;\">' + data + '</a>';
            }"
            )
          ),
          list(
            targets = 4,  # Hide RecipeId column
            visible = FALSE
          )
        ),
        initComplete = JS(
          "function(settings, json) {
          $('#filtered_table').on('click', '.clickable-recipe-name', function() {
            Shiny.setInputValue('table_recipe_clicked', $(this).data('id'), {priority: 'event'});
          });
        }"
        )
      )
    )
  })

  observeEvent(input$filtered_row_clicked, {
    req(input$filtered_row_clicked)
    id <- filtered_data()$RecipeId[input$filtered_row_clicked]
    selected_recipe(rec1pes[rec1pes$RecipeId == id, ])

    shinyjs::runjs("
      setTimeout(function() {
        const headerHeight = $('#advanced-features').outerHeight();
        const card = $('.recipe-card').first();
        if (card.length) {
          $('html, body').animate({
            scrollTop: card.offset().top - headerHeight - 20
          }, 300);
        }
      }, 100);
    ")
  })

  observeEvent(input$find_recipes, {
    ingredients <- trimws(strsplit(input$ingredient_search, ",")[[1]])
    user_ingredients(ingredients[ingredients != ""])  # Remove empty strings
  })


  output$time_series_plot <- renderPlot({
    req(rec1pes)

    forecast_data <- generate_forecast(rec1pes)
    if(is.null(forecast_data)) return(NULL)

    # create time index
    full_time <- seq_along(forecast_data$data$n)
    forecast_time <- (max(full_time)+1):(max(full_time)+6)

    # create data frame
    df <- data.frame(
      Time = c(full_time, forecast_time),
      Value = c(forecast_data$data$n, forecast_data$forecast$mean),
      Type = c(rep("Actual", length(full_time)),
               rep("Forecast", length(forecast_time)))
    )

    # prediction data
    ribbon_df <- data.frame(
      Time = forecast_time,
      ymin = forecast_data$forecast$lower[,2],
      ymax = forecast_data$forecast$upper[,2]
    )

    ggplot() +
      geom_line(data = df, aes(x = Time, y = Value, color = Type), linewidth = 1) +
      geom_ribbon(data = ribbon_df, aes(x = Time, ymin = ymin, ymax = ymax),
                  fill = "grey70", alpha = 0.3) +
      scale_color_manual(values = c("#3498db", "#e74c3c")) +
      labs(title = "Monthly Publications with 6-Month Forecast",
           x = "Time Period", y = "Recipes Published") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$region_plot <- renderPlot({
    region_counts <- rec1pes %>%
      count(Region) %>%
      filter(Region != "Other Regions") %>%
      top_n(10, n)

    ggplot(region_counts, aes(x = reorder(Region, n), y = n)) +
      geom_col(aes(fill = Region)) +
      #geom_text(aes(label = n), hjust = -0.2) +
      coord_flip() +
      labs(title = "Top Recipe Regions",
           x = "", y = "Number of Recipes") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
