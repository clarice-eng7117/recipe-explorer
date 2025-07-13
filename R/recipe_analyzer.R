RecipeAnalyzer <- R6::R6Class(
  "RecipeAnalyzer",
  public = list(
    recipes = NULL,

    #' Initialize RecipeAnalyzer with recipe data
    #'
    #' @param data A data frame containing recipe data
    initialize = function(data) {
      self$recipes <- data
    },

    #' Get healthy recipes based on nutritional criteria
    #'
    #' @param max_calories Maximum calorie threshold (default: 500)
    #' @param min_rating Minimum rating threshold (default: 4)
    #' @return A filtered and sorted data frame of healthy recipes
    #' @details Returns recipes with calories below threshold and rating above threshold,
    #'          sorted by calories (ascending) and rating (descending)
    get_healthy_recipes = function(max_calories = 500, min_rating = 4) {
      self$recipes %>%
        filter(
          Calories <= max_calories,
          AggregatedRating >= min_rating,
          CookTime > 0
        ) %>%
        arrange(Calories, desc(AggregatedRating)) %>%
        select(RecipeId, Name, Calories, AggregatedRating, CookTime)  # Must include RecipeId
    },

    #' Get quick recipes based on preparation time
    #'
    #' @param max_time Maximum preparation time in minutes (default: 30)
    #' @return A filtered and sorted data frame of quick recipes
    #' @details Returns recipes with total time below threshold,
    #'          sorted by preparation time (ascending) and rating (descending)
    get_quick_recipes = function(max_time = 30) {
      self$recipes %>%
        dplyr::filter(
          TotalTime <= max_time,
          !is.na(TotalTime)
        ) %>%
        dplyr::arrange(TotalTime, desc(AggregatedRating)) %>%
        dplyr::select(Name, TotalTime, AggregatedRating, RecipeCategory)
    },

    #' Calculate nutritional statistics for all recipes
    #'
    #' @return A named list containing nutritional statistics
    #' @details Returns average calories, protein, rating, and percentage of healthy recipes
    get_nutrition_stats = function() {
      stats <- self$recipes %>%
        dplyr::summarise(
          avg_calories = mean(Calories, na.rm = TRUE),
          avg_protein = mean(ProteinContent, na.rm = TRUE),
          avg_rating = mean(AggregatedRating, na.rm = TRUE),
          healthy_ratio = sum(Calories < 500, na.rm = TRUE) / dplyr::n()
        )

      list(
        "Average Calories" = round(stats$avg_calories, 1),
        "Average Protein (g)" = round(stats$avg_protein, 1),
        "Average Rating" = round(stats$avg_rating, 1),
        "Healthy Recipes %" = round(stats$healthy_ratio * 100, 1)
      )
    },

    #' Get popular recipes based on review count
    #'
    #' @param min_reviews Minimum number of reviews threshold (default: 10)
    #' @return A filtered and sorted data frame of popular recipes
    #' @details Returns recipes with review count above threshold,
    #'          sorted by review count (descending) and rating (descending)
    get_popular_recipes = function(min_reviews = 10) {
      self$recipes %>%
        dplyr::filter(
          ReviewCount >= min_reviews,
          !is.na(ReviewCount)
        ) %>%
        dplyr::arrange(desc(ReviewCount), desc(AggregatedRating)) %>%
        dplyr::select(Name, ReviewCount, AggregatedRating, RecipeCategory)
    }
  )
)
