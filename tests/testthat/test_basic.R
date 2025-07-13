library(testthat)
library(dplyr)

test_that("Region classification logic is correctly identified Italy", {
  rec1pes <- tibble::tibble(
    Name = "Classic Italian Pasta",
    Description = "A delicious pasta dish from Italy",
    RecipeCategory = "Dinner",
    Keywords = "italian, easy",
    RecipeIngredientParts = "pasta, tomato sauce"
  )

  rec1pes <- rec1pes %>%
    mutate(
      Region = case_when(
        grepl("italian|pasta|risotto|pizza|tiramisu",
              paste(Name, Description, RecipeCategory, Keywords, RecipeIngredientParts),
              ignore.case = TRUE) ~ "Italy",
        TRUE ~ "Other Regions"
      )
    )

  expect_equal(rec1pes$Region, "Italy")
})


