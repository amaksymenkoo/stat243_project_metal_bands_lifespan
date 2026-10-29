
library(tidyverse)

setwd("C:/Users/Pochtarusa/Desktop/archive (4)")

# Load datasets
bands <- read_csv("metal_bands.csv", show_col_types = FALSE)
labels <- read_csv("labels_roster.csv", show_col_types = FALSE)

# Remove duplicate band-label combinations
labels <- labels %>%
  distinct(`Band ID`, `Label ID`, .keep_all = TRUE)

# Join label and band information
df <- labels %>%
  select(
    `Band ID`, `Label ID`,
    Specialization,
    Label_Status = Status,
    Label_Country = Country
  ) %>%
  inner_join(
    bands %>%
      select(
        `Band ID`, Genre,
        Band_Country = Country,
        Band_Status = Status
      ) %>%
      distinct(`Band ID`, .keep_all = TRUE),
    by = "Band ID"
  ) %>%
  filter(!is.na(Genre), !is.na(Band_Status))

# Group rare categories to reduce table size
group_rare <- function(x, min_count = 30) {
  x <- as.character(x)
  counts <- table(x)
  rare <- names(counts[counts < min_count])
  x[!is.na(x) & x %in% rare] <- "Other"
  factor(x)
}

df <- df %>%
  mutate(
    Specialization_group = group_rare(Specialization),
    Label_Country_group = group_rare(Label_Country),
    Band_Country_group = group_rare(Band_Country),
    Genre_group = group_rare(Genre)
  )

# Test association between two categorical variables
test_table <- function(data, var1, var2, B = 999) {
  
  data <- data %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  
  tab <- table(data[[var1]], data[[var2]])
  
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(tibble(
      variable_1 = var1,
      variable_2 = var2,
      n = sum(tab),
      method = NA_character_,
      p_value = NA_real_,
      cramers_v = NA_real_
    ))
  }
  
  # Calculate expected counts
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  expected <- chi$expected
  
  # Select the appropriate statistical test
  if (all(dim(tab) == c(2, 2)) && any(expected < 5)) {
    test <- fisher.test(tab)
    method <- "Fisher's exact test"
  } else if (any(expected < 1) || mean(expected < 5) > 0.20) {
    test <- chisq.test(tab, simulate.p.value = TRUE, B = B)
    method <- "Chi-square (Monte Carlo)"
  } else {
    test <- chi
    method <- "Chi-square"
  }
  
  # Calculate Cramer's V
  n <- sum(tab)
  v <- sqrt(
    as.numeric(chi$statistic) /
      (n * min(nrow(tab) - 1, ncol(tab) - 1))
  )
  
  tibble(
    variable_1 = var1,
    variable_2 = var2,
    n = n,
    method = method,
    p_value = test$p.value,
    cramers_v = v
  )
}

# Test 1: label specialization and band genre
test_specialization_genre <- test_table(
  df, "Specialization_group", "Genre_group"
)

# Test 2: label country and band country
test_label_country <- test_table(
  df, "Label_Country_group", "Band_Country_group"
)

# Test 3: label status and band status
test_label_band_status <- test_table(
  df, "Label_Status", "Band_Status"
)

# Test 4: band genre and band status
test_genre_status <- test_table(
  df, "Genre_group", "Band_Status"
)

# Combine results and adjust p-values
results <- bind_rows(
  test_specialization_genre,
  test_label_country,
  test_label_band_status,
  test_genre_status
) %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"))

# Display and save results of tests 1-4
print(results)
write_csv(results, "labels_test_results.csv")

# Save contingency tables for tests 1-4
write.csv(
  as.data.frame.matrix(
    table(df$Specialization_group, df$Genre_group)
  ),
  "labels_specialization_genre.csv"
)

write.csv(
  as.data.frame.matrix(
    table(df$Label_Country_group, df$Band_Country_group)
  ),
  "labels_country_comparison.csv"
)

write.csv(
  as.data.frame.matrix(
    table(df$Label_Status, df$Band_Status)
  ),
  "labels_status_comparison.csv"
)

write.csv(
  as.data.frame.matrix(
    table(df$Genre_group, df$Band_Status)
  ),
  "genre_band_status.csv"
)

# Test 5: label status and band status separately by genre
stratified_results <- df %>%
  group_by(Genre_group) %>%
  group_modify(
    ~ test_table(.x, "Label_Status", "Band_Status", B = 199)
  ) %>%
  ungroup() %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH"))

# Display and save results of test 5
print(stratified_results)
write_csv(stratified_results, "labels_status_by_genre.csv")


# Print interpretation of the test results
interpret_results <- results %>%
  mutate(
    conclusion = case_when(
      p_adjusted >= 0.05 ~ "No statistically significant association",
      cramers_v < 0.10 ~ "Very weak association",
      cramers_v < 0.30 ~ "Weak association",
      cramers_v < 0.50 ~ "Moderate association",
      TRUE ~ "Strong association"
    )
  )

for (i in seq_len(nrow(interpret_results))) {
  cat("\nVariables:", interpret_results$variable_1[i],
      "and", interpret_results$variable_2[i], "\n")
  cat("P-value:", interpret_results$p_value[i], "\n")
  cat("Adjusted p-value:", interpret_results$p_adjusted[i], "\n")
  cat("Cramer's V:", round(interpret_results$cramers_v[i], 3), "\n")
  cat("Conclusion:", interpret_results$conclusion[i], "\n")
}
