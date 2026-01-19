## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Set gtsummary print engine for proper rendering
options(gtsummary.print_engine = "gt")

## ----setup--------------------------------------------------------------------
# library(sumExtras)
# library(gtsummary)
# library(dplyr)
# library(gt)
# 
# # Apply the recommended JAMA theme
# use_jama_theme()

## ----setup2-------------------------------------------------------------------
library(sumExtras)
library(gtsummary)
library(dplyr)
library(gt)
library(ggplot2)

# Apply the recommended JAMA theme
use_jama_theme()

## -----------------------------------------------------------------------------
# Create a simple dataset
trial_example <- trial

# Set a label attribute on a variable
attr(trial_example$age, "label") <- "Age at Enrollment (years)"

# Check the label
attr(trial_example$age, "label")

## -----------------------------------------------------------------------------
# Create a dictionary for the trial dataset
dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "trt",        "Chemotherapy Treatment",
  "age",        "Age at Enrollment (years)",
  "marker",     "Marker Level (ng/mL)",
  "stage",      "T Stage",
  "grade",      "Tumor Grade",
  "response",   "Tumor Response",
  "death",      "Patient Died"
)

dictionary

## ----eval=FALSE---------------------------------------------------------------
# # Typically at the top of your analysis script
# dictionary <- readr::read_csv("data/variable_dictionary.csv")

## -----------------------------------------------------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade, marker)) |>
  add_auto_labels(dictionary = dictionary) |>
  extras()

## -----------------------------------------------------------------------------
# Dictionary is already in environment from above
trial |>
  tbl_summary(by = trt, include = c(age, stage, response)) |>
  add_auto_labels() |>  # Finds dictionary automatically
  extras()

## -----------------------------------------------------------------------------
# Create data with label attributes
labeled_trial <- trial
attr(labeled_trial$age, "label") <- "Patient Age at Baseline"
attr(labeled_trial$marker, "label") <- "Biomarker Concentration (ng/mL)"

# Use attributes for labeling (no dictionary needed)
labeled_trial |>
  tbl_summary(by = trt, include = c(age, marker)) |>
  add_auto_labels()  # Reads from label attributes

## -----------------------------------------------------------------------------
trial |>
  tbl_summary(
    by = trt,
    include = c(age, grade, marker),
    label = list(age ~ "Age (Custom Label)")  # This overrides dictionary/attributes
  ) |>
  add_auto_labels(dictionary = dictionary) |>
  extras()

## -----------------------------------------------------------------------------
lm(marker ~ age + grade + stage, data = trial) |>
  tbl_regression() |>
  add_auto_labels(dictionary = dictionary)

## -----------------------------------------------------------------------------
# Apply labels to data as attributes
trial_labeled <- trial |>
  apply_labels_from_dictionary(dictionary = dictionary)

# Check that labels were set
attr(trial_labeled$age, "label")
attr(trial_labeled$marker, "label")

## -----------------------------------------------------------------------------
# Labels are automatically recognized
trial_labeled |>
  tbl_summary(by = trt, include = c(age, marker, grade)) |>
  add_auto_labels() |>  # Reads attributes automatically
  extras()

## ----fig.width=7, fig.height=4------------------------------------------------
# Labels appear automatically on axes and legend!
trial_labeled |>
  ggplot(aes(x = age, y = marker, color = trt)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

## -----------------------------------------------------------------------------
# Create data with both sources of labels
trial_both <- trial
attr(trial_both$age, "label") <- "Age from Attribute"

# Also have dictionary (already defined above)
dictionary_conflict <- tibble::tribble(
  ~Variable, ~Description,
  "age", "Age from Dictionary"
)

# Default: attribute wins
trial_both |>
  tbl_summary(by = trt, include = age) |>
  add_auto_labels(dictionary = dictionary_conflict) |>
  extras()
# Shows: "Age from Attribute"

## -----------------------------------------------------------------------------
# Prioritize dictionary over attributes
options(sumExtras.preferDictionary = TRUE)

trial_both |>
  tbl_summary(by = trt, include = age) |>
  add_auto_labels(dictionary = dictionary_conflict) |>
  extras()
# Shows: "Age from Dictionary"

# Reset to default for rest of vignette
options(sumExtras.preferDictionary = FALSE)

## ----fig.width=7, fig.height=5------------------------------------------------
# 1. Define dictionary once
my_dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "age",        "Age at Enrollment (years)",
  "marker",     "Marker Level (ng/mL)",
  "trt",        "Treatment Group",
  "grade",      "Tumor Grade",
  "stage",      "T Stage"
)

# 2. Apply to data
trial_final <- trial |>
  apply_labels_from_dictionary(my_dictionary)

# 3. Create gtsummary table
trial_final |>
  tbl_summary(
    by = trt,
    include = c(age, marker, grade, stage)
  ) |>
  add_auto_labels() |>
  extras()

# 4. Create ggplot2 visualization with same labels
trial_final |>
  filter(!is.na(marker)) |>
  ggplot(aes(x = age, y = marker)) +
  geom_point(aes(color = grade), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~trt) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----fig.width=8, fig.height=6------------------------------------------------
# Step 1: Define your master dictionary
# In practice, this would be loaded from a CSV file
study_dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "trt",        "Treatment Assignment",
  "age",        "Age at Baseline (years)",
  "marker",     "Biomarker Level (ng/mL)",
  "stage",      "Clinical Stage",
  "grade",      "Tumor Grade",
  "response",   "Treatment Response",
  "death",      "Patient Died"
)

# Step 2: Apply labels to your data once
trial_study <- trial |>
  apply_labels_from_dictionary(study_dictionary)

# Step 3: Create multiple tables using the same labels

# Table 1: Overall summary
trial_study |>
  tbl_summary(include = c(age, marker, stage, grade)) |>
  add_auto_labels() |>
  extras(overall = TRUE, pval = FALSE)

# Table 2: By treatment comparison
trial_study |>
  tbl_summary(
    by = trt,
    include = c(age, marker, response)
  ) |>
  add_auto_labels() |>
  extras()

# Table 3: Regression analysis
lm(marker ~ age + grade + stage, data = trial_study) |>
  tbl_regression() |>
  add_auto_labels()

# Step 4: Create plots using the same labels

# Plot 1: Age distribution by treatment
trial_study |>
  ggplot(aes(x = trt, y = age, fill = trt)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Marker vs age relationship
trial_study |>
  filter(!is.na(marker)) |>
  ggplot(aes(x = age, y = marker, color = trt)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal()

# Plot 3: Response rates by grade and treatment
trial_study |>
  filter(!is.na(response)) |>
  count(grade, trt, response) |>
  group_by(grade, trt) |>
  mutate(prop = n / sum(n)) |>
  filter(response == 1) |>
  ggplot(aes(x = grade, y = prop, fill = trt)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Response Rate") +
  theme_minimal()

## -----------------------------------------------------------------------------
# Create a subset
trial_subset <- trial_labeled |>
  filter(stage %in% c("T1", "T2")) |>
  select(age, marker, stage, trt)

# Labels are still there
trial_subset |>
  tbl_summary(by = trt) |>
  add_auto_labels() |>
  extras()

## -----------------------------------------------------------------------------
# Labels persist through mutations
trial_labeled |>
  mutate(
    age_group = cut(age, breaks = c(0, 50, 70, 100),
                    labels = c("<50", "50-70", ">70"))
  ) |>
  select(age, age_group, marker, trt) |>
  tbl_summary(by = trt, include = c(age, marker)) |>
  add_auto_labels() |>
  extras()

## -----------------------------------------------------------------------------
# Demographics dictionary
demographics_dict <- tibble::tribble(
  ~Variable, ~Description,
  "age",     "Age at Enrollment (years)",
  "sex",     "Biological Sex"
)

# Clinical dictionary
clinical_dict <- tibble::tribble(
  ~Variable,  ~Description,
  "marker",   "Marker Level (ng/mL)",
  "stage",    "T Stage",
  "grade",    "Tumor Grade"
)

# Combine for use
combined_dict <- bind_rows(demographics_dict, clinical_dict)

trial |>
  tbl_summary(include = c(age, marker, grade)) |>
  add_auto_labels(dictionary = combined_dict) |>
  extras()

## -----------------------------------------------------------------------------
# Check for label attributes
str(trial_labeled$age)

