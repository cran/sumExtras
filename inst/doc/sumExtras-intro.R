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
# 
# # Apply the recommended JAMA theme
# use_jama_theme()

## ----setup2-------------------------------------------------------------------
library(sumExtras)
library(gtsummary)
library(dplyr)

# Apply the recommended JAMA theme
use_jama_theme()

## ----extras-comparison-standard, eval=FALSE-----------------------------------
# trial |>
#   tbl_summary(by = trt) |>
#   add_overall() |>
#   add_p() |>
#   bold_labels() |>
#   modify_header(label ~ "")

## ----extras-comparison-extras, eval=FALSE-------------------------------------
# trial |>
#   tbl_summary(by = trt) |>
#   extras()

## ----build-extras-comparison, echo=FALSE--------------------------------------
table_standard <- trial |>
  tbl_summary(by = trt) |>
  add_overall() |>
  add_p() |>
  bold_labels() |>
  modify_header(label ~ "")

table_extras <- trial |>
  tbl_summary(by = trt) |>
  extras()

## ----render-standard, echo=FALSE----------------------------------------------
table_standard

## ----render-extras, echo=FALSE------------------------------------------------
table_extras

## -----------------------------------------------------------------------------
# Table without p-values
trial |>
  tbl_summary(by = trt) |>
  extras(pval = FALSE)

# Table without overall column
trial |>
  tbl_summary(by = trt) |>
  extras(overall = FALSE)

# Overall column as last column (default is to set it as first)
trial |>
  tbl_summary(by = trt) |>
  extras(last = TRUE)

## -----------------------------------------------------------------------------
# Define standard table settings for a project
standard_table_args <- list(
  pval = TRUE,
  overall = TRUE,
  last = TRUE
)

# Apply consistently across multiple tables
trial |>
  select(age, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras(.args = standard_table_args)

## ----create-missing-data, echo=FALSE------------------------------------------
# Create data with some missing patterns
trial_missing <- trial |>
  mutate(
    age = if_else(trt == 'Drug B', NA_real_, age),
    marker = if_else(trt == 'Drug A', NA_real_, marker)
  )

## ----clean-comparison-without, eval=FALSE-------------------------------------
# trial_missing |>
#   tbl_summary(by = trt)

## ----clean-comparison-with, eval=FALSE----------------------------------------
# trial_missing |>
#   tbl_summary(by = trt) |>
#   clean_table()

## ----build-clean-comparison, echo=FALSE---------------------------------------
table_without_clean <- trial_missing |>
  tbl_summary(by = trt)

table_with_clean <- trial_missing |>
  tbl_summary(by = trt) |>
  clean_table()

## ----render-without-clean, echo=FALSE-----------------------------------------
table_without_clean

## ----render-with-clean, echo=FALSE--------------------------------------------
table_with_clean

## -----------------------------------------------------------------------------
trial_missing |>
  tbl_summary(by = trt) |>
  add_overall() |>
  add_p() |>
  clean_table()

## -----------------------------------------------------------------------------
# Create a simple dictionary
dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "trt",        "Chemotherapy Treatment",
  "age",        "Age at Enrollment (years)",
  "marker",     "Marker Level (ng/mL)",
  "stage",      "T Stage",
  "grade",      "Tumor Grade"
)

# Apply labels automatically
trial |>
  tbl_summary(by = trt, include = c(age, grade, marker)) |>
  add_auto_labels(dictionary = dictionary) |>
  extras()

## -----------------------------------------------------------------------------
# Apply JAMA compact theme (typically done once at the beginning)
use_jama_theme()

## -----------------------------------------------------------------------------
# 1. Define your dictionary (typically done once per project)
my_dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "trt",        "Chemotherapy Treatment",
  "age",        "Age at Enrollment (years)",
  "marker",     "Marker Level (ng/mL)",
  "stage",      "T Stage",
  "grade",      "Tumor Grade",
  "response",   "Tumor Response"
)

# 2. Set the recommended theme (once per session)
use_jama_theme()

# 3. Create a clean, labeled table with one function call
trial |>
  select(age, marker, stage, grade, response, trt) |>
  tbl_summary(
    by = trt,
    missing = "no"
  ) |>
  add_auto_labels(dictionary = my_dictionary) |>
  extras()

