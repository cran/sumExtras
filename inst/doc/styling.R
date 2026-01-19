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

# Apply the recommended JAMA theme
use_jama_theme()

## ----basic-groups-example-----------------------------------------------------
trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Clinical Measures",
    variables = marker:response
  )

## ----group-styling-without, eval=FALSE----------------------------------------
# trial |>
#   select(age, marker, grade, stage, response, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Demographics",
#     variables = age
#   ) |>
#   add_variable_group_header(
#     header = "Clinical Measures",
#     variables = marker:response
#   )

## ----group-styling-with, eval=FALSE-------------------------------------------
# trial |>
#   select(age, marker, grade, stage, response, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Demographics",
#     variables = age
#   ) |>
#   add_variable_group_header(
#     header = "Clinical Measures",
#     variables = marker:response
#   ) |>
#   add_group_styling()

## ----build-group-styling, echo=FALSE------------------------------------------
table_without_group_styling <- trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Clinical Measures",
    variables = marker:response
  )

table_with_group_styling <- trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Clinical Measures",
    variables = marker:response
  ) |>
  add_group_styling()

## ----render-without-group-styling, echo=FALSE---------------------------------
table_without_group_styling

## ----render-with-group-styling, echo=FALSE------------------------------------
table_with_group_styling

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Characteristics",
    variables = age:stage
  ) |>
  add_group_styling(format = "bold")

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Characteristics",
    variables = age:stage
  ) |>
  add_group_styling(format = "italic")

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Characteristics",
    variables = age:stage
  ) |>
  add_group_styling(format = c("bold", "italic"))

## ----indentation-without, eval=FALSE------------------------------------------
# trial |>
#   select(age, grade, stage, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Patient Variables",
#     variables = age:stage
#   )

## ----indentation-with, eval=FALSE---------------------------------------------
# trial |>
#   select(age, grade, stage, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Patient Variables",
#     variables = age:stage
#   ) |>
#   add_group_styling()

## ----build-indentation, echo=FALSE--------------------------------------------
table_without_indent <- trial |>
  select(age, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Variables",
    variables = age:stage
  )

table_with_indent <- trial |>
  select(age, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Variables",
    variables = age:stage
  ) |>
  add_group_styling()

## ----render-without-indent, echo=FALSE----------------------------------------
table_without_indent

## ----render-with-indent, echo=FALSE-------------------------------------------
table_with_indent

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Clinical Measures",
    variables = marker:response
  ) |>
  add_group_styling() |> 
  add_group_colors()  # Default light gray background

## -----------------------------------------------------------------------------
# Create a table with groups
grouped_table <- trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Disease Measures",
    variables = marker:stage
  )

# Get row numbers
group_rows <- get_group_rows(grouped_table)
group_rows

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Baseline Characteristics",
    variables = age:stage
  ) |>
  add_group_styling() |> 
  add_group_colors(color = "#E3F2FD")  # Light blue

## ----colors-manual, eval=FALSE------------------------------------------------
# my_table <- trial |>
#   select(age, marker, grade, stage, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Baseline Characteristics",
#     variables = age:stage
#   ) |>
#   add_group_styling()
# 
# group_rows <- get_group_rows(my_table)
# 
# my_table |>
#   as_gt() |>
#   gt::tab_style(
#     style = gt::cell_fill(color = "#E3F2FD"),
#     locations = gt::cells_body(rows = group_rows)
#   )

## ----colors-convenience, eval=FALSE-------------------------------------------
# trial |>
#   select(age, marker, grade, stage, trt) |>
#   tbl_summary(by = trt) |>
#   extras() |>
#   add_variable_group_header(
#     header = "Baseline Characteristics",
#     variables = age:stage
#   ) |>
#   add_group_styling() |>
#   add_group_colors(color = "#E3F2FD")

## ----build-colors-comparison, echo=FALSE--------------------------------------
table_manual <- trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Baseline Characteristics",
    variables = age:stage
  ) |>
  add_group_styling()

group_rows_manual <- get_group_rows(table_manual)

table_manual <- table_manual |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_fill(color = "#E3F2FD"),
    locations = gt::cells_body(rows = group_rows_manual)
  )

table_convenience <- trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Baseline Characteristics",
    variables = age:stage
  ) |>
  add_group_styling() |> 
  add_group_colors(color = "#E3F2FD")

## ----render-manual, echo=FALSE------------------------------------------------
table_manual

## ----render-convenience, echo=FALSE-------------------------------------------
table_convenience

## -----------------------------------------------------------------------------
trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt, missing = "no") |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "Tumor Characteristics",
    variables = grade:response
  ) |>
  add_group_styling(format = "bold")  # Bold only, no italic

## -----------------------------------------------------------------------------
my_table <- trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt, missing = "no") |>
  extras() |>
  add_variable_group_header(
    header = "DEMOGRAPHICS",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "TUMOR CHARACTERISTICS",
    variables = grade:response
  ) |>
  add_group_styling()  # Bold + italic

group_rows <- get_group_rows(my_table)

my_table |>
  as_gt() |>
  gt::tab_style(
    style = list(
      gt::cell_fill(color = "#E8E8E8"),
      gt::cell_text(weight = "bold")
    ),
    locations = gt::cells_body(rows = group_rows)
  )

## -----------------------------------------------------------------------------
# Create a more complex grouping
my_table <- trial |>
  select(trt, age, marker, grade, stage, response, death) |>
  tbl_summary(by = trt, missing = "no") |>
  extras() |>
  add_variable_group_header(
    header = "Baseline Demographics",
    variables = age
  ) |>
  add_variable_group_header(
    header = "Biomarkers",
    variables = marker
  ) |>
  add_variable_group_header(
    header = "Disease Characteristics",
    variables = grade:stage
  ) |>
  add_variable_group_header(
    header = "Outcomes",
    variables = response:death
  ) |>
  add_group_styling()

group_rows <- get_group_rows(my_table)

my_table |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_fill(color = "#F5F5F5"),
    locations = gt::cells_body(rows = group_rows)
  )

## -----------------------------------------------------------------------------
# Apply JAMA compact theme (typically done once at the beginning)
use_jama_theme()

# Now all gtsummary tables use this theme
trial |>
  tbl_summary(by = trt) |>
  extras()

## ----eval=FALSE---------------------------------------------------------------
# gtsummary::set_gtsummary_theme(gtsummary::theme_gtsummary_compact("jama"))

## ----eval=FALSE---------------------------------------------------------------
# gtsummary::reset_gtsummary_theme()

## ----gt-theme-without, eval=FALSE---------------------------------------------
# trial |>
#   select(trt, age, grade) |>
#   head(10) |>
#   gt::gt()

## ----gt-theme-with, eval=FALSE------------------------------------------------
# trial |>
#   select(trt, age, grade) |>
#   head(10) |>
#   gt::gt() |>
#   theme_gt_compact()

## ----build-gt-theme, echo=FALSE-----------------------------------------------
table_gt_without <- trial |>
  select(trt, age, grade) |>
  head(10) |>
  gt::gt()

table_gt_with <- trial |>
  select(trt, age, grade) |>
  head(10) |>
  gt::gt() |>
  theme_gt_compact()

## ----render-gt-without, echo=FALSE--------------------------------------------
table_gt_without

## ----render-gt-with, echo=FALSE-----------------------------------------------
table_gt_with

## -----------------------------------------------------------------------------
trial |>
  select(trt, age, grade, marker) |>
  head(8) |>
  gt::gt() |>
  theme_gt_compact() |>
  gt::tab_header(
    title = "Trial Patient Sample",
    subtitle = "First 8 patients"
  ) |>
  gt::tab_style(
    style = gt::cell_fill(color = "#F0F0F0"),
    locations = gt::cells_body(rows = seq(2, 8, 2))  # Zebra striping
  )

## -----------------------------------------------------------------------------
my_table <- trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Demographics",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "Disease",
    variables = grade:stage
  ) |>
  add_group_styling()

group_rows <- get_group_rows(my_table)

# Convert to gt and get the actual number of body rows
gt_table <- my_table |> as_gt()
n_body_rows <- nrow(gt_table$`_data`)

gt_table |>
  gt::tab_style(
    style = gt::cell_fill(color = "#E8E8E8"),
    locations = gt::cells_body(rows = group_rows)
  ) |>
  gt::tab_style(
    style = gt::cell_fill(color = "#F9F9F9"),
    locations = gt::cells_body(rows = !seq_len(n_body_rows) %in% group_rows)
  )

## -----------------------------------------------------------------------------
my_table <- trial |>
  select(age, marker, grade, stage, response, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Baseline",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "Disease & Outcome",
    variables = grade:response
  ) |>
  add_group_styling()

group_rows <- get_group_rows(my_table)

my_table |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_fill(color = "#E8E8E8"),
    locations = gt::cells_body(rows = group_rows)
  ) |>
  gt::tab_style(
    style = list(
      gt::cell_fill(color = "#FFF9E6"),
      gt::cell_text(weight = "bold")
    ),
    locations = gt::cells_body(
      columns = label,
      rows = label == "Tumor Response"  # Highlight primary outcome
    )
  )

## -----------------------------------------------------------------------------
my_table <- trial |>
  select(age, marker, grade, stage, trt) |>
  tbl_summary(by = trt) |>
  extras() |>
  add_variable_group_header(
    header = "Patient Factors",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "Tumor Factors",
    variables = grade:stage
  ) |>
  add_group_styling()

group_rows <- get_group_rows(my_table)

my_table |>
  as_gt() |>
  gt::tab_style(
    style = gt::cell_fill(color = "#F5F5F5"),
    locations = gt::cells_body(rows = group_rows)
  ) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = "bottom",
      color = "#CCCCCC",
      weight = gt::px(2)
    ),
    locations = gt::cells_body(rows = max(group_rows))
  )

## -----------------------------------------------------------------------------
# Create dictionary for labeling (see vignette("labeling") for details)
dictionary <- tibble::tribble(
  ~Variable,    ~Description,
  "trt",        "Treatment Assignment",
  "age",        "Age at Baseline (years)",
  "marker",     "Biomarker Level (ng/mL)",
  "stage",      "Clinical Stage",
  "grade",      "Tumor Grade",
  "response",   "Treatment Response",
  "death",      "Patient Died"
)

# Build the styled table
trial |>
  select(trt, age, marker, grade, stage, response, death) |>
  tbl_summary(by = trt, missing = "no") |>
  add_auto_labels(dictionary = dictionary) |>
  extras() |>
  add_variable_group_header(
    header = "BASELINE CHARACTERISTICS",
    variables = age:marker
  ) |>
  add_variable_group_header(
    header = "DISEASE CHARACTERISTICS",
    variables = grade:stage
  ) |>
  add_variable_group_header(
    header = "OUTCOMES",
    variables = response:death
  ) |>
  add_group_styling() |> 
  add_group_colors(color = "#E8E8E8") |> 
  gt::tab_header(
    title = "Patient Characteristics and Outcomes by Treatment",
    subtitle = "Clinical Trial Dataset"
  ) |>
  gt::tab_source_note(
    source_note = "Data from gtsummary package trial dataset"
  )

