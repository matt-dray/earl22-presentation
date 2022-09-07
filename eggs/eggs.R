# Extract and prepare egg statistics
# Matt Dray, Aug 2022

# This script reads in data from an ODS published on GOV.UK, rearranges it and
# outputs some objects to be passed as arguments to create_a11ytables() in the 
# presentation file (../index.qmd).


# Attach packages ---------------------------------------------------------

suppressPackageStartupMessages({
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(a11ytables)
})


# Extract data ------------------------------------------------------------


# File eggs-packers-28jul22.ods downloaded from 
#   https://www.gov.uk/government/statistics/egg-statistics

table_content <- c("country", "system", "prices")
table_names <- c(paste0("qtrly_", table_content), paste0("yrly_", table_content))

# Features to map over so we can ingest each individual subtable's cells
args_df <- data.frame(
  path = "eggs/eggs-packers-28jul22.ods",
  sheet = c(rep("Packers", 3), rep("Packers_Annual", 3)), 
  range = c(
    "D9:G115", "H9:K115", "M9:Q115",  # quarterly
    "C9:F35",  "G9:J35",  "L9:P35"    # annually
  )
)

# Read in all the subtables
data_in <- pmap(args_df, readODS::read_ods) |> set_names(table_names)

# Full years (quarterly data does go to 2022 Q2)
year_range <- 1996:2021

# Add columns for year and quarter (quarterly data)
qtrly_mutated <- map(
  data_in[grep("^qtrly", names(data_in))],
  ~mutate(
    .x,
    Year = c(sort(rep(year_range, 4)), rep(2022, 2)),
    Quarter = c(rep(paste0("Q", 1:4), length(year_range)), "Q1", "Q2"),
    .before = 1
  )
)

# Add columns for year only (annual data)
yrly_mutated <- map(
  data_in[grep("^yrly", names(data_in))],
  ~mutate(.x, Year = year_range, .before = 1)
)

# Combine quarterly and annual back into single list, update shorthand
tables_wide <- c(qtrly_mutated, yrly_mutated) |>
  map(
    ~mutate(
      .x, 
      across(everything(), as.character),
      across(everything(), ~if_else(str_detect(., "^c$"), "[c]", .)),
      across(everything(), ~if_else(is.na(.), "[z]", .))
    )
  )

# Function that pivots tables to long format and cleans up decimal places
lengthen_tables <- function(dat, cols_ignore, type, stat_col) {
  
  # Subfunction that rounds numbers in character columns to 1dp
  correct_dp <- function(dat, col) {
    
    dat[[col]] <- ifelse(       
      !dat[[col]] %in% c("[c]", "[z]"), 
      as.character(round(as.numeric(dat[[col]]), 1)), 
      dat[[col]]
    )
    
    dat
    
  }
  
  dat |> 
    pivot_longer(
      -all_of(cols_ignore),
      names_to = type,
      values_to = stat_col
    ) |> 
    correct_dp(col = stat_col)
  
}

# Prepare datframe of features to be mapped over for each subtable (includes
# notes in column headers as required)
pivot_features <- tribble(
  ~dat, ~cols_ignore, ~type, ~stat_col,
  tables_wide$qtrly_country, c("Year", "Quarter"), "Country", "Eggs (million dozen)",
  tables_wide$qtrly_system, c("Year", "Quarter"), "System [note 1]", "Eggs (million dozen)",
  tables_wide$qtrly_prices, c("Year", "Quarter"), "System  [note 1]", "Eggs (million dozen)",
  tables_wide$yrly_country, "Year", "Country [note 2]", "Average packer to producer prices (pence per dozen) [note 3] [note 4]",
  tables_wide$yrly_system, "Year", "System [note 1]", "Average packer to producer prices (pence per dozen) [note 3] [note 4]",
  tables_wide$yrly_prices, "Year", "System [note 1]", "Average packer to producer prices (pence per dozen) [note 3] [note 4]"
)

# Map over each set of features to pivot longer and correct numeric rounding
tables_list <- suppressWarnings(pmap(pivot_features, lengthen_tables)) |> 
  map(as.data.frame) |>  # convert from tibbles for demo purposes
  set_names(table_names)


# Prepare tables for meta sheets ------------------------------------------


# Table of cover information (will be formatted into a single column, with rows
# alternating between subsection titles and subsection bodies)
cover_df <- tribble(
  ~"subsection_title", ~"subsection_body",
  "Purpose",           "This publication gives quarterly information on egg production, usage and prices. This includes UK egg packing station throughput by country and egg production system (intensive, barn, free range, organic) and prices paid by UK egg packers to producers. The information about egg usage includes the number of eggs bought by UK egg processors and the quantity of egg products they produce. Monthly information about trade in eggs and egg products is also included.",
  "Shorthand",         "All sheets in this workbook use shorthand: [z] means 'not applicable' because data was not collected and [c] means the value was suppressed for confidentiality reasons.",
  "Methodology",       paste0("\nDefra runs a quarterly survey of registered UK egg packing stations. It is a voluntary sample survey of 27 respondents that collects information on throughput by production type and prices of graded eggs and sales of ungraded eggs. The response rate is typically 100 per cent and the survey accounts for 75 per cent of eggs packed in the UK. The survey figures are raised up to give UK estimates using information on the number of commercial laying hens, average egg yields, average mortality rates, the proportion of UK eggs that go through packing stations. Throughput by egg type for packing stations not surveyed is calculated using data provided by packing stations responding to the survey.  The raised figures are published in this statistics notice and the associated datasets. The figures in this notice therefore represent all Class A eggs passed over a grader in the UK, including seconds. The prices obtained on the survey are weighted according to the volume of eggs packed by each packing station to obtained average prices for the UK. From 2012, prices include any bonus payments paid to producers.",
                              "\n\nIn tables that show numbers of eggs the units used are 'thousand cases'. There are 360 eggs in one case.",
                              "\n\nThe data are subject to a variety of validation checks which identify inconsistencies in the data. All data are cleaned prior to publication.",
                              "\n\nThe percentage changes shown are calculated using unrounded figures. Thus any percentage changes calculated using the published (rounded) figures may not equate exactly with the changes shown.",
                              "\n\nThroughput was previously published in cases. To aid users who wish to convert this data from dozens into cases; there are 360 eggs or 30 dozens in a case.",
                              "\n\nThese data represent all Class A eggs passed over a grader, including seconds."),
  "Revisions policy",  paste0("Figures in this dataset are provisional and subject to revision. We will provide information about any revisions we make to previously published information in this dataset, and the associated statistics notice. Revisions could occur for various reasons including:",
                              "if we have not received survey data from respondents we make an estimate based on their previous returns (these estimates will be replaced with actual survey data when it is received)",
                              "; if survey respondents occasionally supply amended figures for previous periods",
                              "; or if we have revised the methodology used to raise the survey data to give UK totals."),
  "Data users",        paste("Representatives of the egg and poultry industry are also major users of the data. The data on egg production volumes and egg type are the key sector indicators for the British Egg Industry Council (BEIC) as they reflect the size of the national laying flock. The Home Grown Cereals Authority (HGCA), part of the Agricultural and Horticultural Development Board, rely on egg production data as a good indicator of the commercial layer flock and associated feed demand and hence grain usage by the sector.",
                             "Our statistics are also often heavily referenced in industry publications such as 'Poultry World'."),
  "Contact",           "Firstname Lastname, +44 (0) 1234 567 890, firstname.lastname@department.gov.uk"
)

# Table of contents
contents_df <- tribble(
  ~"Tab name", ~"Sheet title",
  "Notes",     "Notes",
  "Table_1",   "Throughput by country, quarterly",
  "Table_2",   "Throughput by system, quarterly",
  "Table_3",   "Average packer to producer prices, quarterly",
  "Table_4",   "Throughput by country, annual",
  "Table_5",   "Throughput by system, annual",
  "Table_6",   "Average packer to producer prices, annual"
)

# Table of notes (notes in square brackets as per the guidance)
notes_df <- tribble(
  ~"Note number", ~"Note text",
  "[note 1]",     "All eggs from free range and organic flocks have been re-labelled as barn eggs since 21st March 2022 due to avian influenza restrictions.",
  "[note 2]",     "From Q1 2006 the average UK All Types Price includes Organic eggs.",
  "[note 3]",     "Prices up to and including 2011 are a period end price and exclude bonus payments.",
  "[note 4]",     "From Q1 2012 prices shown are based on an average across the period and include bonus payments."
)

# Combine into a list
meta_list <- list(cover = cover_df, contents = contents_df, notes = notes_df) |> 
  map(~data.frame(.x, check.names = FALSE))


# Prepare objects for create_a11ytable() ----------------------------------


# Titles to go on the tabs (should match notes_df)
egg_tab_titles <- c("Cover", "Contents", "Notes", paste0("Table_", 1:6))

# Sheet types to indicate how the sheet will be styled (can only be 'cover',
# 'contents', 'notes' or 'tables')
egg_sheet_types <- c("cover", "contents", "notes", rep("tables", 6))

# Titles for each sheet (goes into cell A1)
egg_sheet_titles <- c(
  "UK egg packing station throughput and prices",
  "Contents",
  "Notes",
  "Table 1: Throughput by country, quarterly",
  "Table 2: Throughput by system, quarterly",
  "Table 3: Average packer to producer prices, quarterly",
  "Table 4: Throughput by country, annual",
  "Table 5: Throughput by system, annual",
  "Table 6: Average packer to producer prices, annual"
)

# Supply any reasons for blank cells on a sheet-by-sheet basis
egg_blank_cells <-  rep(NA_character_, 9)

# Supply sources on sheet-by-sheet basis
egg_sources <- c(
  rep(NA_character_, 3),
  rep("Quarterly UK Egg Packing Station Survey", 6)
)

# The tables should be a list-column of 'flat' dataframes
egg_tables <- c(meta_list, tables_list)

# Remove all objects except the ones that will go into create_a11ytable() in the
# example in the presentation (I call ls() in the slides and I want only to see
# the objects I care about)
rm(list = ls()[!(ls() %in% c(
  "egg_tab_titles", "egg_sheet_types", "egg_sheet_titles", 
  "egg_blank_cells", "egg_sources", "egg_tables"
))])
