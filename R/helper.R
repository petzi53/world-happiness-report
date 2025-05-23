## HEADER #######################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Last edit date: April 20, 2025
# CONTENT:
## - load glossary package
## - my_create_folder: create folder at path if it not already exists
## - my_save_data_file: save data file
## - my_excel_as_csv_and_rds:
##            save content of stored Excel file with all sheets
##            as CSV snapshots and RDS objects
## - my_ls_region: well being ladder score line chart for regions


## glossary #####################################################
library(glossary)

glossary::glossary_path("../glossary-pb/glossary.yml")


## my_create_folder #############################################
# my_create_folder:
# Purpose:
# check if folder already exists at parameter "path"
# if not, then create folder
# Author: Peter Baumgartner
# path = character string:
#                example: "/Users/xxyyzz/Documents/my-data/"

my_create_folder <- function(path){

  if (!base::file.exists(path))
    {base::dir.create(path)}
}


## my_save_data_file ############################################
# my_save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"

my_save_data_file <- function(chapter_folder, object, file_name){
    data_folder <- base::paste0(here::here(), "/data/")
    if (!base::file.exists(data_folder))
    {base::dir.create(data_folder)}

    chap_folder <-
        base::paste0(
            here::here(),
            paste0("/data/", chapter_folder, "/")
        )
    if (!base::file.exists(chap_folder))
    {base::dir.create(chap_folder)}

    base::saveRDS(object = object,
                  file = paste0(chap_folder, "/", file_name))
}



## my_excel_as_csv_and_rds ######################################
# my_excel_as_csv_and_rds:
# Purpose:
#            save content of stored Excel file with all sheets
#            as CSV snapshots and RDS objects
# Author: Peter Baumgartner


# sheet: vector of sheet names
# path_excel: path to the already saved excel file
# path_csv: folder path where to store all Excel sheets as .csv files
# path_rds: folder path where to store all Excel sheets as .rds objects

my_excel_as_csv_and_rds <- function(
    sheet, path_excel, path_csv, path_rds) {
  path_base <- path_excel |>
    base::basename()  |>
    tools::file_path_sans_ext()
  path_excel  |>
    readxl::read_excel(sheet = sheet) |>
    readr::write_csv(base::paste0(path_csv, path_base, "-", sheet, ".csv")) |>
    readr::write_rds(base::paste0(path_rds, path_base, "-", sheet, ".rds"))
}

class_scheme <- function(df, sel1, sel2) {
  ## df = dataframe to show
  ## sel1 = name of the first column (country names) to select
  ## sel2 = name of the column with the regional indicator
  df |>
    dplyr::select(!!sel1, !!sel2) |>
    dplyr::nest_by(!!sel2) |>
    dplyr::mutate(data = as.vector(data)) |>
    dplyr::mutate(data = stringr::str_c(data, collapse = "; ")) |>
    dplyr::mutate(data = paste(data, ";")) |>
    dplyr::mutate(N = lengths(gregexpr(";", data))) |>
    dplyr::rename(Country = data) |>
    dplyr::arrange(!!sel2) |>
    DT::datatable(class = 'cell-border compact stripe',
                  options = list(
                    pageLength = 25,
                    lengthMenu = c(5, 10, 15, 20, 25, 50),
                    columnDefs = list(
                      list(className = 'dt-body-left', targets = 2)
                    )
                  )
    )
}

class_scheme2 <- function(df, sel1, sel2) {
  ## df = dataframe to show
  ## sel1 = name of the first column (country names) to select
  ## sel2 = name of the column with the regional indicator
  df |>
    dplyr::select(!!sel1, !!sel2) |>
    dplyr::nest_by(!!sel2) |>
    dplyr::mutate(data = as.vector(data)) |>
    dplyr::mutate(data = stringr::str_c(data, collapse = "; ")) |>
    dplyr::mutate(data = paste(data, ";")) |>
    dplyr::mutate(N = lengths(gregexpr(";", data))) |>
    dplyr::rename(Country = data) |>
    dplyr::arrange(!!sel2)
}


## my_ls_region ######################################
# Create plotly line chart with well being ladder scores (ls)
# Data from the World Happiness Report (WHR)
# Using as origin data my `whr_final`
# combining WHR data and M49/World Bank classification dataset
# Author: Peter Baumgartner


my_ls_region <- function(
    df,                     # dataset with three or four columns
                            #   - year (numeric)
                            #   - region (factor)
                            #   - score (numeric)
    fig_title,              # figure title (character)
    mean_column  = TRUE,    #  is there a mean column (logical)
    legend_title = "Region" # legend title (character)
    ) {

if (mean_column) {
      df_mean <- df |>
        dplyr::group_by(year) |>
        dplyr::summarize(
          mean = mean(score)
        )

      df <- dplyr::left_join(
        x = df,
        y = df_mean,
        by = dplyr::join_by(year)
      )
    }

p <- df |>
      plotly::plot_ly(
        x = ~year,
        y = ~score
      ) |>
      plotly::add_trace(
        color = ~ forcats::fct_reorder2(
          region, year, score, .fun = forcats::last2
        ),
        type = "scatter",
        mode = "lines+markers",
        colors = my_colors13()
      )

      (function(x) if (mean_column) {
        plotly::add_lines(
          p,
          y = ~mean,
          name = "<b>mean</b>",
          mode = "lines+marker",
          line = list(
            width = 6,
            dash = "dot",
            color = "black",
            opacity = 0
          )
        )
       }else (x))() |>

      plotly::layout(
        title = fig_title,
        legend = base::list(title =
                base::list(text =
                base::paste0('<b>', legend_title, '</b>'))
            ),
        xaxis = base::list(title = "Year"),
        yaxis = base::list(title = "Cantril Ladder Score")
      ) |>
      plotly::config(doubleClickDelay = 1000)
  }


my_get_ls_data <-  function(
  group_name,
  region_name,
  world_bank_group,
  filter_string
  ) {

    base::readRDS(
      base::paste0(here::here(),
                   "/data/whr-cantril/rds/whr_final.rds")
    ) |>
    dplyr::filter(
      stringr::str_detect(
        !!group_name, filter_string) &
        group48 == world_bank_group
      ) |>
    dplyr::select(year, !!region_name, ladder_score) |>
    dplyr::summarize(
      score = mean(ladder_score), .by = c(year, !!region_name)
    ) |>
    dplyr::rename(region = !!region_name) |>
    base::droplevels()
  }


## my_colors13 ######################################
# Southern Europe has 13 categories
# add 13th color to RColorBrewer "Set3" with 12 colors
# idea from Brave-KI "RColorBrewer what to do with 13 categories"
# Author: Peter Baumgartner

my_colors13 <-  function() {
  c(RColorBrewer::brewer.pal(12, "Paired"), "#999999")
}

## my_colors ######################################
# Sometimes I need more than colors for max. 12 categories
# Combine different palettes 'Paired' and 'Set3'
# resulting in 24 colors
# idea from Brave-KI "RColorBrewer combining multiple palettes"
# see also: https://stackoverflow.com/a/70739992/7322615
# and: https://stackoverflow.com/questions/15282580/
# Author: Peter Baumgartner

my_colors <-  function() {
  c(
    RColorBrewer::brewer.pal(12,'Paired'),
    RColorBrewer::brewer.pal(12,'Set3')
  )
}

## END

