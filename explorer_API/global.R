# global.R

# Load required libraries

## 1. Base R : These are libraries primarily focused on foundational data manipulation, I/O, and processing
library(data.table)
library(stringr)
library(purrr)
library(readxl)
library(pdftools)
library(sas7bdat)
library(XML)
library(jsonlite)
library(dbplyr)

## 2. User Experience (UX): These libraries enhance the interface and interactivity

library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(bs4Dash)
library(reactlog)
library(glue)
library(RColorBrewer)

## 3. Visualization: These libraries are used for creating visual representations of data

library(visNetwork)
library(heatmaply)
library(DT)
library(DataExplorer)

## 4. Data Science: Libraries used for broader data analysis, wrangling, or modeling tasks:

library(tidyverse)

## 5. API/HTTP/database: These libraries are specifically for working with APIs, HTTP requests and connecting to database:

library(httr2)
library(DBI)
library(RSQLite)

# source the app functions
source("dataExplorer_functions.R")

# Source the app modules into the server script
source("navbar_menu_module.R")
source("homeTab_module.R")
source("aboutTab_module.R")
source("networkTab_module.R")
source("datareviewTab_module.R")
source("studyoverview_module.R")
source("metadata_module.R")
source("dataExploration_module.R")



