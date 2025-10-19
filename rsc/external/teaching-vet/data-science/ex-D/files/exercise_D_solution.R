##########################################################################################################################
## Data science template code for exercise D, version 1.0.0 (2025-10-18)
## Developed by Matt Denwood <md@sund.ku.dk> for students at the University of Copenhagen
## Available from:  https://ku-awdc.github.io/rsc/external/teaching-vet/data-science/ex-C/files/data_science_template_D.R
## Distribution and re-use is permitted under a CC-BY license (https://creativecommons.org/licenses/by/4.0/)
##########################################################################################################################

## Run the magic incantation below to make sure your software is installed correctly:
{\(){; minRdt <- as.Date(paste0(as.numeric(strftime(Sys.Date(), "%Y")) - ifelse(as.numeric(strftime(Sys.Date(), "%m")) %in% 1:3, 2, 1) ,"-04-01")); Rdt <- with(R.Version(), paste(year, month, day, sep="-")); if(minRdt > as.Date(Rdt)) stop("Your R version (from ", Rdt, ") is no longer supported: please re-install from https://cran.r-project.org", call.=FALSE); options(install.packages.check.source="no", install.packages.compile.from.source="never", tidyverse.quiet=TRUE, dplyr.summarise.inform=FALSE); if(length(find.package(c("nanonext","mirai","rstudioapi"), quiet=TRUE, verbose=FALSE)) != 3L){; install.packages(c("nanonext","mirai","rstudioapi")); }; rq <- \(x) require(x, character.only=TRUE, quietly=TRUE) |> suppressPackageStartupMessages() |> suppressWarnings(); mr <- \(x) mirai::mirai({ rq(pp) }, .args=list(pp=x, rq=rq)) |> mirai::collect_mirai(); pkglist <- c("checkmate","assertr","tidyverse"); for(pp in pkglist){; if(!mr(pp)){; install.packages(pp); if(length(find.package(pp, quiet=TRUE, verbose=FALSE))==0L) stop("Installing the ", pp, " package failed\nPlease make sure that you have installed R from https://cran.r-project.org\nrecently, and that you have an active internet connection", call.=FALSE); }; }; if(!all(sapply(pkglist, rq)) && rstudioapi::isAvailable()){; rstudioapi::restartSession(paste0("options(install.packages.check.source='no',install.packages.compile.from.source='never'); suppressWarnings(install.packages(c(tools::package_dependencies(", paste0("c('",paste(pkglist,collapse="','"),"')"), ",recursive=TRUE)|>unlist()|>unique(),'",paste(pkglist,collapse="','"),"'))); cat('\n\n*** R NEEDED TO RESTART: PLEASE RUN THE INCANTATION AGAIN ***\n\n')"), clean = TRUE); }; pkglist <- c(pkglist, "readxl", "broom"); if(!all(sapply(pkglist, rq))) stop("Installation of packages failed!\nPlease quit RStudio, re-open and try again.\nIf it fails again after restarting then seek help.", call.=FALSE); if(rstudioapi::isAvailable()){; rsw <- rstudioapi::writeRStudioPreference; rsw("soft_wrap_r_files", TRUE); rsw("rainbow_parentheses", TRUE); rsw("restore_source_documents", FALSE); rsw("restore_last_project", FALSE); }; theme_set(theme_light()); cli::cli_alert_success("\nSuccess: your R session is ready to use :)\n"); }}(); enforce <- \(x, ...){; if(!qtest(x, "d")) stop("First argument to enforce must be a data frame"); if(...length()!=1L) stop("You must provide exactly one statement to enforce"); x |> group_split() |> lapply(\(y) y |> with(...) |> withVisible() |> _$visible |> ifelse(assertr::verify(y, ...), y)); x; }
## Note: you don't need to understand what this does; just run it!

#####################################################################################################################
## Notes:

# This R script is for solving Exercise D
# It was written by Matt Denwood on 2025-10-18

## End notes
#####################################################################################################################


#####################################################################################################################
## File setup:

# Show filenames in this directory, so we can copy/paste what we need:
list.files()

# Enter the Excel file name that we are working with (needs to be changed!):
data_file <- "example_data.xlsx"

# List sheets within the excel file (helpful for copy/paste):
excel_sheets(data_file)

# Read and save the metadata:
metadata <- read_excel(data_file, sheet="metadata")

# Read and save the tables sheet:
tables <- read_excel(data_file, sheet="tables")

# Read and save the variables sheet:
variables <- read_excel(data_file, sheet="variables")

## End file setup
#####################################################################################################################


#####################################################################################################################
## Reading, formatting and verifying data from a specific sheet:

# The name of the Excel sheet we want to read (needs to be changed!):
sheet_name <- "example_dogs"

# Read the Excel file/sheet:
example_dogs_raw <- read_excel(data_file, sheet=sheet_name)

# Examine the data structure:
str(example_dogs_raw)

# Look-up table for variable types:
#  Variable-type    R-type
#  nominal          Factor
#  ordinal          Factor or Ord.factor (either is fine, but we recommend Factor)
#  id               chr
#  text             chr
#  discreet         num or int (either is fine)
#  continuous       num
#  date             Date
#  datetime         POSIXct

## Data formatting and cleaning pipeline:
example_dogs_raw |>

  ## Add formatting and verification code below here!

  # Formatting discrete variables as ID and text variables (num/int -> chr) or adding text to ID variables (chr -> chr):
  mutate(DogID = str_c("Dog_", DogID, "")) |>
  # Verifying variables with R-type chr:
  enforce(assert_character(DogID, any.missing=FALSE, unique=TRUE)) |>

  # Verifying variables with R-type chr:
  enforce(assert_character(Name, any.missing=FALSE, unique=FALSE)) |>

  # Verifying variables with R-type chr:
  enforce(assert_character(Address, any.missing=FALSE, unique=FALSE)) |>

  # Formatting nominal or ordinal variables (chr -> Factor):
  mutate(Sex = fct(Sex, levels=c("Male","Female"))) |>
  # Verifying variables with R-types Factor or Ord.factor
  enforce(assert_factor(Sex, any.missing=TRUE, unique=FALSE)) |>

  # Formatting nominal or ordinal variables (chr -> Factor):
  mutate(Breed = fct(Breed, levels=c("Alsation","Beagle","Husky","Terrier","Weimaraner"))) |>
  # Verifying variables with R-types Factor or Ord.factor
  enforce(assert_factor(Breed, any.missing=TRUE, unique=FALSE)) |>

  # Formatting datetime variables as date variables (POSIXct -> Date):
  mutate(BirthDate = as_date(BirthDate)) |>
  # Verifying variables with R-type Date:
  enforce(assert_date(BirthDate, lower=NULL, upper="2025-10-21", any.missing=TRUE, unique=FALSE)) |>

  # Verifying discreet variables with R-type num or int:
  enforce(assert_integerish(MealsPerDay, lower=1, upper=4, any.missing=TRUE, unique=FALSE)) |>

  ## Add formatting and verification code above here!

  ## End of pipeline:
  identity() ->
  example_dogs

# Re-examine the data structure and show summary statistics:
str(example_dogs)
summary(example_dogs)

## End reading, formatting and verifying data from the given sheet
#####################################################################################################################

