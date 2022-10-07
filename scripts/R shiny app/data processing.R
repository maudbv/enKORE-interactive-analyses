# Import and pre-process data for the hi-knowledge shiny app
# 2022 Maud Bernard-Verdier
library(stringr)
library(purrr)

# all_files <- list.files('resources/csv', pattern = 'comparison*', full.names = TRUE)
# df_list <- map(all_files, 
#                  ~.x %>% readr::read_csv(show_col_types = FALSE))
# 
# # Correct one wrong column name
# df_list <-  lapply(df_list,function(x) {
#   names(x)[which(names(x) == "number of plant species")] = "Number of species"
#   return(x)
#   })

# Import comparison tables exported from ORKG via python package ####
darwin <- read.csv(file = "resources/csv/comparison_R53407_Darwin's naturalisation.csv")
enemy <- read.csv(file = "resources/csv/comparison_R58002_Enemy release.csv")

# Merge all tables in one ####
df_list = list(darwin,enemy)
total_df <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

# reformat column names
total_df$Title <- total_df$publication
names(total_df) <- stringr::str_replace_all(names(total_df), "\\.","_")

# Chronological accumulation of studies: ####
total_df <-   group_by(.data = total_df, hypothesis) %>%
  mutate(chrono_hyp = row_number(Study_date))

total_df <-   group_by(.data = total_df, hypothesis , support_for_hypothesis) %>%
  mutate(chrono_support = row_number(Study_date))

total_df <-  ungroup(total_df)

# order support factor correctly: ####
total_df$support_for_hypothesis <- factor(total_df$support_for_hypothesis,
                                    levels = c("Supported","Undecided","Questioned"))


# Homogenize continent information ####
total_df$Continent <- str_replace_all(
  str_replace(
    str_replace(total_df$Continent,"\\[", ""),
    "\\]", ""),
  "'", "")
## Create a column that is a list of Continents :
total_df$continents <- str_split(tolower(total_df$Continent), pattern = ",")

# Homogenize taxa information ####
total_df$Investigated_species <- str_replace_all(
  str_replace_all(
    str_replace_all(total_df$Investigated_species," and ", ","),
  "-", ","),
  ";", ",")
## Create a column that is a list of taxon :
total_df$taxa <- str_split(tolower(total_df$Investigated_species) , pattern = ",")

# Homogenize Habitat information ####
# Create a habitat column that is a list of habitat :
total_df$Habitat <- str_split(tolower(total_df$Habitat) , pattern = "/")

# Homogenize method information ####
total_df$Research_Method[grep("obs", total_df$Research_Method, ignore.case = TRUE)] <-  "observational"
total_df$Research_Method[grep("experim", total_df$Research_Method, ignore.case = TRUE)] <-  "experimental"

# create clean vectors of filtering factor ####
taxa_groups <-  sort(unique(tolower(unlist(total_df$taxa))))
taxa_groups <- taxa_groups[which((taxa_groups != "") & !is.na(taxa_groups))]

habitat_groups <- sort(unique(tolower(unlist(total_df$Habitat))))
method_groups <- sort(unique(tolower(unlist(total_df$Research_Method))))
