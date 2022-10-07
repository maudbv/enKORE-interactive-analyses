# Import and pre-process data for the hi-knowledge shiny app
# 2022 Maud Bernard-Verdier
library(stringr)

# Import comparison tables exported from ORKG via python package ####
darwin <- read.csv(file = "resources/csv/comparison_R53407_Darwin's naturalisation.csv")
darwin$Title <- darwin$publication
names(darwin) <- stringr::str_replace_all(names(darwin), "\\.","_")
darwin <- darwin[!duplicated(darwin$Title),]

enemy <- read.csv(file = "resources/csv/comparison_R58002_Enemy release.csv")
enemy$Title <- enemy$publication
names(enemy) <- stringr::str_replace_all(names(enemy), "\\.","_")
enemy <- enemy[!duplicated(enemy$Title),]

# Merge all tables in one ####
df_list <- list(darwin,enemy)
total_df <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

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
  str_replace_all(total_df$Investigated_species," and ", ","),
  "-", ",")

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
habitat_groups <- sort(unique(tolower(unlist(total_df$Habitat))))
method_groups <- sort(unique(tolower(unlist(total_df$Research_Method))))
