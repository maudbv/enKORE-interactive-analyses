# Import and pre-process data for the hi-knowledge shiny app
# 2022 Maud Bernard-Verdier
require(stringr)
require(purrr)
require(dplyr)
require(readr)

taxa_grouping <-  read_csv("resources/additional data/taxa grouping.csv", show_col_types = FALSE)

all_files <- list.files('resources/csv', pattern = 'comparison*', full.names = TRUE)
df_list <- map(all_files,
                 ~.x %>% readr::read_csv(show_col_types = FALSE))

# homogenize column names
#unique_columns <- unlist(lapply(1:9, function(x) setdiff(tmp[[x]], tmp[[x+1]])))

# #Sub-hyp & sub-sub hyp column names:   TO COMPLETE
#  list("Measure of species similarity",
#    "Measure of Species Relationship",
#    
#  )

# Correct one wrong column name
df_list <-  lapply(df_list,function(x) {
  names(x)[which(names(x) == "number of plant species")] = "Number of species"
  return(x)
  })

# # Import comparison tables exported from ORKG via python package ####
# darwin <- read.csv(file = "resources/csv/comparison_R53407_Darwin's naturalisation.csv")
# enemy <- read.csv(file = "resources/csv/comparison_R58002_Enemy release.csv")
# 
# # Merge all tables in one ####
# df_list = list(darwin,enemy)

# Reduce to one dataframe
total_df <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

# reformat column names
total_df$Title <- total_df$publication
names(total_df) <- stringr::str_replace_all(names(total_df), "\\ ","_")

# Chronological accumulation of studies: ####
total_df <-   group_by(.data = total_df, hypothesis) %>%
  mutate(chrono_hyp = row_number(Study_date))

total_df <-   group_by(.data = total_df, hypothesis , support_for_hypothesis) %>%
  mutate(chrono_support = row_number(Study_date))

total_df <-  ungroup(total_df)

# order support factor correctly: ####
total_df$support_for_hypothesis <- factor(total_df$support_for_hypothesis,
                                    levels = c("Supported","Undecided","Questioned"))

# correct typo in hyps ####
total_df$hypothesis <- str_replace(string = total_df$hypothesis,
                                   pattern = "Biotic resistence",
                                   replacement = "Biotic resistance")
  
# Homogenize continent information ####
total_df$Continent <- str_replace_all(
  str_replace(
    str_replace(total_df$Continent,"\\[", ""),
    "\\]", ""),
  "'", "")
## Create a column that is a list of Continents :
total_df$continents <- str_split(tolower(total_df$Continent), pattern = ",")

# Homogenize taxa information ####

taxa_col <-  total_df$Investigated_species

taxa_col <- str_replace_all(string = taxa_col, "Inscets","Insects")
taxa_col <- str_replace_all(string = taxa_col, "Insect","Insects")
taxa_col <- str_replace_all(string = taxa_col, "Insectss","Insects")
taxa_col <- str_replace_all(string = taxa_col, " Fishes","Fishes")
taxa_col <- str_replace_all(string = taxa_col, " Mammals","Mammals")
taxa_col <- str_replace_all(string = taxa_col, " Molluscs","Molluscs")
taxa_col <- str_replace_all(string = taxa_col, "Algaei","Algae")
taxa_col <- str_replace_all(string = taxa_col, " Crustaceans","Crustaceans")
taxa_col <- str_replace_all(string = taxa_col, " Birds","Birds")
taxa_col <- str_replace_all(string = taxa_col, " Reptiles","Reptiles")
# taxa_col <- str_replace_all(string = taxa_col, " andreptiles","and Reptiles")
taxa_col <- str_replace_all(string = taxa_col, "PlantsMolluscs","Plants,Molluscs")
taxa_col <- str_replace_all(string = taxa_col, " and ","and")
taxa_col <- str_replace_all(string = taxa_col, " and","and")
taxa_col <- str_replace_all(string = taxa_col, "and ","and")

taxa_col  <- str_replace_all(
  str_replace_all(
    str_replace_all(taxa_col ,"and", ","),
    "-", ","),
  ";", ",")


total_df$Investigated_species <- taxa_col

## Create a column that is a list of taxon :
 total_df$taxa <- str_split(tolower(total_df$Investigated_species) , pattern = ",")
 
#### TODO : add here a column with higher level taxa grouping
 
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
