# Import and pre-process data for the hi-knowledge shiny app
# 2022 Maud Bernard-Verdier
require(stringr)
require(purrr)
require(dplyr)
require(readr)

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

# correct some strange typos:
total_df$publication <- str_replace_all(total_df$publication,"\n","")
total_df$publication <- str_replace_all(total_df$publication,"\"","\'")

total_df$index <- str_replace_all(total_df$index,"\n","")
total_df$index <- str_replace_all(total_df$index,"\"","\'")

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

# check for duplicate rows

total_df$index[which(duplicated(total_df$index))] <- paste( total_df$index[which(duplicated(total_df$index))],
                                                            "a", sep = "")

# Homogenize continent information ####
total_df$Continent_old <-  total_df$Continent

total_df$Continent <- str_replace_all(
  str_replace(
    str_replace(total_df$Continent,"\\[", ""),
    "\\]", ""),
  "'", "")

# correct typos
continents_vec <-  c("Africa","Asia","Antarctica","Europe", "North-America", "South-America","Oceania")
total_df$Continent <- str_replace_all(total_df$Continent,pattern = 'North ', 'North-')
total_df$Continent <- str_replace_all(total_df$Continent,pattern = 'South ', 'South-')
total_df$Continent <- str_replace_all(total_df$Continent,pattern = 'North ', 'North-')
total_df$Continent <- str_replace_all(total_df$Continent,pattern = 'Hawaii ', '')
total_df$Continent <- str_replace_all(total_df$Continent,pattern = ';', ',')
total_df$Continent <- str_replace_all(total_df$Continent,pattern = ', ', ',')

total_df$Continent <- str_replace_all(total_df$Continent,
                                      pattern = 'All continents except Antarctica',
                                      replacement = paste(continents_vec[-3], collapse = ","))
total_df$Continent <- str_replace_all(total_df$Continent,
                                      pattern = 'All continents except Antarctica and Asia',
                                      replacement = paste(continents_vec[-c(2,3)], collapse = ","))
total_df$Continent <- str_replace_all(total_df$Continent,
                                      pattern = 'All continents except Asia and Antarctica',
                                      replacement = paste(continents_vec[-c(2,3)], collapse = ","))
total_df$Continent <- str_replace_all(total_df$Continent,
                                      pattern = 'All continents',
                                      replacement = paste(continents_vec, collapse = ","))

total_df$Continent <- str_replace_all(total_df$Continent,pattern = ' and ', ',')
total_df$Continent <- str_replace_all(total_df$Continent,
                                      pattern = ' ',
                                      replacement = ",")


## Create a column that is a list of Continents :
total_df$continents <- str_split(total_df$Continent, pattern = ",")
table(unlist(total_df$continents))

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
taxa_grouping <-  read_csv("resources/additional data/taxa grouping.csv", show_col_types = FALSE)

# order taxa groups in the table:
taxa_grouping <- taxa_grouping %>% 
  group_by((taxa_groups_large)) %>% 
  arrange((taxa_groups), .by_group = TRUE)

# extract taxa names and groups
taxa_groups <- unique(taxa_grouping$taxa_groups)
taxa_groups <- taxa_groups[-which(taxa_groups == "any")]
taxa_groups_large <- sort(unique(taxa_grouping$taxa_groups_large))
taxa_groups_large <- taxa_groups_large[-which(taxa_groups_large == "any")]
taxa_labels <-  unique(tolower(unlist(total_df$taxa)))
taxa_labels <- taxa_groups[which((taxa_groups != "") & !is.na(taxa_groups))]

# create columns with taxa groups
total_df$taxa_group <- lapply(total_df$taxa, FUN = function(x) {
  taxa_grouping$taxa_groups[match(x, taxa_grouping$taxa_label)]
})
total_df$taxa_group <- lapply(total_df$taxa_group, FUN = function(x){
  if ("any" %in% x) return(taxa_groups)
  else x
})

total_df$taxa_group_large <- lapply(total_df$taxa, FUN = function(x) {
  taxa_grouping$taxa_groups_large[match(x, taxa_grouping$taxa_label)]
})
total_df$taxa_group_large <- lapply(total_df$taxa_group_large, FUN = function(x){
  if ("any" %in% x) return(taxa_groups_large)
  else x
})

# Homogenize Habitat information ####
# Create a habitat column that is a list of habitat :
total_df$Habitat <- str_replace_all(str_replace_all(string = total_df$Habitat, 
                                            pattern = "/",replacement = ","),
                                    pattern = " and ", replacement = ",")
total_df$Habitat[which(total_df$Habitat=="x")]  <-  "undetermined"

# Group "Brackishwater" with freshwater research for simplification (and because represented by only a few studies )
total_df$Habitat <- str_replace_all(string = total_df$Habitat, 
                pattern = "(b|B)rackishwater",replacement = "freshwater" )

#Convert to a list to be able to list unique habitats
total_df$Habitat_list <- str_split(tolower(total_df$Habitat) , pattern = c(","))
 
# Homogenize method information ####
total_df$Research_Method[
  grep("obs", total_df$Research_Method, ignore.case = TRUE)] <-  "observational"
total_df$Research_Method[
  grep("experim", total_df$Research_Method, ignore.case = TRUE)] <-  "experimental"

# create clean vectors of filtering factor ####
habitat_groups <- sort(unique(tolower(unlist(total_df$Habitat_list))))
method_groups <- sort(unique(tolower(unlist(total_df$Research_Method))))
continents_vec<- sort(unique(unlist(total_df$continents)))

# RE-label the hypotheses
total_df$hypothesis_old <- total_df$hypothesis

hyp_labels <- data.frame(old_label = unique(total_df$hypothesis),
                         new_label = c("Disturbance hypothesis",
                                       "Biotic resistance hypothesis",
                                       "Enemy release hypothesis",
                                       "Island susceptibility hypothesis",
                                       "Phenotypic plasticity hypothesis",
                                       "Invasional meltdown hypothesis",
                                       "Tens rule",
                                       "Darwin's naturalisation hypothesis",
                                       "Propagule pressure hypothesis",
                                       "Limiting similarity hypothesis")
)
total_df$hypothesis <- hyp_labels$new_label[match(total_df$hypothesis_old, hyp_labels$old_label)]
