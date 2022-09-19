# Import comparison tables

darwin <- read.csv(file = "csv/comparison_R53407_Darwin's naturalisation.csv")
darwin$Title <- darwin$publication
names(darwin) <- stringr::str_replace_all(names(darwin), "\\.","_")
darwin <- darwin[!duplicated(darwin$Title),]

enemy <- read.csv(file = "csv/comparison_R58002_Enemy release.csv")
enemy$Title <- enemy$publication
names(enemy) <- stringr::str_replace_all(names(enemy), "\\.","_")
enemy <- enemy[!duplicated(enemy$Title),]

# Merge all tables in one
df_list <- list(darwin,enemy)
total_df <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  df_list
)

# order support factor correctly:
total_df$support_for_hypothesis <- factor(total_df$support_for_hypothesis,
                                    levels = c("Supported","Undecided","Questioned"))


# Homogenize continent information
library(stringr)
total_df$Continent <- str_replace_all(
  str_replace(
    str_replace(total_df$Continent,"\\[", ""),
    "\\]", ""),
  "'", "")
## Create a column that is a list of Continents :
total_df$continents <- str_split ( total_df$Continent, pattern = ",")


# Homogenize taxa information
library(stringr)
total_df$Investigated_species <- str_replace_all(
  str_replace_all(total_df$Investigated_species," and ", ","),
  "-", ",")
## Create a column that is a list of taxon :
total_df$taxa <- str_split ( total_df$Investigated_species , pattern = ",")


# Chronological accumulation of studies:
total_df =  group_by(.data = total_df, hypothesis) %>%
  mutate(chrono_hyp = row_number(Study_date))
total_df =  group_by(.data = total_df, hypothesis , support_for_hypothesis) %>%
  mutate(chrono_support = row_number(Study_date))

