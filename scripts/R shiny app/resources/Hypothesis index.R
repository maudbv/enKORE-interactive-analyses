# Build a central index of hypotheses

# import list of all hypotheses and definitions:

hyp_mat <- readr::read_csv("resources/additional data/Roxane hyps merged with attributes.csv",
                           trim_ws = TRUE, skip_empty_rows = TRUE)
hyp_mat <- as.data.frame(hyp_mat)

# check for duplicated acronyms
hyp_mat[which(duplicated(hyp_mat$Acronym)),] %>%
  select(Acronym, Hypothesis,Definition)

#
# Acronym                                         Hypothesis
# 25       DS                                     Matrix species
# 59       HP                           Herbivore proliferation*
# 71       IS Non-native species hypothesis* aka Invader species
# 96       PO                ﻿Pest outbreaks - defense-free space
# 99      PPH                             Predator proliferation
# 118      SP                                     Suburban peak*

# rename redundant acronyms
hyp_mat[which(duplicated(hyp_mat$Acronym)),"Acronym"] <- c(
  "MS",    # Matrix Species
  "HbP",   # Herbivore Proliferation
  "UIS",   # Urban Invader Species
  "DFS",   # Defense Free Space
  "HPPC",  # High Predator Proliferation in Cities"
  "SUP"    # SubUrban Peak
)

# Make acronyms as rownames
rownames(hyp_mat) <- hyp_mat$Acronym # OK

# Rename hypotheses to include the word "hypothesis" or "rule"

hyp_mat <- mutate(hyp_mat,
                  Hypothesis_name = paste(Hypothesis,"hypothesis"))
hyp_mat$Hypothesis_name


# transform in simplified index table
hyp_index <- select(hyp_mat, Acronym:`Key ref`,
                    Origin:`Lokatis' acronyms + new acronyms for Lokatis sub-hypotheses`,
                    `Daly's Hypothesis`,`Enders' Hypothesis`)


# Add themes and RQ ####

# Import matrices
rhrq_mat <- readr::read_csv("resources/additional data/RH-RQ.csv")
theme_rq_mat <- as.data.frame(readr::read_csv("resources/additional data/Theme-RQ.csv"))

# add columns in hyp_mat

tmp <- apply(as.data.frame(rhrq_mat), 1, FUN = function(x) paste(names(rhrq_mat)[x==1], collapse = "; "))
hyp_index$RQ <- tmp[ match(hyp_index$Acronym, names(tmp))]

# Add Wikidata pages


