# Build a central index of hypotheses
library(dplyr)

# import list of all hypotheses and definitions:

hyp_mat <- readr::read_csv("resources/additional data/hyp_def.csv",
                           trim_ws = TRUE,
                           skip_empty_rows = TRUE,
                           name_repair = "universal")

hyp_mat <- as.data.frame(hyp_mat)

# check for duplicated acronyms
nrow(hyp_mat[which(duplicated(hyp_mat$Acronym)),] %>%
  select(Acronym, Hypothesis,Definition))

# Make acronyms as rownames
rownames(hyp_mat) <- hyp_mat$Acronym # OK


# Import conceptual scheme: themes and RQ ####

# Import matrices
rhrq_mat <- readr::read_csv("resources/additional data/RH-RQ.csv")
theme_rq_mat <- as.data.frame(readr::read_csv("resources/additional data/Theme-RQ.csv"),escape_double =FALSE)


# check and update acronyms of hypotheses in rhrq
rhrq_mat$Hypothesis <- hyp_mat$Acronym[ match( rhrq_mat$Hypothesis, hyp_mat$Old_acronym ) ]

# change rhrq into a dataframe contingency table
rhrq_mat <- as.data.frame(rhrq_mat)
rownames(rhrq_mat) <- rhrq_mat$Hypothesis
rhrq_mat <- rhrq_mat[ ,-1]

rhrq_mat[ ,theme_rq_mat$RQ_abb]
                      
                      
# check the names of RQ match
stopifnot(setequal(names(rhrq_mat),theme_rq_mat$RQ_abb))

