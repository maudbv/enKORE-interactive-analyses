# Helper functions for the hi knowledge shiny dashboard

.detect_items <- function(obj, items) {
  #Function to detect the presence or absence of at least one or more items in a vector or list
  # useful to filter a table based on matches to multiple items
  # obj : a list, or a vector in which we are trying to detect items
  # items : a vector or single value of the items to detect
  # return: logical value, TRUE if at least one item is detected in the list, FALSE if not.
  y <- NULL
  if (is.null(obj)) y <- NULL
  if (is.list(obj)) {
    y <-  unlist(lapply(obj, FUN = function(x) sum(items %in% x)>0))
  }
  if (is.vector(obj)) {
    y <-  unlist(sapply(obj, FUN = function(x) sum(items %in% x)>0))
  }
  return(y)
}

