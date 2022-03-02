library(mirtCAT)

setupCAT <- function() {
  # Setting up item parameters and questions


  # Item bank and associated questions
  load("data/coef_AMPD.Rdata") # load item bank parameters
  questions <- read.table("data/SCID 5-AMPD questions.csv", # load questions, could leave this out
    sep = ";", header = T, quote = "\""
  )
  model <<- generate.mirt_object(coef_AMPD, itemtype = "graded") # mirt object from item parameters under GRM

  # create dataframe of question and responses for mirtCAT
  n_items <- nrow(coef_AMPD) # Number of items
  Options <- matrix(rep(0:4, n_items), ncol = 5, nrow = n_items, byrow = T) # Response options
  Questions <- rownames(coef_AMPD) # subdomain names
  resp_dat <<- data.frame(
    Question = Questions, Option = Options,
    Type = "radio", stringsAsFactors = FALSE
  )
}


runCAT <- function(resp_items, resp_answer) {

  # direct manipulation of internal objects using design_elements = TRUE
  # What to choose as starting item? Item 6 has good psychometric properties (high) and is currently selected
  # using MI criterion assuming theta = 0
  CATdesign <- mirtCAT(df = resp_dat, mo = model, criteria = "MI", design_elements = TRUE, start_item = "MI")

  # Actual item selection

  # update theta and se for items answered so far
  # updateDesign does not take a vector as input
  if (length(resp_items) > 1) { # skip and recommend start item if no items have been administered
    for (i in 1:length(resp_items)) {
      CATdesign <- updateDesign(CATdesign, new_item = resp_items[i], new_response = resp_answer[i])
    }
  }

  # Return theta estimate and se
  (theta_est <- extract.mirtCAT(CATdesign$person, "thetas"))
  (theta_se <- extract.mirtCAT(CATdesign$person, "thetas_SE"))

  # Return next item
  # findNextItem seems to be what we need here
  if (length(resp_items) < 12) {
    (next_item <- findNextItem(CATdesign)) # next item
  } else {
    next_item <- "complete"
  } # return "complete" if there are no more items

  return(next_item) # returns next item as integer
}
