# setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))

# Using examples from mirtCAT
# Create customUpdateThetas function to include a prior distribution N(mu, sigma)
# customUpdateThetas: https://www.rdocumentation.org/packages/mirtCAT/versions/1.7/topics/mirtCAT
# customNextItem: http://philchalmers.github.io/mirtCAT/html/sim-unidimensional_custom.html
# fscores: https://rdrr.io/cran/mirt/man/fscores.html

library(mirt)
library(mirtCAT)

# Custom update theta function to take prior distribution
custom_density_function_prior <- function(Theta, prior, ...) {
  dnorm(Theta, mean = prior$mean, prior$sd)
}

# mirtCAT,customUpdateThetas function, utilizing prior
custom_update_thetas_prior <- function(design, person, test) {
  mo <- extract.mirtCAT(test, "mo") # Extract CATirt model
  responses <- extract.mirtCAT(person, "responses") # extract response patterns

  # Estimate theta using prior custom density
  adjusted_scores <- fscores(mo,
    response.pattern = responses,
    custom_den = custom_density_function_prior,
    prior = prior,
    method = "MAP"
  )

  # Update theta estimate and SE
  person$Update_thetas(
    adjusted_scores[, "F1"],
    adjusted_scores[, "SE_F1", drop = FALSE]
  )

  invisible()
}


# Setting prior based on global score
# Prior values for each global score based on empirical data (Frans et al., 2022)
set_prior <- function(global_score, prior = list()) {
  prior$mean <- c(-2.4, -1.0, 0, 0.9, 2.5)[global_score + 1] # prior mean
  prior$sd <- c(.800, .599, .583, .773, .500)[global_score + 1] # prior sd
  prior
}

setup_cat_data <- function() {
  # Setting up item parameters and questions

  # Item bank and associated questions
  load("../data/coef_AMPD.Rdata") # load item bank parameters
  questions <- read.csv2("../data/SCID 5-AMPD questions.csv") # load questions, could leave this out
  model <- generate.mirt_object(coef_AMPD, itemtype = "graded") # mirt object from item parameters under GRM

  # create dataframe of question and responses for mirtCAT
  n_items <- nrow(coef_AMPD) # Number of items
  options <- matrix(rep(0:4, n_items), ncol = 5, nrow = n_items, byrow = T) # Response options
  question_names <- rownames(coef_AMPD) # subdomain names

  return(list(model = model, questions = questions, question_names = question_names, options = options))
}

create_person_response_data <- function(data){
  data.frame(
    Question = data$question_names,
    Option = data$options,
    Type = "radio",
    stringsAsFactors = FALSE
  )
}

run_cat <- function(data, prior, responses) {
  # direct manipulation of internal objects using design_elements = TRUE
  # thetas.start sets starting theta value = prior_mean
  # customupdatethetas sets function with different prior distribution

  design <- list(
    thetas.start = prior$mean,
    customUpdateThetas = custom_update_thetas_prior
  )

  person_response_data <- create_person_response_data(data)

  # run CAT with prior
  # Prior SE doesn't seem to work without setting local_pattern
  cat_instance <- mirtCAT(
    df = person_response_data, mo = data$model, start_item = "Trule",
    method = "MAP", criteria = "MI", design = design,
    design_elements = TRUE
  )

  # update theta and se for items answered so far
  # updateDesign does not take a vector as input
  # Does allow missing data in resp_answer
  if (nrow(responses) > 1) { # skip and recommend start item if no items have been administered
    for (i in seq_len(nrow(responses))) {
      if (!is.null(responses[i, "response"]) &&
        !is.na(responses[i, "response"])) {
        cat_instance <- updateDesign(cat_instance, new_item = responses[i, "item_id"], new_response = responses[i, "response"])
      }
    }
  }

  invisible(cat_instance)

  # # Return theta estimate and se
  # # Unfortunately theta_se is returned as 1 if no items are administered
  # # Since the prior is set in updateDesign. Prior is considered in subsequent estimates
  # # and correct starting items are selected
  # (theta_est <- extract.mirtCAT(cat$person, "thetas"))
  # (theta_se <- extract.mirtCAT(cat$person, "thetas_SE"))


  # # Return next item
  # # findNextItem seems to be what we need here
  # if (nrow(responses) < 12) {
  #   next_item <- findNextItem()
  #   findNextItem(CATdesign)) # next item
  # } else {
  #   next_item <- "complete"
  # } # return "complete" if there are no more items

  # return(next_item) # returns next item as integer
}
