library(plumber)
source("./functions/mirtCAT_v2.R")

cat_data <- setup_cat_data()
#
# #* @filter log
# function(req, res) {
#   print(req$location)
# }

#* @apiTitle Computerized Adaptive Testing
#* @apiDescription Provides functions for administering a computerized adaptive version of the xxx.

#* @plumber
function(pr) {
  pr$setApiSpec(jsonlite::read_json("../spec.json"))
}

#* Record user response
#* @post /next_item
#* @param prior:int integer in range [0,4], score on preliminary questions.
#* @param responses:array(int) array of responses, where each response is an object containing the item id, and - if applicable - the recorded response
#* @serializer unboxedJSON
function(prior, administered) {
  prior <- set_prior(prior)
  cat <- run_cat(cat_data, prior, administered)

  # calculate estimate
  estimate <- extract.mirtCAT(cat$person, "thetas") %>% drop()
  variance <- extract.mirtCAT(cat$person, "thetas_SE") %>% drop()

  # if applicable, calculate next item
  if (nrow(administered) >= 12) {
    result <- list(
      complete = TRUE,
      estimate = estimate,
      variance = variance
    )
  } else {
    result <- list(
      complete = FALSE,
      next_item = mirtCAT::findNextItem(cat),
      estimate = estimate,
      variance = variance
    )
  }

  invisible(result)
}
