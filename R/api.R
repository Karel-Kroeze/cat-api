library(plumber)
source("./functions/mirtCAT_v2.R")
cat_data <- setup_cat_data()

#* @apiTitle Computerized Adaptive Testing
#* @apiDescription Provides functions for administering a computerized adaptive version of the xxx.

#* @plumber
function(pr) {
  pr$setApiSpec(jsonlite::read_json("../spec.json"))
}

#* @filter log
function(req, res) {
  cat(req$REMOTE_ADDR, "\t", req$REQUEST_METHOD, "\t", req$PATH_INFO, "\t", req$HTTP_USER_AGENT, "\n")

  plumber::forward()
}

#* @filter cors
# https://github.com/rstudio/plumber/issues/66#issuecomment-418660334
# effectively disable CORS, by allowing everything.
# TODO: whitelist domain if not the same, or ideally lock down CORS again if on the same host
function(req, res) {
  if(!is.null(req$HTTP_ORIGIN)) {
    cat("CORS ::", req$HTTP_ORIGIN, "::", req$REQUEST_METHOD, "\n")
    res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  } else {
    res$setHeader("Access-Control-Allow-Origin", "*")
  }
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
}

#* Record user response
#* @post /next_item
#* @get /next_item
#* @param prior:int integer in range [0,4], score on preliminary questions.
#* @param responses:array(int) array of responses, where each response is an object containing the item id, and - if applicable - the recorded response
#* @serializer unboxedJSON
function(prior, administered) {
  # TODO: figure out how to inject prior without making it a global
  .prior <<- set_prior(prior)
  cat <- run_cat(cat_data, .prior, administered)

  # calculate estimate
  estimate <- extract.mirtCAT(cat$person, "thetas") %>% drop()
  se <- extract.mirtCAT(cat$person, "thetas_SE") %>% drop()

  # if applicable, calculate next item
  if (nrow(administered) >= 12 || se <= .316) {
    result <- list(
      complete = TRUE,
      estimate = estimate,
      se = se
    )
  } else {
    result <- list(
      complete = FALSE,
      next_item = mirtCAT::findNextItem(cat),
      estimate = estimate,
      se = se
    )
  }

  invisible(result)
}

#* Create parameter estimate graph
#* @get /estimate_plot
#* @param estimate:double participant theta estimate as returned by `/next_item`
#* @param se:double() standard error of estimate, as returned by `/next_item`
#* @serializer svg
function(estimate, se) {
  
}