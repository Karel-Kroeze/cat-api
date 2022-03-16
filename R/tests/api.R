library(httr)
setwd("..")
source("functions/mirtCAT_v2.R")

# assume api is running on localhost
HOST <- "http://localhost"
PORT <- 4321

# live test version
# HOST <- "https://cat-api.home.karel-kroeze.nl"
# PORT <- NULL

PRIOR <- 0
PATTERN <- data.frame(
  item_id = c(3, 8),
  response = c(2, 1)
)

# get estimate and next_item from API
response <- httr::POST(
  HOST,
  port = PORT, path = "next_item", httr::content_type("application/json"),
  body = list(
    prior = PRIOR,
    administered = PATTERN
  ),
  encode = "json"
)
httr::content(response)

# get estimate 'directly' (ish)
data <- setup_cat_data()
.prior <- set_prior(PRIOR)
cat <- run_cat(data, .prior, PATTERN)
cat$person$thetas
mirtCAT::findNextItem(cat)