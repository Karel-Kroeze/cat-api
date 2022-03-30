library(plumber)
library(magrittr)
library(ggplot2)
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
  cat(
    req$REMOTE_ADDR,
    "\t",
    req$REQUEST_METHOD,
    "\t",
    req$PATH_INFO,
    "\t",
    req$HTTP_USER_AGENT,
    "\n"
  )

  plumber::forward()
}

#* @filter cors
# https://github.com/rstudio/plumber/issues/66#issuecomment-418660334
# effectively disable CORS, by allowing everything.
# TODO: whitelist domain if not the same, or ideally lock down CORS again if on the same host
function(req, res) {
  if (!is.null(req$HTTP_ORIGIN)) {
    cat("CORS ::", req$HTTP_ORIGIN, "::", req$REQUEST_METHOD, "\n")
    res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  } else {
    res$setHeader("Access-Control-Allow-Origin", "*")
  }

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader("Access-Control-Allow-Headers",
                  req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
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
#* @param responses:int array of responses, where each response is an object containing the item id, and - if applicable - the recorded response
#* @serializer unboxedJSON
function(prior, administered) {
  # TODO: figure out how to inject prior without making it a global
  .prior <<- set_prior(prior)

  # cast administered as a data.frame even if the list is empty
  administered <- as.data.frame(administered)

  # create  cat design, then update with administered items
  cat <- run_cat(cat_data, .prior, administered)

  # calculate estimate
  estimate <- extract.mirtCAT(cat$person, "thetas") %>% drop()
  se <- extract.mirtCAT(cat$person, "thetas_SE") %>% drop()

  # if applicable, calculate next item
  if (nrow(administered) >= 12 || se <= .316) {
    result <- list(complete = TRUE,
                   estimate = estimate,
                   se = se)
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
#* @get /normal_estimate_plot.svg
#* @param estimate:double participant theta estimate as returned by `/next_item`
#* @param se:double standard error of estimate, as returned by `/next_item`
#* @serializer svg list(bg = "transparent")
function(estimate,
         se,
         precision = .01,
         xmin = -3,
         xmax = 3) {
  estimate <- as.numeric(estimate)
  se <- as.numeric(se)
  precision <- as.numeric(precision)
  range <- c(as.numeric(xmin), as.numeric(xmax))
  cat(estimate, se, precision, range)

  theta <- seq(min(-3, range[1]), max(3, range[2]), by = precision)
  population <- dnorm(theta)
  ci_estimate <- estimate + c(-2, 2) * se
  y_range <- dnorm(ci_estimate)
  y_min <- min(y_range) - 0.02
  y_max <- max(y_range) + 0.02
  area <-
    data.frame(x = c(rep(ci_estimate, each = 2), ci_estimate[1]),
               y = c(y_min, y_max, y_max, y_min, y_min))


  p <- ggplot(data.frame(theta, population)) +
    geom_polygon(aes(theta, population), fill = "#4091d3", alpha = .4) +
    geom_polygon(aes(x, y),
                 data = area,
                 fill = "#50ac4d",
                 alpha = .4) +
    # geom_path(aes(x, y), data = area, linetype = 4, colour = "#50ac4d") +
    geom_segment(
      x = estimate,
      xend = estimate,
      y = y_min,
      yend = y_max,
      linetype = 2,
      colour = "#50ac4d"
    ) +
    coord_cartesian(ylim = c(0.01, 0.6),
                    xlim = range,
                    expand = FALSE) +
    theme_minimal() +
    labs(x = "Score") +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_rect(fill = "#ffffff00", colour = "#ffffff00"),
      axis.line = element_blank(),
      axis.line.x.bottom = element_line(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      plot.background = element_rect(fill = "#ffffff00", colour = "#ffffff00")
    )

  print(p)
}


#* Create parameter estimate graph
#* @get /estimate_plot.svg
#* @param estimate:double participant theta estimate as returned by `/next_item`
#* @param se:double standard error of estimate, as returned by `/next_item`
#* @serializer svg list(bg = "transparent")
function(estimate,
         se,
         cutoffs = c(-1.5,-.4, .54, 1.83),
         range = c(-3, 3),
         points = 20,
         background = "transparent",
         stroke_colour = "#2F4CD4",
         fill_colour = "#7080EB") {
  # cast to numerics
  estimate <- as.numeric(estimate)
  se <- as.numeric(se)
  cutoffs <- as.numeric(cutoffs)
  range <- as.numeric(range)
  points <- as.numeric(points)

  # Plot parameters
  points_y <-
    qnorm(ppoints(points), estimate, se) # Figure out locations for the dots

  # Create plot
  p <- ggplot() +
    coord_cartesian(ylim = range, expand = FALSE) +
    geom_hline(yintercept = cutoffs,
               color = "white",
               size = .5) +
    geom_dotplot(
      aes(y = points_y, x = 0),
      fill = fill_colour,
      color = stroke_colour,
      dotsize = 1.1,
      width = 1.0,
      binpositions = "bygroup",
      binwidth = .25,
      binaxis = "y",
      stackdir = "center"
    ) +
    theme(
      panel.background = element_rect(fill = background, colour = "transparent"),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = "transparent")
    )

  print(p)
}
