setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
source("mirtCAT function+prior.R")

# Simulate some random response vectors for testing
n_items <- 12
id <- 1:1000
testdata <- vector("list",length(id))
for(i in id){
  testdata[[i]]$global_score <- sample(0:4,1)
  testdata[[i]]$resp_items <- sample(1:n_items, sample(0:n_items)) # Random selection of items
  testdata[[i]]$resp_answer <- sample(0:4,length(testdata[[i]]$resp_items),replace = T) # Random reponses 0:4
}



# Function to load item parameters and questions
setupCAT()

# Run the function for all simulees
# May give convergence error warning when very specific prior is set (global score = 4)
# and many items are administered
start <- Sys.time()
item <- c()
for(i in id){
  setprior(testdata[[i]]$global_score)
tryCatch({testdata[[i]]$item <- runCAT(testdata[[i]]$resp_items,testdata[[i]]$resp_answer)}, 
         warning=function(w) print(i))
}
end <- Sys.time()

end - start

# Check if suggested item has not been administered
!all(sapply(testdata, function(x) x$item %in% x$resp_items))
