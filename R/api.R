library(plumber)

# #* @filter log
# function(req, res) {
#     print(req)
#     forward()
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
function(prior, administered) {
  print(prior)
  print(administered)
  str(administered)
}
