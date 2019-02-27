###############################
## Imports
###############################
imports <- c('dplyr', 'plyr', 'GoogleAnalyticsR', 'rio', 'RcppAlgos', 'data.table')
invisible(lapply(imports, require, character.only = TRUE))
###############################
## Helper Functions
###############################
UsersWide2Long <- function(x){
  # Transform 'wide' web data to 'long' web data. The API Query will return multiple users per path
  # This function replicates rows by the number of users that have taken a path then removes the user
  # column from the long data.
  # Args:
    # x: A dataframe returned by the google_analytics_4 function
  # Returns: long data.table
  dt <- data.table(x) # Retype x as a data.table object
  dt <- dt[rep(seq(1,nrow(dt)), da$users)] #replicate rows by number of users
  dt$users <- NULL # Remove users column
  dt
}
PathCombinations <- function(x){
  # Generate all combinations of a path without repition
  # This is a convinience wrapper function for the comboGeneral function
  data.frame(comboGeneral(v = x, m = 2, repetition = FALSE), stringsAsFactors = FALSE)
}
###############################
## Authentication and API Request
###############################
ga_auth() #Autorization for access to Google Analytics Account
# Google Analytics API Data Request
# Observe the metrics and diminesions needed for analyzing page relations
my_id <- 'your_id_goes_here' # Put your Google Analytics Account ID Here
dates <- c('2018-21-01', '2018-12-31') # Edit the date range to your preference
web_data <- google_analytics_4(my_id,
                                date_range = dates,
                                metrics = c("users"),
                                dimensions = c("landingPagePath",
                                               "secondPagePath",
                                               "exitPagePath",
                                               "previousPagePath"),
                                anti_sample = TRUE
)
###############################
## Data Priming and Output
###############################
y <- UsersWide2Long(web_data)
output <- apply(y, 1, PathCombinations) #apply to data.table, will return list of data frames
output <- rbindlist(output) #bind the dataframes in the list together
output <- plyr::count(output) #produce data frame with rows displaying frequencies of 2 pages occuring in the same path


# Remove rows where:
# 1. the same page is related to itself
# 2. a page is an enterance
# 3. a page is the homepage
# 4. a page is (not set)
output_subset = output %>%
  select(X1, X2, freq) %>%
  filter(X1 != X2) %>%
  filter(X1 != "(entrance)")%>%
  filter(X1 != "/") %>%
  filter(X1 != "(not set)")

# Order rows in descending order
output_subset <- output_subset[order(-output_subset$freq), ]

# Export page relations to csv
export(output_subset, "page_relations.csv")
