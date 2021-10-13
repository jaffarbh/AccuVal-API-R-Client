# AccuVal API R Client
# This client, along with the accompanying "Property_Samples.csv" are intended as a guide to help developers build their own clients.
# They are provided without any warranty. Use at your own risk.
# For help, email jaafar@reexm.com
# 2021 Jaafar Almusaad. All Rights Reserved

# Note: if you run this program from RStudio, you may need to set the Working Directory to the R file:
# Session -> Set Working Directory -> To Source File Location

# Last updated 12/10/2021

##################################################################################################################################
##################################################################################################################################

# Pre-load required libraries
# To install a missing package, use install.packages(package_name")
library(httr)
library(data.table)
library(doParallel)
library(foreach)
library(dplyr)
library(jsonify)
library(stringr)

#  EDIT THE LINE BELOW AND REPLACE WITH YOUR API KEY
apikey <- "replace-with-your-api-key"

# Set API base URL
URL <- "https://accuval.co.uk/api/"

# Read sample properties file. Searched in the same directory as this R file
# NOTE: the provided file contains invalid samples to show how the API reacts. The return price is -1 for these cases
Properties <- fread("Property_Samples.csv")

# Create in-machine cluster for async, parallel requests
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Capture current time
t1 <- Sys.time()

# Build API requests and send in parallel. Results are accumulated in "Valuations" object
Valuations <- foreach(i = 1:nrow(Properties) , .combine = 'rbind', .packages=c('foreach', 'data.table', 'dplyr', 'httr', 'jsonify', 'stringr')) %dopar% {
  
  # Get URI body elements from the sample file
  postcode <- as.character(Properties[i,"postcode"])
  type <- as.character(Properties[i,"type"])
  rooms <- as.character(Properties[i,"rooms"])
  area <- as.character(Properties[i,"area"])
  epc <- as.character(Properties[i,"epc"])
  age <- as.character(Properties[i,"age"])
  uuid <- as.character(Properties[i,"uuid"])
  
  # Paste together
  BODY <- paste0("?", "postcode=", postcode, "&type=", type, "&rooms=", rooms, "&area=", area, "&epc=", epc, "&age=", age)
  
  # Replace whitespaces wihth %20 in the request body. Important for HTML
  BODY <- str_replace_all(BODY, " ", "%20")
  
  # Send GET request to the API
  try(Res <- GET(url = paste0(URL, BODY), add_headers(.headers = c('apikey' = apikey))))
  
  # Check if a response is received within API default timeout 
  
  ifelse(exists("Res"),
         {
           # Get status code from the response
           Status <- Res$status_code
           
           # If failuer (other than 200), set price to -1 and confidence to NA
           ifelse(Status != 200, {
             PriceConfidence  <- data.table(price=-1, confidence="NA")
           },
           {
             # if success (200), retrieve the price from "content" 
             Cont <- content(Res, as = "text", encoding = "UTF-8")
             PriceConfidence <- from_json(Cont, simplify = TRUE)
             PriceConfidence <- data.table(PriceConfidence[, c("price", "confidence")]) })
         },
         {
           # If no response is received (i.e. timeout), set price to -1 and confidence to NA
           PriceConfidence  <- data.table(price=-1, confidence="NA")
         })
  
  # Retuen Price and Confidence
  PriceConfidence
}

# Capure finishing time
t2 <- Sys.time()

# Terminate the cluster
stopCluster(cl)

# Get the price and confidence columns from Valuations object and attach to Properties
Properties <- cbind(Valuations[, c(1, 2)], Properties)

# Done with Valuations object so delete it from memory
rm(Valuations)

# Print combined result on the console
print(Properties)

# Print total time and valuation rate (per second) on the console
print(paste("Total time:", round(difftime(t2, t1, units = "secs")), "second(s)"))
print(paste("API Throuput:", round(nrow(Properties) / as.numeric(difftime(t2, t1, units = "secs"))), "valuations /second"))

# Uncomment the line below to write to the valuations to a CSV file "Valuations.csv" under the same directory
#fwrite(Properties, "Valuations.csv")