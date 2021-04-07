#geocoding using GeoAdmin REST API
#BF 18.03.2021

#FAIR USE limits: https://www.geo.admin.ch/de/geo-dienstleistungen/geodienste/terms-of-use.html
#20 requests per minute!!! 28'800 per day!
#comment function geocoding:
#returns data frame with results
#address = string with all address information
#sleep = delay, if set to 3 seconds (default) not more than 20 requests per minute => FAIR USE regulations of API
#gives several addresses if not precise (the lower the weight, the better the match)
library(jsonlite)
library(httr)

geocoding <- function(address, sleep=3){
  if(is.character(address)==FALSE){
    address <- as.character(address) 
  } 
  if(sleep<3) warning('too many requests per minute')
  path="https://api3.geo.admin.ch/rest/services/api/SearchServer?"
  request <- GET(url = path, 
                 query = list(
                   searchText = address,
                   origins= "address",
                   type = "locations")
  )
  response <- content(request, as = "text", encoding = "UTF-8")
  #if no result give warning
  if(response == "{\"fuzzy\":\"true\",\"results\":[]}") warning(paste0("no result found for ", address))
  #if no result return empty data frame
  if (response == "{\"fuzzy\":\"true\",\"results\":[]}"){
    df <- data.frame()
  } else {
    df <- data.frame(fromJSON(response, flatten = TRUE)) 
    df$status_code <- status_code(request) #200=all fine
    df$get_request <- address
    #delete fuzzy column if exists
    if("fuzzy" %in% colnames(df))
    {
      df$fuzzy <- NULL;
    }
  }
  Sys.sleep(sleep)
  return(df)
}