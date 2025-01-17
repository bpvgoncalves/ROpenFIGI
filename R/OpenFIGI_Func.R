#' Generate a Sample data.frame for FIGI request
#' @return a data.frame of sample request
#' @export
sampleOpenFIGIdf <- function(){
  data.frame(idType=c("ID_ISIN","ID_WERTPAPIER"),idValue=c("US4592001014","851399"),
             exchCode=c(NA,"US"),currency=NA,micCode=NA,stringsAsFactors = F)
}

#' Generate FIGI Supported Identifiers
#' @return a char vector
#' @export
showFIGIIDType <- function(){
  c("ID_ISIN","ID_BB_UNIQUE","ID_SEDOL","ID_COMMON","ID_WERTPAPIER","ID_CUSIP","ID_BB","ID_ITALY",
    "ID_EXCH_SYMBOL","ID_FULL_EXCHANGE_SYMBOL","COMPOSITE_ID_BB_GLOBAL","ID_BB_GLOBAL_SHARE_CLASS_LEVEL",
    "ID_BB_SEC_NUM_DES","ID_BB_GLOBAL","TICKER","ID_CUSIP_8_CHR","OCC_SYMBOL","UNIQUE_ID_FUT_OPT",
    "OPRA_SYMBOL","TRADING_SYSTEM_IDENTIFIER")
}

#' OpenFIGI main function
#' @param input input data.frame or json, and others that can be feed into toJSON
#' @param apikey your API key
#' @param openfigiurl Bloomberg's OpenFIGI URL, please see https://openfigi.com/api
#' @param preferdf if only supply 1 input, do you prefer to see the data.frame directly
#' @param proxy if needed, web proxy information can be passed to the function.
#'   Accepts: NULL (default) = No proxy,   "auto" = Tries to auto detect a suitable proxy or an
#'   object created by httr::use_proxy().
#' @return a list of data.frame, of a data.frame if preferdf=T and only 1 request
#' @examples
#' \dontrun{
#'   figirst = OpenFIGI(sampleOpenFIGIdf())
#' }
#' @export
OpenFIGI <- function(input,
                     apikey = NULL,
                     openfigiurl = "https://api.openfigi.com/v1/mapping",
                     preferdf = FALSE,
                     proxy = NULL){

  if(is.null(apikey)){
    h <- httr::add_headers(
      "Content-Type" = "text/json"
    )
  } else {
    h <- httr::add_headers(
      "Content-Type" = "text/json",
      "X-OPENFIGI-APIKEY" = apikey
    )
  }

  if(inherits(input, "json")){
    myjson <- input
  } else {
    myjson <- jsonlite::toJSON(input)
  }

  if(is.null(proxy)){
    # No proxy used
    req <- httr::POST(openfigiurl, h, body = myjson)
  } else {
    if (!inherits(proxy, "request")) {
      if (tolower(proxy) == "auto") {
        proxy <- httr::use_proxy(curl::ie_get_proxy_for_url(openfigiurl),
                                 auth = "ntlm")
      } else {
        stop("Invalid proxy input. Expected: NULL, \"auto\" or httr::use_proxy() object.")
      }
    }
    req <- httr::POST(openfigiurl, config = c(h, proxy), body = myjson)
  }

  if(as.integer(req$status_code)!=200L){
    ## has invalid return code.
    warning(paste0("Got return code ",req$status_code," when POST json request.\n"))

    return(NULL)
  }
  jsonrst <- httr::content(req, as = "text")

  jsonrst <- jsonlite::fromJSON(jsonrst)


  jsonrst <- jsonrst[["data"]]

  ## now, jsonrst has all result, as a list.
  if(preferdf && length(jsonrst) == 1L) return(jsonrst[[1L]])

  jsonrst
}

#' Create mapping table out of OpenFIGI
#' @details assiging ISIN, CUSIP and other columns needed. Requires dplyr package to bind rows.
#' @param input input data.frame or json, and others that can be feed into toJSON
#' @param apikey your API key
#' @param openfigiurl Bloomberg's OpenFIGI URL, please see https://openfigi.com/api
#' @param additioncols additional columns you would like to include in the data.frame
#' @param proxy if needed, web proxy information can be passed to the function.
#'   Accepts: NULL (default) = No proxy,   "auto" = Tries to auto detect a suitable proxy or an
#'   object created by httr::use_proxy().
#' @return a data.frame
#' @examples
#' \dontrun{
#'   figirst = OpenFIGI_MappingCreator(sampleOpenFIGIdf())
#' }
#' @export
OpenFIGI_MappingCreator <- function(input,
                                    apikey=NULL,
                                    openfigiurl = "https://api.openfigi.com/v1/mapping",
                                    additioncols = c("ID_ISIN","ID_BB_UNIQUE","ID_SEDOL","ID_COMMON",
                                                     "ID_WERTPAPIER","ID_CUSIP","ID_BB","ID_ITALY",
                                                     "ID_EXCH_SYMBOL","ID_FULL_EXCHANGE_SYMBOL",
                                                     "COMPOSITE_ID_BB_GLOBAL","ID_BB_GLOBAL_SHARE_CLASS_LEVEL",
                                                     "ID_BB_SEC_NUM_DES","ID_BB_GLOBAL","TICKER",
                                                     "ID_CUSIP_8_CHR","OCC_SYMBOL","UNIQUE_ID_FUT_OPT",
                                                     "OPRA_SYMBOL","TRADING_SYSTEM_IDENTIFIER"),
                                    proxy = NULL){


  jsonrst <- OpenFIGI(input, apikey, openfigiurl, FALSE, proxy)

  if(is.null(jsonrst) || length(jsonrst)==0)return(NULL)

  stopifnot(length(jsonrst) == nrow(input))

  .f <- function(i){
    if(is.null(jsonrst[[i]])) return(NULL)
    temprst <- OpenFIGI_assignadditionalcols(jsonrst[[i]],additioncols)
    if(!is.na(input[["idType"]][i])){
      if(any(input[["idType"]][i]==additioncols)){
        temprst[[input[["idType"]][i]]] <- input[["idValue"]][i]
      }
    }
    temprst
  }
  finalrst <- dplyr::bind_rows(lapply(1:length(jsonrst), .f))
  finalrst
}

#' Assign addition cols
#' @param rst1 a result list
#' @param additioncols additional columns you would like to include in the data.frame
#' @details internal function
#' @return a data.frame with new columns
OpenFIGI_assignadditionalcols <- function(rst1,additioncols){
  for(i in additioncols){
    rst1[[i]] <- NA_character_

  }
  rst1
}
