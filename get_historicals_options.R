require("RobinHood");require("httr");require("jsonlite");require("quantmod")

mod_json <- function(x, type) {
  
  if (type == "toJSON") {
    x <- x %>% jsonlite::toJSON()
    x <- substr(x, 2, nchar(x) - 1)
    return(x)
  }
  
  if (type == "fromJSON") {
    x <- jsonlite::fromJSON(rawToChar(x$content))
    return(x)
  }
  
}

chain_symbol = "VXX"
type = "put" # put
expiration = "2021-03-05"
strike_prc = 14.5
# interval =  '5minute'|'10minute'|'hour'|'day'|'week'
interval = 'hour'
# *************************************************************************************************************
#                                           Wrapper
# *************************************************************************************************************
get_historicals_options = function(chain_symbol,interval,type,expiration,strike_prc,username,password){
  # establish RH connection
  RH = RobinHood(username = username, password = password)
  # URL and token
  url = paste0("https://api.robinhood.com/options/instruments/",
               "?state=active",
               "&type=", type,
               "&chain_symbol=", chain_symbol,
               "&strike_price=", strike_prc)
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET data
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # turn strike to numeric value
  dta$strike_price = as.numeric(dta$strike_price)
  
  # subset to desired expiration
  op = subset(dta, dta$expiration_date == expiration)
  
  # interval =  '5minute'|'10minute'|'hour'|'day'|'week'
  hist = paste0("https://api.robinhood.com/marketdata/options/historicals/",op$id,
                "/?interval=",interval)

  # GET call
  dta2 <- GET(hist,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  # logout 
  logout(RH)
  
  # format return
  dta2 <- mod_json(dta2, "fromJSON")
  dta2 = dta2$data_points
  
  # format Time Stamp
  dta2$begins_at <- as.POSIXct(as.character(dta2$begins_at), 
                               format="%Y-%m-%dT%H:%M:%SZ",TZ="UTC")
  
  # add columns for strike + symbol + expiration + type
  dta2$strike_prc  <- strike_prc
  dta2$expiration  <- expiration
  dta2$type  <- type
  dta2$underlying_symbol <- chain_symbol
  
  # return data
  dta2
}
# *************************************************************************************************************
#                                           Examples
# *************************************************************************************************************
op = get_historicals_options(chain_symbol = "AAPL", interval = "hour", type="call",
                             expiration = "2021-03-12",strike_prc = 122, 
                             username = 'username',password = 'password')

op = get_historicals_options(chain_symbol = "SPY", interval = "5minute", type="call",
                             expiration = "2021-06-30",strike_prc = 380, 
                             username = 'username',password = 'password')

op = get_historicals_options(chain_symbol = "TSLA", interval = "day", type="put",
                             expiration = "2021-03-26",strike_prc = 650, 
                             username = 'username',password = 'password')
# *************************************************************************************************************
#                                             Plot
# *************************************************************************************************************
# prices 2 numeric
op$open_price = as.numeric(op$open_price)
op$high_price = as.numeric(op$high_price)
op$low_price = as.numeric(op$low_price)
op$close_price = as.numeric(op$close_price)
# convert to xts
op_xts = as.xts(op[,c("open_price","high_price","low_price","close_price")], 
                order.by = as.POSIXct(op$begins_at))
# format column names
colnames(op_xts) = paste0(unique(op$underlying_symbol),
                          c(".Open",".High",".Low",".Close"))

# plot
chartSeries(op_xts,name = paste0(unique(op$underlying_symbol),
                                 "-",unique(op$strike_prc),
                                 "-",toupper(unique(op$type)),
                                 "--EXP: ",unique(op$expiration)))
addRSI()
addEMA(n=20,col="green")
addEMA(n=50,col="red")
