
#calculateWeights(c("IBM","AAPL","QCOM","CAT"),"2016-01-01","2017-01-05","months")
# Valid period for the argument on include: "days", "weeks", "months", "quarters", and "years".
calculateWeights <- function(ticker_symbol, dateFrom,dateTo,period)
{
ETF_Data <- new.env()
prices = getSymbols(ticker_symbol,env=ETF_Data, from = dateFrom, to = dateTo)
ETF_Adj_Data <- do.call(merge, eapply(ETF_Data, Ad))
Monthly_ETF_Adj_Data <- ETF_Adj_Data[endpoints(ETF_Adj_Data,period)]
returns <- Return.calculate(Monthly_ETF_Adj_Data)
assetReturns=returns[-c(1)]
targetReturn <- mean(colMeans(assetReturns))
portfolio <- portfolio(assetReturns, targetReturn)
portfolio
}
