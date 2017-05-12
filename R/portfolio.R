portfolio <- function(assetReturns, targetReturn)
{
# Arguments:
# assetReturns - multivariate data set of asset returns
# target Return - the portfolios target return
# 1 Create Portfolio Settings:
nAssets = ncol(assetReturns)
Dmat = cov(assetReturns)
dvec = rep(0, times=nAssets)
Amat = t(rbind(
Return=colMeans(assetReturns),
Budget=rep(1, nAssets),
LongOnly=diag(nAssets)))
bvec = c(
Return=targetReturn,
budget=1,
LongOnly=rep(0, times=nAssets))
meq = 2
# 2 Optimize Weights:
portfolio = solve.QP(Dmat, dvec, Amat, bvec, meq)
weights = round(portfolio$solution, digits = 4)
names(weights) = colnames(assetReturns)
# Return Value:
list(
weights = 100*weights,
risk = portfolio$value,
return = targetReturn)
}
calculateWeights <- function(ticker_symbol, dateFrom,dateTo)
{
ETF_Data <- new.env()
prices = getSymbols(ticker_symbol,env=ETF_Data, from = dateFrom, to = dateTo)
ETF_Adj_Data <- do.call(merge, eapply(ETF_Data, Ad))
Monthly_ETF_Adj_Data <- ETF_Adj_Data[endpoints(ETF_Adj_Data,'months')]
returns <- Return.calculate(Monthly_ETF_Adj_Data)
assetReturns=returns[-c(1)]
targetReturn <- mean(colMeans(assetReturns))
portfolio <- portfolio(assetReturns, targetReturn)
portfolio
}