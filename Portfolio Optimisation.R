#importing useful libraries

library("tseries")
library("PortfolioAnalytics")
library("quantmod")
library("Quandl")
library("DEoptim")
library('stats')

#reading of data

data = read.csv("10-Year Indian Bond Yield Historical.csv", header=T)
data$ï..date = as.Date(data$ï..date)
data = xts(data[,2:5], order.by = data$ï..date)

#bonds

bonds = data[,1]
mbonds=monthlyReturn(bonds, type='arithmetic')
colnames(mbonds) = "mbonds"
table.AnnualizedReturns(mbonds)
charts.PerformanceSummary(mbonds)

#stocks

stocks = data[,2]
mbse = monthlyReturn(stocks, type='arithmetic')
colnames(mbse)='mbse'
table.AnnualizedReturns(mbse)
charts.PerformanceSummary(mbse)


#int_bonds

ibonds = data[,3]
mibonds = monthlyReturn(ibonds, type='arithmetic')
colnames(mibond)='mibond'
table.AnnualizedReturns(mibonds)
charts.PerformanceSummary(mibonds)

#int_stocks

istocks = data[,4]
mistocks = monthlyReturn(istocks, type='arithmetic')
colnames(mistocks)='mistocks'
table.AnnualizedReturns(mistocks)
charts.PerformanceSummary(mistocks)

comp1=cbind(mbonds,mbse,mibonds,mistocks)
table.AnnualizedReturns(comp1)
charts.PerformanceSummary(comp1)
charts.PerformanceSummary(mistocks)

#int_stock

getSymbols("", src="yahoo", from="2007-01-02", to="2022-01-01")
istocks = data[,6]
mistocks = monthlyReturn(istocks, type='arithmetic')
colnames(mistocks)='mistocks'
table.AnnualizedReturns(mistocks)
charts.PerformanceSummary(mistocks)

comp1=cbind(mbonds,mbse,mibonds,mistocks)
table.AnnualizedReturns(comp1)
charts.PerformanceSummary(comp1)


# 1. Portfolio Optimization

mport = cbind(mbonds,mbse,mibonds,mistocks)

# 1.1. Naive Global Portfolio (Monthly Rebalancing)


mnaivew = as.numeric(t(c(0.25,0.25,0.25,0.25)))
names(mnaivew) = c("mbonds","mstocks","mibonds","mistocks")
mnaive = Return.portfolio(R=mport,weights=mnaivew,geometric=F,rebalance_on="months")
colnames(mnaive) = "mnaive"

# 1.2. Roche Global Portfolio (Monthly Rebalancing)

mrochew = as.numeric(t(c(0.24,0.18,0.33,0.25)))
names(mrochew) = c("mbonds","mstocks","mibonds","mistocks")
mroche = Return.portfolio(R=mport,weights=mrochew,geometric=F,rebalance_on="months")
colnames(mroche) = "mroche"

# 1.3. Bogle U.S. Portfolio (Monthly Rebalancing)

mboglew = as.numeric(t(c(0.40,0.60,0.00,0.00)))
names(mboglew) = c("mbonds","mstocks","mibonds","mistocks")
mbogle = Return.portfolio(R=mport,weights=mboglew,geometric=F,rebalance_on="months")
colnames(mbogle) = "mbogle"

benchcomp = cbind(mnaive,mroche,mbogle)
View(benchcomp)


# 1.4. Benchmark Portfolios Returns Comparison

table.AnnualizedReturns(benchcomp)
charts.PerformanceSummary(benchcomp)


# 1.5. Portfolio Optimization 

# 2. Mean Maximization 

# Portfolio Specifications
mport1c = portfolio.spec(assets = colnames(mport))

# Portfolio Constraints
mport1c = add.constraint(mport1c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport1c = add.constraint(mport1c,type="long_only")

# Portfolio Objectives
mport1c = add.objective(mport1c,type="return",name="mean")

# Portfolio Optimization
mportopt1 = optimize.portfolio(R=mport["::2013-12-31"],portfolio=mport1c,optimize_method="DEoptim",)
chart.Weights(mportopt1)

####
mport1w = as.numeric(mportopt1$weights)
names(mport1w) = c("mbonds","mstocks","mibonds","mistocks")
mport1 = Return.portfolio(R=mport["2014-01-31::"],weights=mport1w,geometric=F,rebalance_on="months")
colnames(mport1) = "mport1"

####

# 2.1. Standard deviation minimization

# Portfolio Specifications
mport2c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport2c = add.constraint(mport2c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport2c = add.constraint(mport2c,type="long_only")

# Portfolio Objectives
mport2c = add.objective(mport2c,type="risk",name="StdDev")

# Portfolio Optimization
mportopt2 = optimize.portfolio(R=mport["::2013-12-31"],portfolio=mport2c,optimize_method="DEoptim")
chart.Weights(mportopt2)

####
mport2w = as.numeric(mportopt2$weights)
names(mport2w) <- c("mbonds","mstocks","mibonds","mistocks")
mport2 <- Return.portfolio(R=mport["2014-01-31::"],weights=mport2w,geometric=F,rebalance_on="months")
colnames(mport2) <- "mport2"

####

# 2.1.1. Mean Maximization and Standard Deviation Minimization Portfolio

# Portfolio Specifications

mport3c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints

mport3c = add.constraint(mport3c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport3c = add.constraint(mport3c,type="long_only")

# Portfolio Objectives

mport3c = add.objective(mport3c,type="return",name="mean")
mport3c = add.objective(mport3c,type="risk",name="StdDev")

# Portfolio Optimization

mportopt3 = optimize.portfolio(R=mport["::2013-12-31"],portfolio=mport3c,optimize_method="DEoptim")
chart.Weights(mportopt3)

####
mport3w = as.numeric(mportopt3$weights)
names(mport3w) = c("mbonds","mstocks","mibonds","mistocks")
mport3 = Return.portfolio(R=mport["2014-01-31::"],weights=mport3w,geometric=F,rebalance_on="months")
colnames(mport3) = "mport3"

####

# 2.1.2. Mean Maximization Value at Risk (VaR) Minimization Portfolio

# Portfolio Specifications
mport4c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport4c = add.constraint(mport4c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport4c = add.constraint(mport4c,type="long_only")

# Portfolio Objectives
mport4c = add.objective(mport4c,type="return",name="mean")
mport4c = add.objective(mport4c,type="risk",name="VaR",arguments=list(p = 0.99,method="modified"))

# Portfolio Optimization
mportopt4 = optimize.portfolio(R=mport["::2013-12-31"],portfolio=mport4c,optimize_method="DEoptim")
chart.Weights(mportopt4)


#####
mport4w = as.numeric(mportopt4$weights)
names(mport4w) <- c("mbonds","mstocks","mibonds","mistocks")
mport4 <- Return.portfolio(R=mport["2014-01-31::"],weights=mport4w,geometric=F,rebalance_on="months")
colnames(mport4) <- "mport4"

#####
#Comparision

comp3 = cbind(mnaivew,mport1w,mport2w,mport3w,mport4w)
comp3

# 2.1.3. Optimized Portfolios Backtesting Comparison

mportcomp = cbind(mnaive["2014-01-31::"],mport1,mport2,mport3,mport4)
table.AnnualizedReturns(mportcomp)
charts.PerformanceSummary(mportcomp)
