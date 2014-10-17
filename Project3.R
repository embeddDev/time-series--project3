#TASK 1 - project 3

############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################


# -------Brain storming excersice (10%)-------
# Identify causal variables and hypothize an econometric structural equation for the following variables;
# . Stock price index, for your home country,

#OMX =  beta0 -ISL_GENGI * beta1 + epsilon
#Heakandi kronu gengi laekkar hlutabrefavisitoluna, thi staerstu felogin gera upp i erlendum gjaldeyri.
#haegt ad nalgast upplysingar t.d. hja gamma.is

# . Scandinavian electricity market(Nordpool) or the electrical market of your home country(for foreign
#                  students only, as there is no spot market in Iceland),

#NORDPOOL = beta0 - ?tihitastig*beta1 + epsilon

# haerra utihitastig laekkar eftirspurn eftir rafmagni, vedurupplysingar eru audvelt ad nalgast

# . Inflation.
#INFLATION = beta0 + styrivextir * beta1 + VSK*beta2 + epsilon
# haerri styrivextri og haekkandi VSK h?kkar verdbolgu, g?gn til hja t.d. hagstofu


# -------Housing Economics (40%)--------

Data = read.csv("hpi.csv", header=TRUE, sep= ";",dec=",")

#TASK 1

# Construct a structural equation for house prices. Theorize on the expected sign of the parameter
# estimates on each of the variables. 
#ANS: 

#Does it make sense to use all of the data? Theorize, 

#ANS: NO, some of the variable contain very similar information, and should not improve the model very much due to cross correlation.

#which data to use and explain your data selection.
# 
#ANS: 

# TASK 2 
#Plot the relationship between house prices and leasing over time. Theorize on the relationship 
# between the two variables. Is there an endogenous relationship between the two? 

plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     type="l",
     main="Comparison",
     xlab="Time",
     ylab="House Price Index/Lease Index",
     col="black") 
lines(Data$LeaseIndex/max(Data$LeaseIndex) ~Data$YearMonth,
      type="l", 
      main="Lease index",
      col="dark red")
legend("topleft",
       c("House Price index","Lease index"),
       lty = 1,
       col=c('black', 'darkred'),
       cex=0.6)

#THEORY : No obvious relation ship  between the variables.
# We see no endogenous relationship.

# Do some preliminary analysis, plotting all variables with the house price index.

# Plot House Price index vs. Construction Cost, Real estate Transactions, Loans of Banks to Households
layout(1:1)
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     xlab="Time",
     ylab="Scaled indices/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(0.1, 1)) 
lines(Data$ConstructionCostIndex/max(Data$ConstructionCostIndex) ~Data$YearMonth,
      type="l", 
      col="dark red")
lines(Data$RealEstateTransactions/max(Data$RealEstateTransactions) ~Data$YearMonth,
      type="l", 
      col="green")
lines(Data$LoansOfBanksToHouseholds/max(Data$LoansOfBanksToHouseholds) ~Data$YearMonth,
      type = 'l',
      col="purple"
      )
legend(0.1,0.4,
       c("House Price index","Construction Cost index","Real Estate Transactions", "Loans of Banks to Housholds"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6)
       
# Plot House Price index vs. UnindexedLoans, CentralBankRates, Inflation
layout(1:1)
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(0, 1)) 
lines(Data$Un.indexedLoans/max(Data$Un.indexedLoans) ~Data$YearMonth,
      type="l", 
      col="dark red")
lines(Data$CentralBankRates/max(Data$CentralBankRates) ~Data$YearMonth,
      type="l", 
      col="green")
lines(Data$Inflation/max(Data$Inflation) ~Data$YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Unindexed Loans","Central Bank Rates", "Inflation"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6)


# Plot House Price index vs. PurchasingPower, PurchasingPowerOfWages, IndexLoans
layout(1:1)
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-1.6, 1)) 
lines(Data$PurchasingPower/max(Data$PurchasingPower) ~Data$YearMonth,
      type="l", 
      col="dark red")
lines(Data$PurchasingPowerOfWages/max(Data$PurchasingPowerOfWages) ~Data$YearMonth,
      type="l", 
      col="green")
lines(Data$IndexLoans/max(Data$IndexLoans) ~Data$YearMonth,
      type = 'l',
      col="purple"
)
legend("bottomleft",
       c("House Price index","Purchasing Power","Purchasing Power of Wages", "Index Loans"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6)


# Plot House Price index vs. CurrencyLoans, HousingStarts, HousingFinished
layout(1:1)
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(0, 1)) 
lines(Data$CurrencyLoans/max(Data$CurrencyLoans) ~Data$YearMonth,
      type="l", 
      col="dark red")
lines(Data$HousingStarts/max(Data$HousingStarts) ~Data$YearMonth,
      type="l", 
      col="green")
lines(Data$HousingFinished/max(Data$HousingFinished) ~Data$YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Currency Loans","Housing Starts", "Housing Finished"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6)

