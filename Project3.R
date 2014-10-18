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
attach(Data)
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

plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     type="l",
     main="Comparison",
     xlab="Time",
     ylab="House Price Index/Lease Index",
     ylim=c(-2, 2),
     col="black") 
lines((LeaseIndex-mean(LeaseIndex))/sd(LeaseIndex) ~YearMonth,
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
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indices/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-2, 2.3),
     lwd=4) 
lines((ConstructionCostIndex-mean(ConstructionCostIndex))/sd(ConstructionCostIndex) ~YearMonth,
      type="l", 
      col="dark red")
lines((RealEstateTransactions-mean(RealEstateTransactions))/sd(RealEstateTransactions) ~YearMonth,
      type="l", 
      col="green",
      )
lines((LoansOfBanksToHouseholds-mean(LoansOfBanksToHouseholds))/sd(LoansOfBanksToHouseholds) ~YearMonth,
      type = 'l',
      col="purple"
      )
legend("topleft",
       c("House Price index","Construction Cost index","Real Estate Transactions", "Loans of Banks to Housholds"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Loans of Banks to households seem to share similar trend.       



# Plot House Price index vs. UnindexedLoans, CentralBankRates, Inflation
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-1.6, 3.2),
     lwd=4) 
lines((Un.indexedLoans-mean(Un.indexedLoans))/sd(Un.indexedLoans) ~YearMonth,
      type="l", 
      col="dark red")
lines((CentralBankRates-mean(CentralBankRates))/sd(CentralBankRates) ~YearMonth,
      type="l", 
      col="green")
lines((Inflation-mean(Inflation))/sd(Inflation) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Unindexed Loans","Central Bank Rates", "Inflation"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Central Bank Rates seem to share a similar trend.

# Plot House Price index vs. PurchasingPower, PurchasingPowerOfWages, IndexLoans
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-3, 2.5),
     lwd=4) 
lines((PurchasingPower-mean(PurchasingPower))/sd(PurchasingPower) ~YearMonth,
      type="l", 
      col="dark red")
lines((PurchasingPowerOfWages-mean(PurchasingPowerOfWages))/sd(PurchasingPowerOfWages) ~YearMonth,
      type="l", 
      col="green")
lines((IndexLoans-mean(IndexLoans))/sd(IndexLoans) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Purchasing Power","Purchasing Power of Wages", "Index Loans"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Purchasing power of wages and index loans, seem to share a similar trend

# Plot House Price index vs. CurrencyLoans, HousingStarts, HousingFinished
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-1.7, 4.5),
     lwd=4) 
lines((CurrencyLoans-mean(CurrencyLoans))/sd(CurrencyLoans) ~YearMonth,
      type="l", 
      col="dark red")
lines((HousingStarts-mean(HousingStarts))/sd(HousingStarts) ~YearMonth,
      type="l", 
      col="green")
lines((HousingFinished-mean(HousingFinished))/sd(HousingFinished) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Currency Loans","Housing Starts", "Housing Finished"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Housing finished seem to share a similar trend.

#now a comparison of variables with similar trend
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison of variables with similar trend",
     col="black",
     ylim=c(-1.5, 2.5),
     lwd=4) 
lines((CentralBankRates-mean(CentralBankRates))/sd(CentralBankRates) ~YearMonth,
      type="l", 
      col="dark red")
lines((LoansOfBanksToHouseholds-mean(LoansOfBanksToHouseholds))/sd(LoansOfBanksToHouseholds) ~YearMonth,
      type="l", 
      col="green")
lines((HousingFinished-mean(HousingFinished))/sd(HousingFinished) ~YearMonth,
      type = 'l',
      col="purple"
)
lines((ConstructionCostIndex-mean(ConstructionCostIndex))/sd(ConstructionCostIndex) ~YearMonth,
      type = 'l',
      col="blue"
)
legend("topleft",
       c("House Price index","Central bank rates","loans to households", "New housing finished","Construnction cost index"),
       lty = 1,
       col=c('black', 'darkred','green','purple','Blue'),
       cex=0.6,
       lwd=4)

#-----Task 3----------
#Create a model for the house price index using the relevant available data

HousePrice_mdl =lm(HousePriceIndex ~CentralBankRates+HousingFinished+LoansOfBanksToHouseholds+ConstructionCostIndex)
