#TASK 1 - project 3

############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################


# Brain storming excersice (10%)
# Identify causal variables and hypothize an econometric structural equation for the following variables;
# . Stock price index, for your home country,

#OMX =  beta0 -ISL_GENGI * beta1 + epsilon
#Heakandi kronu gengi laekkar hlutabrefavisitoluna, thi staerstu felogin gera upp i erlendum gjaldeyri.
#haegt ad nalgast upplysingar t.d. hja gamma.is

# . Scandinavian electricity market(Nordpool) or the electrical market of your home country(for foreign
#                  students only, as there is no spot market in Iceland),

#NORDPOOL = beta0 - útihitastig*beta1 + epsilon

# haerra utihitastig laekkar eftirspurn eftir rafmagni, vedurupplysingar eru audvelt ad nalgast

# . Inflation.
#INFLATION = beta0 + styrivextir * beta1 + VSK*beta2 + epsilon
# haerri styrivextri og haekkandi VSK hækkar verdbolgu, gögn til hja t.d. hagstofu


# Housing Economics (40%)

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

# Task 2 Plot the relationship between house prices and leasing over time. Theorize on the relationship between
# the two variables. Is there an endogenous relationship between the two? Do some preliminary analysis,
# plotting all variables with the house price index.


plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     type="l", 
     main="house price index",
     col="blue") # skrifa i x of y as
lines(Data$LeaseIndex/max(Data$LeaseIndex) ~Data$YearMonth,type="l", main="Lease index",col="dark red")
#THEORY : No obvious relation ship  between the variables.
#We see no endogenous relationship.
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,
     type="l", 
     main="house price index",
     col="blue") # skrifa i x of y as
lines(Data$ConstructionCostIndex/max(Data$ConstructionCostIndex) ~Data$YearMonth,type="l", main="Lease index",col="dark red")
lines(Data$RealEstateTransactions/max(Data$RealEstateTransactions) ~Data$YearMonth,type="l", main="Lease index",col="green")
#Afhverju er realestate transaction svona skritin og fa gogn

layout(1:1)
plot(Data$HousePriceIndex/max(Data$HousePriceIndex) ~Data$YearMonth,type="b", main="house price index")
