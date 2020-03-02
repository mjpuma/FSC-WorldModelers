library(dplyr)
library(igraph)

setwd("/Users/puma/GitHub_mjpuma/FSC-WorldModelers")
##setwd()

CountriesList <- read.csv("ancillary/country_list178.csv")
CountriesList <- CountriesList[order(CountriesList$FAO), ]
CountriesList$iso3 <- as.character(CountriesList$iso3)

## Load production, trade, stocks data
load("inputs_processed/Wheat2007P0.RData")
load("inputs_processed/Wheat2007R0.RData")
load("inputs_processed/WheatAllCommodities2007E0.RData")

years = 2007:2011

## Production aomaly data
Anomalies <-
  read.csv("wheat_production_change_country_yr1-15_multi_model_mean_2019-08-14.csv")

for (i in 1:length(years)) {
  ## Adjust production changes from percent to fraction
  Anomalies[i + 1] <- Anomalies[i + 1] / 100
  
  ## Create 'Shocks' dataframe
  Shocks <-
    merge(
      CountriesList,
      Anomalies[c(1, i + 1)],
      by.x = 'iso3',
      by.y = 'X',
      all.x = TRUE,
      all.y = FALSE
    )
  Shocks[is.na(Shocks)] <- 0
  colnames(Shocks)[4] <- "singleyr"
  
  ## Separate fractional gains and losses in production
  Shocks$FracGain <- ifelse(Shocks$singleyr > 0, Shocks$singleyr, 0)
  Shocks$FracLoss <-
    ifelse(Shocks$singleyr > 0, 0, (Shocks$singleyr * -1))
  P <- data.frame(P0 = P0, iso3 = names(P0))
  Shocks <- merge(Shocks, P, by = "iso3")
  Shocks$dP <- (-Shocks$FracLoss) * (Shocks$P0)
  Shocks <- Shocks[order(Shocks$FAO), ]
  
  ## Create 'Reserves' dataframe
  Reserves <- data_frame(iso3 = names(R0), R0 = R0)
  Reserves <-
    merge(Shocks,
          Reserves,
          by = 'iso3',
          all.x = TRUE,
          all.y = FALSE)
  Reserves <- Reserves[order(Reserves$FAO), ]
  
  ## Add positive anomalies to reserves
  Reserves$R1 <- (Reserves$P0 * Reserves$FracGain) + Reserves$R0
  identical(Reserves$R0[Reserves$Y1Inc == 0], Reserves$R1[Reserves$FracGain ==
                                                            0]) ## R0=R1 where no production increases occurred
  R1 <- (Reserves$R1)
  names(R1) <- Reserves$iso3
  
  ## Convert negative production shocks from dataframe to list
  shockintensities <- Shocks$FracLoss
  names(shockintensities) <- Shocks$iso3
  SingleyrLoss <- list(shockintensities)
  
  ## Run Marchand et al (2016) version of Food Shocks Cascade (FSC) model
  Results_raw <- sim_mc_multi(
    SingleyrLoss, 
    P0, 
    R1,
    E0,
    cfrac = 0.1,
    asym = T,
    kmax = 1000,
    amin = 1E-5
  )
  
  ## Store output in dataframe
  
  # Create dataframe for output if i = 1
  if (i == 1) {
    FSCResults <- data.frame(iso3 = names(Results_raw$P0), 
                             dR = 1:length(years),
                             R1 = R1)
  }
  text <- paste("dR", i)
  FSCResults$dR[[text]] <- ifelse(FSCResults$dR == i, 1, 0)
  FSCResults <- data.frame(dR = Results_raw$dR)
}


  ########YEAR2################
  #update reserve levels based on year 1 results 
  #add positive Y2 shocks to R
  R2<-((WheatResultsY1$R0+WheatResultsY1$dR)+(WheatResultsY1$P0*Reserves$Y2Inc))
  R2<-R2[,1]
  
  R2_Results<-data.frame(iso3=names(WheatResultsY1$P0), R2=R2)
  
  ####Run Original Cascade model 
  shockintensitiesY2<-Shocks$Y2Dec
  names(shockintensitiesY2)<-Shocks$iso3
  Y2Dec<-list(shockintensitiesY2)
  
  WheatResultsY2 <- sim_mc_multi(Y2Dec, P0, R2, E0, cfrac = 0.1,
                                 asym = T, kmax = 1000, amin = 1E-5)
  
  ###Compare Results
  Results<-data.frame(iso3=names(WheatResultsY1$P0),
                      R1 = R1,
                      dRY1v1 = WheatResultsY1$dR,
                      R2=R2,
                      dRY2v1 = WheatResultsY2$dR)
  