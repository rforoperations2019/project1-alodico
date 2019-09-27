library(ggplot2)
#reading and merging data files for geography names with FIPS
CCFIPS <- read.csv(file="citycountyfips.csv")
SASMCD <- read.csv(file="SAS_County_Data.csv")
CountyDataWNames <- merge(CCFIPS, 
                          SASMCD, 
                          by.x = "CityCountyFIPS", 
                          by.y= "FIPS")

#LAOPR80 scatterplot
ggplot(CountyDataWNames, aes(x=Long_Acting_Opioid_Pres2, 
                             y=Opioid_Prescribing_Rate2,
                             color=target_LAOPR_80)) + geom_point()
#LAOPR90 scatterplot
ggplot(CountyDataWNames, aes(x=Long_Acting_Opioid_Pres2, 
                             y=Opioid_Prescribing_Rate2,
                             color=target_LAOPR_90)) + geom_point()
