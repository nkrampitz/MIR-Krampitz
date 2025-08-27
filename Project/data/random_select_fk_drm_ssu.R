# Random Sample Selection
# January 2017
library(gdata)
# require(gdata)

allocation<-read.csv("C:\\Users\\dione.swanson\\MIR2022_benthic_analysis\\fk_drm_sualloc_2022.csv")
survey.domain<-read.csv("C:\\Users\\dione.swanson\\MIR2022_benthic_analysis\\fk_drm_sulist_2022.csv")
head(allocation)
allocation
sum(allocation$nh)
head(survey.domain)
sample.allocation <- data.frame()


# Loop through Strata randomly selecting "nh" rows from <domain> based on "nh" in <allocation>  
for(i in 1:length(allocation$PRIMARY_SAMPLE_UNIT)) {
  temp <- subset(survey.domain, survey.domain$PRIMARY_SAMPLE_UNIT == as.character(allocation$PRIMARY_SAMPLE_UNIT)[i])
  sample <- temp[sample(nrow(temp), allocation$nh[i], replace = FALSE),]
  sample.allocation <- rbind(sample.allocation,sample)
}

sample.allocation
grids<-subset(sample.allocation,select = c("YEAR","PRIMARY_SAMPLE_UNIT","STATION_NR")) 
rownames(grids)<-NULL

#print(paste("Total PSU: ",length(grids$UNIQUEID

grids

#write a *.csv file with the PSU Grid_IDs for joining in ArcGIS
write.csv(grids,file = "C:/Users/dione.swanson/MIR2022_benthic_analysis/fk_drm_su_select_2022.csv", row.names=F)


