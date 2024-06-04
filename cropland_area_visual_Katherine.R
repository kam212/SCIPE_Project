rm(list=ls())

## load cropland area data
load(file="/Users/katherine/Documents/ha_state.rData")

## load cross-walk data
library(readxl)
crop0 <- as.data.frame(read_xlsx("/Users/katherine/Documents/us_cropgrids.xlsx",
  sheet = "us_cropgrids"))

## remove duplicates
crop1 <- subset(crop0,TOTAL==2&DUPLICATE==2)

## remove undefined FAO group
crop <- subset(crop1,!is.na(FAO_GROUP))

ha1 <- merge(
  ha_state_df,
  crop[,c("crop_id","FAO_GROUP","Temporrary/Permanent")],
  by="crop_id")


load(file="/Users/katherine/Documents/conUS.rData")
library(ggplot2)
## map for a given year and crop
df_ <- subset(ha1,year==2000&crop_id==1)

df2_ <- merge(df_,crop1[,1:4],by="crop_id")
title_ <- with(df2_,paste(commodity_desc,class_desc,util_practice_desc,FAO_GROUP))
shp_ <- merge(conUS,df_,by.x="STATEFP",by.y="state_fips_code")
range(df_$kHA)
ggplot(data=shp_)+geom_sf(aes(fill=kHA))+
  ggtitle(title_)+
  guides(fill=guide_legend(title="HA(x1,000)"))

## time series
## for a given state and crop
df_ <- subset(ha1,state_fips_code=="01"&crop_id==3)
df2_ <- merge(df_,crop1[,1:4],by="crop_id")
title_ <- with(df2_,paste(commodity_desc,class_desc,util_practice_desc,FAO_GROUP))
ggplot(data=df2_,aes(x=year,y=kHA))+geom_line()+
  ggtitle(title_)+ylab("HA (x1,000)")

## bar plots
## for a given state and year, over crop groups
df_ <- subset(ha1,state_fips_code=="01"&year==2000)
df2_ <- aggregate(kHA~FAO_GROUP,data=df_,FUN=sum)
ggplot(data=df2_,
       aes(x=reorder(FAO_GROUP,-kHA),y=kHA))+
  geom_bar(stat="identity")+
  coord_flip()+xlab("FAO Group")+ylab("HA(x1,000)")
