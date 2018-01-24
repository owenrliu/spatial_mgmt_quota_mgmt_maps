## Maps for Liu et al. "The use of spatial management tools in rights-based groundfish fisheries"
## Fish and Fisheries 2018 (in revision)

# libraries
library(tidyverse)
library(sf)
library(patchwork)
library(extrafont)

## Ggplot theme
theme_rockwell <- function() {
  theme_minimal()+
    theme(text=element_text(family="Rockwell",size=12),
          axis.title = element_text(family="Rockwell",size=14),
          strip.background = element_rect(colour="black"),
          panel.border = element_rect(color="black",fill=NA))
}


## 4 spatial plots:

## world borders ##
cntrys <- st_read(dsn="data/spatial",layer="CNTR_RG_03M_2014")

#### Iceland ####
# protected area shapefiles
dsn<-"data/spatial/iceland"
st_layers(dsn=dsn)

# get the names of all shapefiles in the folder
iceland <- st_layers(dsn=dsn)$name %>% 
  purrr::map(st_read,dsn=dsn,stringsAsFactors=F)
names(iceland) <- st_layers(dsn=dsn)$name

# four types of closed areas-- organize and rename
iceland$yearround <- iceland$yearround %>% mutate(type="yearround")
iceland$realtime <- iceland$realtime %>% mutate(type="real time")
iceland$trawlclosures <- iceland$trawlclosures %>% mutate(type="trawl closure")
iceland$seasonal_closures$Dscrptn <- as.character(iceland$seasonal_closures$id)
iceland$seasonal_closures <- iceland$seasonal_closures %>% mutate(type="seasonal closure")%>%
  st_transform(crs=4326) %>% select(-id)

# combine layers into one object
iceland<-do.call("rbind",iceland)

#plot
bbox <- st_bbox(iceland)
iceland_plot <- ggplot()+
  geom_sf(data=cntrys,fill="gray75")+
  geom_sf(data=iceland,aes(fill=type))+
  xlim(bbox$xmin,bbox$xmax)+
  ylim(bbox$ymin,bbox$ymax)+
  theme_rockwell()

#### Southeast Australia ####

# for SESSF
dsn<-"data/spatial/australia/sessf"
st_layers(dsn=dsn)

# get the names of all shapefiles in the folder
australia <- st_layers(dsn=dsn)$name %>% 
  purrr::map(st_read,dsn=dsn,stringsAsFactors=F)
names(australia) <- st_layers(dsn=dsn)$name

# australia has a lot of different areas. we'll categorize them by type
australia<-do.call("rbind",australia) %>% mutate(type="unassigned")
australia$type[c(6:24,29,37)] <- "trawl"
australia$type[1] <- "seasonal"
australia$type[c(2,31:34,38)] <- "gillnet"
australia$type[c(3,6:24,29,37)] <- "trawl"
australia$type[c(4,5,25:28,39,40)] <- "all fishing"
australia$type[c(30,41:43)] <- "100% observer coverage"
australia$type[c(35,36)] <- "longline"

#mpas
australia_mpas <- st_read(dsn="data/spatial/australia/mpa",layer="capad_2016_marine") %>%
  mutate(type="marine reserve")

#set bounding box to SESSF (since MPAs cover the whole country and are outside our scope)
bbox <- st_bbox(australia)
#plot
australia_plot <- ggplot()+
  geom_sf(data=cntrys,fill="gray75")+
  geom_sf(data=australia,aes(fill=type))+
  geom_sf(data=australia_mpas,aes(fill=type))+
  xlim(bbox$xmin,bbox$xmax)+
  ylim(bbox$ymin,bbox$ymax)+
  theme_rockwell()
  

#### Scotian Shelf ####
dsn<-"data/spatial/scotian shelf"
st_layers(dsn=dsn)
ss <- st_read(dsn=dsn,layer="scotian shelf closures") %>%
  rename(name=type,type=descriptio)

#plot
bbox <- st_bbox(ss)
ss_plot <- ggplot()+
  geom_sf(data=cntrys,fill="gray75")+
  geom_sf(data=ss,aes(fill=type))+
  xlim(bbox$xmin,bbox$xmax)+
  ylim(bbox$ymin,bbox$ymax)+
  theme_rockwell()

#### Plaice Box ####
dsn<-"data/spatial/plaice box"
st_layers(dsn=dsn)
pb <- st_read(dsn=dsn,layer="plaice_box_clip3")

#plot
bbox <- st_bbox(pb)
pb_plot <- ggplot()+
  geom_sf(data=cntrys,fill="gray75")+
  geom_sf(data=pb,fill="blue")+
  xlim(bbox$xmin,bbox$xmax)+
  ylim(bbox$ymin,bbox$ymax)+
  theme_rockwell()
pb_plot
