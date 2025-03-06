# set the path where my datasets are stored
setwd('path where my datasets are stored')
# read districts
library(rgdal)
dis<-readOGR(dsn='.',layer='bydel') 
# rename districts to avoid Danish characters
dis@data[['navn']]<-c('Valby','Osterbro','Amager Ost','Amager Vest','Vesterbro-Kongens Enghave','Norrebro',
                      'Indre By','Bronshoj-Husum','Bispebjerg','Vanlose')
# compute de centroids of the districts
library(rgeos)
dis_cent<-gCentroid(dis,byid=T)
# transform districts' centroids to data frame (to enable subsequent crops)
library(sf)
dis_cent_df<-st_as_sf(dis_cent)
# reproject districts' centroids to match the projection of UA
dis_cent_proj<-dis_cent_df%>%st_transform(3035)
# compute buffer distances of 20 km around the centroids ("Short food supply chains and local food systems in the EU")
dis_cent_buf<-st_buffer(dis_cent_proj,20000)
# merge all the buffer areas into a single one
dis_cent_buf_merge<-st_union(dis_cent_buf)
# read Urban Atlas (UA) for Copenhagen
ua<-st_read('DK001L2_KOBENHAVN_UA2018_v013/Data/DK001L2_KOBENHAVN_UA2018_v013.gpkg',layer='DK001L2_KOBENHAVN_UA2018')
# crop UA to the boundaries enclosed by the buffer area
ua_buf<-st_intersection(ua,dis_cent_buf_merge)
# transform districts to data frame (to enable subsequent crops)
dis_df<-st_as_sf(dis)
# compute area of each district
dis_df$area<-as.double(st_area(dis_df))
# compute perimeter for each district
dis_df$perimeter<-as.double(st_length(st_cast(dis_df,'MULTILINESTRING')))
# reproject districts to match the projection of UA
dis_proj<-dis_df%>%st_transform(3035)
# crop UA to the boundaries enclosed by the districts
ua_dis<-st_intersection(ua_buf,dis_proj)
# calculate the area of the CLC classes
ua_dis$area_ua<-as.double(st_area(ua_dis))

#### PROVISIONING SERVICES ####

### i1
# subset to extract the UA classes corresponding to i1
i1<-subset(ua_buf,grepl('^21',ua_buf$code_2018) | grepl('^22',ua_buf$code_2018) |
             grepl('^24',ua_buf$code_2018))
# compute the centroids of classes 21, 22 and 24
library(rgeos)
i1_cent<-gCentroid(as(i1,'Spatial'),byid=T)
# transform districts to data frame (to enable plotting)
library(sf)
i1_cent_df<-st_as_sf(i1_cent)
# set the coordinates of the districts' centroids as the destinations
dst<-as.data.frame(cbind('destination',coordinates(dis_cent)))
# transform the coordinates' columns to double
dst$x<-as.double(dst$x)
dst$y<-as.double(dst$y)
# transform the i1 classes to sf to enable reprojection
i1_src<-st_as_sf(i1_cent)
# reproject to WGS84 (required by the osrm package)
i1_src<-i1_src%>%st_transform(4326)
# set the coordinates of the i1 clasess' centroids as the sources
i1_src<-as.data.frame(cbind('origin',coordinates(as(i1_src,'Spatial'))))
# rename the sources to match the colnames of the destinations
colnames(i1_src)<-c('V1','x','y')
# transform the coordinates' columns to double
i1_src$x<-as.double(i1_src$x)
i1_src$y<-as.double(i1_src$y)
# split the sources into three because osrmTable does not compute with so many data
i1_src<-split(i1_src,rep(1:3,length.out=nrow(i1_src),each=ceiling(nrow(i1_src)/3)))
# build three (see above) data frames with both the sources and the destinations
i1_src_dst.1<-as.data.frame(rbind(dst,i1_src[[1]]))
i1_src_dst.2<-as.data.frame(rbind(dst,i1_src[[2]]))
i1_src_dst.3<-as.data.frame(rbind(dst,i1_src[[3]]))
# compute the distance and travel duration among the i1 classes' and districts' centroids (three times, see above)
library(osrm)
didu_i1.1<-osrm::osrmTable(i1_src[[1]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
didu_i1.2<-osrm::osrmTable(i1_src[[2]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
didu_i1.3<-osrm::osrmTable(i1_src[[3]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
# merge again the values of duration and distance previously split
didu_i1.dur<-rbind(didu_i1.1[['durations']],didu_i1.2[['durations']],didu_i1.3[['durations']])
didu_i1.dis<-rbind(didu_i1.1[['distances']],didu_i1.2[['distances']],didu_i1.3[['distances']])
# calculate the area of i1
i1$area_i1<-as.double(st_area(i1))
# assign a score to each of the three classes involved in i1 (1 to 21 and 22 and 0.5 to 24)
i1$score<-ifelse(grepl('^24',i1$code_2018),0.5,1)
# compute the distances from each district's centroid to the centroids of i1 classes (only if there are less than 20 km)
i1_sc<-c()
for (i in 1:ncol(didu_i1.dur)){
  i1_sc[[i]]<-ifelse(didu_i1.dis[,i]<=20000, # "Short food supply chains and local food systems in the EU" para los 20 km
                  (1/didu_i1.dur[,i])*i1$area_i1*i1$score,0) # el inverso de la duraci?n porque interesan trayectos cortos
}
# calculate the sum of scores per district
i1_sc<-sapply(i1_sc,sum)  
# plot the areas corresponding to i1
library(ggplot2)
i1_p<-ggplot() + geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),lwd=0.4,show.legend='polygon') +
  geom_sf(data=dis_cent_proj,size=1,shape=17,aes(color='A'),show.legend='point') +
  geom_sf(data=i1,aes(fill=as.factor(score)),lwd=0.1) + scale_fill_manual(values=c('skyblue','khaki'),name='Score',
                                                                          guide=guide_legend(override.aes=list(shape=NA))) + 
  geom_sf(data=i1_cent_df,size=0.3,shape=20,aes(color='B'),show.legend='point') +
  scale_color_manual(values=c('A'='tomato','B'='black'),labels=c('Districts','Agricultural land'),
                     name='Centroids',guide=guide_legend(override.aes=list(shape=c(17,20)))) +
  scale_x_continuous(limits=c(4458378,4495923)) + scale_y_continuous(limits=c(3608287,3644524)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.margin=margin(t=0,r=0,b=0,l=0,unit='pt'),legend.title=element_text(size=9),legend.position='right')
# add the  i1 scores to the districts data frame
dis_df$i1<-i1_sc

### i2
# subset to extract the UA classes corresponding to i2
i2<-subset(ua_buf,grepl('^23',ua_buf$code_2018))
# compute de centroids of class 23
i2_cent<-gCentroid(as(i2,'Spatial'),byid=T)
# transform districts to data frame (to enable plotting)
i2_cent_df<-st_as_sf(i2_cent)
# transform the i1 classes to sf to enable reprojection
i2_src<-st_as_sf(i2_cent)
# reproject to WGS84 (required by the osrm package)
i2_src<-i2_src%>%st_transform(4326)
# set the coordinates of the i1 clasess' centroids as the sources
i2_src<-as.data.frame(cbind('origin',coordinates(as(i2_src,'Spatial'))))
# rename the sources to match the colnames of the destinations
colnames(i2_src)<-c('V1','x','y')
# transform the coordinates' columns to double
i2_src$x<-as.double(i2_src$x)
i2_src$y<-as.double(i2_src$y)
# split the sources into two because osrmTable does not compute with so many data
i2_src<-split(i2_src,rep(1:3,length.out=nrow(i2_src),each=ceiling(nrow(i2_src)/2)))
# build two (see above) data frames with both the sources and the destinations
i2_src_dst.1<-as.data.frame(rbind(dst,i2_src[[1]]))
i2_src_dst.2<-as.data.frame(rbind(dst,i2_src[[2]]))
# compute the distance and travel duration among the i2 classes' and districts' centroids (two times, see above)
library(osrm)
didu_i2.1<-osrm::osrmTable(i2_src[[1]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
didu_i2.2<-osrm::osrmTable(i2_src[[2]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
# merge again the values of duration and distance previously split
didu_i2.dur<-rbind(didu_i2.1[['durations']],didu_i2.2[['durations']])
didu_i2.dis<-rbind(didu_i2.1[['distances']],didu_i2.2[['distances']])
# calculate the area of i2
i2$area_i2<-as.double(st_area(i2))
# assign a score to the three class involved in i2
i2$score<-1
# compute the distances from each district's centroid to the centroids of i2 class (only if there are less than 20 km)
i2_sc<-c()
for (i in 1:ncol(didu_i2.dur)){
  i2_sc[[i]]<-ifelse(didu_i2.dis[,i]<=20000, # "Short food supply chains and local food systems in the EU" para los 20 km
                     (1/didu_i2.dur[,i])*i2$area_i2*i2$score,0) # el inverso de la duraci?n porque interesan trayectos cortos
}
# calculate the sum of scores per district
i2_sc<-sapply(i2_sc,sum)  
# plot the areas corresponding to i2
library(ggplot2)
i2_p<-ggplot() + geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),lwd=0.4,show.legend='polygon') +
  geom_sf(data=dis_cent_proj,size=1,shape=17,aes(color='A'),show.legend='point') +
  geom_sf(data=i2,aes(fill=as.factor(score)),lwd=0.1) + scale_fill_manual(values=c('khaki','khaki'),name='Score',
                                                                          guide=guide_legend(override.aes=list(shape=NA))) + 
  geom_sf(data=i2_cent_df,size=0.3,shape=20,aes(color='B'),show.legend='point') +
  scale_color_manual(values=c('A'='tomato','B'='black'),labels=c('Districts','Pastures         '),
                     name='Centroids',guide=guide_legend(override.aes=list(shape=c(17,20)))) +
  scale_x_continuous(limits=c(4458378,4495923)) + scale_y_continuous(limits=c(3608287,3644524)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.margin=margin(t=0,r=0,b=0,l=-7,unit='pt'),legend.title=element_text(size=9),legend.position='right')
# add the i2 scores to the districts data frame
dis_df$i2<-i2_sc
# plot the values of i2 per district
# ggplot(dis_df) + geom_sf(aes(fill=i2)) + scale_fill_gradient(low='grey',high='green') + 
#   geom_sf_text(aes(label=sprintf('%0.3f',round(i2,digits=3))),colour='black') + theme(legend.position='none')

### i3
# subset to extract the UA classes corresponding to i3
i3<-subset(ua_buf,grepl('^31',ua_buf$code_2018) | grepl('^32',ua_buf$code_2018))
# compute de centroids of classes 31 and 32 
i3_cent<-gCentroid(as(i3,'Spatial'),byid=T)
# transform districts to data frame (to enable plotting)
i3_cent_df<-st_as_sf(i3_cent)
# transform the i3 classes to sf to enable reprojection
i3_src<-st_as_sf(i3_cent)
# reproject to WGS84 (required by the osrm package)
i3_src<-i3_src%>%st_transform(4326)
# set the coordinates of the i3 clasess' centroids as the sources
i3_src<-as.data.frame(cbind('origin',coordinates(as(i3_src,'Spatial'))))
# rename the sources to match the colnames of the destinations
colnames(i3_src)<-c('V1','x','y')
# transform the coordinates' columns to double
i3_src$x<-as.double(i3_src$x)
i3_src$y<-as.double(i3_src$y)
# split the sources into three because osrmTable does not compute with so many data
i3_src<-split(i3_src,rep(1:3,length.out=nrow(i3_src),each=ceiling(nrow(i3_src)/3)))
# build three (see above) data frames with both the sources and the destinations
i3_src_dst.1<-as.data.frame(rbind(dst,i3_src[[1]]))
i3_src_dst.2<-as.data.frame(rbind(dst,i3_src[[2]]))
i3_src_dst.3<-as.data.frame(rbind(dst,i3_src[[3]]))
# compute the distance and travel duration among the i3 classes' and districts' centroids (three times, see above)
library(osrm)
didu_i3.1<-osrm::osrmTable(i3_src[[1]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
didu_i3.2<-osrm::osrmTable(i3_src[[2]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
didu_i3.3<-osrm::osrmTable(i3_src[[3]][,c('x','y')],dst[,c('x','y')],
                           measure=c('distance','duration')) # meters and minutes
# merge again the values of duration and distance previously split
didu_i3.dur<-rbind(didu_i3.1[['durations']],didu_i3.2[['durations']],didu_i3.3[['durations']])
didu_i3.dis<-rbind(didu_i3.1[['distances']],didu_i3.2[['distances']],didu_i3.3[['distances']])
# calculate the area of i1
i3$area_i3<-as.double(st_area(i3))
# assign a score to each of the three classes involved in i3 (1 to 31 and 0.5 to 32)
i3$score<-ifelse(grepl('^32',i3$code_2018),0.5,1)
# compute the distances from each district's centroid to the centroids of i3 classes (only if there are less than 20 km)
i3_sc<-c()
for (i in 1:ncol(didu_i3.dur)){
  i3_sc[[i]]<-ifelse(didu_i3.dis[,i]<=20000, # "Short food supply chains and local food systems in the EU" para los 20 km
                     (1/didu_i3.dur[,i])*i3$area_i3*i3$score,0) # el inverso de la duraci?n porque interesan trayectos cortos
}
# calculate the sum of scores per district
i3_sc<-sapply(i3_sc,sum)  
# plot the areas corresponding to i3
library(ggplot2)
i3_p<-ggplot() + geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),lwd=0.4,show.legend='polygon') +
  geom_sf(data=dis_cent_proj,size=1,shape=17,aes(color='A'),show.legend='point') +
  geom_sf(data=i3,aes(fill=as.factor(score)),lwd=0.1) + scale_fill_manual(values=c('skyblue','khaki'),name='Score',
                                                                          guide=guide_legend(override.aes=list(shape=NA))) + 
  geom_sf(data=i3_cent_df,size=0.3,shape=20,aes(color='B'),show.legend='point') +
  scale_color_manual(values=c('A'='tomato','B'='black'),labels=c('Districts','Forests          '),
                     name='Centroids',guide=guide_legend(override.aes=list(shape=c(17,20)))) +
  scale_x_continuous(limits=c(4458378,4495923)) + scale_y_continuous(limits=c(3608287,3644524)) +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.margin=margin(t=0,r=0,b=0,l=-10,unit='pt'),legend.title=element_text(size=9),legend.position='right')
# add the i3 scores to the districts data frame
dis_df$i3<-i3_sc
# plot the values of i3 per district
# ggplot(dis_df) + geom_sf(aes(fill=i3)) + scale_fill_gradient(low='grey',high='green') + 
#   geom_sf_text(aes(label=sprintf("%0.3f",round(i3,digits=3))),colour='black') + theme(legend.position='none')

#### REGULATING SERVICES ####

### i4
# read Street Tree Layer (STL) for Copenhagen
stl18<-st_read('DK001L2_KOBENHAVN_UA2018_STL_v012/Data/DK001L2_KOBENHAVN_UA2018_STL_v012.gpkg',
               layer='DK001L2_KOBENHAVN_UA2018_STL')
# crop STL to the boundaries enclosed by the districts
stl18_dis<-st_intersection(stl18,dis_proj)
# subset to extract the UA classes corresponding to roads
roads_dis<-subset(ua_dis,(grepl('^1221',ua_dis$code_2018) | grepl('^1222',ua_dis$code_2018)))
# read raster with the elevation of buildings
library(raster)
build<-raster('DK001L2_KOBENHAVN_UA2012_DHM_v020/Data/DK001L2_KOBENHAVN_UA2012_DHM_v020.tif')
# crop to the extent of the districts
build_crop<-crop(build,dis_proj)
#  mask to the shape of the districts
build_mask<-mask(build_crop,dis_proj)
# convert the masked raster of buildings to polygon
library(stars)
build_pol<-as_Spatial(st_as_sf(st_as_stars(build_mask),as_points=F,merge=T))
# convert the polygonized buildings to sf to enable subsequent plotting
build_pol_df<-st_as_sf(build_pol)
# remove all rows whose value of elevation is 0
build_pol_df<-build_pol_df[build_pol_df$DK001L2_KOBENHAVN_UA2012_DHM_v020!=0,]
# reproject buildings to match the projection of UA
build_pol_df<-build_pol_df%>%st_transform(3035)
#  intersect buildings with districts
build_dis<-st_intersection(build_pol_df,dis_proj)
# plot the areas corresponding to STL, buildings and road areas
library(ggplot2)
i4_p<-ggplot() + geom_sf(data=build_pol_df,aes(fill='A'),colour=NA,show.legend='polygon') + 
  geom_sf(data=roads_dis,aes(fill='B'),colour=NA,show.legend='polygon') + 
  geom_sf(data=stl18_dis,aes(fill='C'),colour=NA,show.legend='polygon') + 
  scale_fill_manual(values=c('A'='khaki','B'='black','C'='forestgreen'),guide=guide_legend(),name=NULL,
                     labels=c('Buildings','Roads','Urban trees')) + 
  labs(colour='Districts') + 
  geom_sf(data=st_as_sf(dis),aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm'))
# calculate the area of STL
stl18_dis$area_i4<-as.double(st_area(stl18_dis))
# calculate the sum STL per district
stl18_sum<-aggregate(stl18_dis$area_i4,list(stl18_dis$navn),sum)
# calculate the area of roads
roads_dis$area_i4<-as.double(st_area(roads_dis))
# calculate the sum roads per district
roads_sum<-aggregate(roads_dis$area_i4,list(roads_dis$navn),sum)
# calculate the area of buildings
build_dis$area_i4<-as.double(st_area(build_dis))
# calculate the sum buildings per district
build_sum<-aggregate(build_dis$area_i4,list(build_dis$navn),sum)
# create a data frame with the ratio of STL to roads+buildings per district
st18toroads_build<-data.frame(stl18_sum$Group.1,stl18_sum$x/(roads_sum$x+build_sum$x))
# rename the columns to enable the subsequent merge with the districts data frame
colnames(st18toroads_build)<-c('navn','i4')
# add the values of i4 to the districts data frame
dis_df<-merge(dis_df,st18toroads_build,by='navn')

### i5
## i5.1
# allocate values of albedo to the UA classes
library(dplyr)
ua_dis<-ua_dis %>%
  mutate(.,i5.1_sc=with(.,case_when(
    (grepl('^11',code_2018) | grepl('^121',code_2018)) ~ 0.15,grepl('^13',code_2018) ~ 0.17,grepl('^14',code_2018) ~ 0.21,
    (grepl('^122',code_2018) | grepl('^123',code_2018) | grepl('^124',code_2018)) ~ 0.10,grepl('^25',code_2018) ~ 0.19,
    (grepl('^21',code_2018) | grepl('^22',code_2018) | grepl('^24',code_2018)) ~ 0.18,grepl('^23',code_2018) ~ 0.19,
    grepl('^31',code_2018) ~ 0.16,grepl('^32',code_2018) ~ 0.16,
    grepl('^33',code_2018) ~ 0.17,grepl('^4',code_2018) ~ 0.10,grepl('^5',code_2018) ~ 0.07
  )))
# compute a weighted value of albedo by multipliying its values by the UA areas
ua_dis$i5.1<-as.double(ua_dis$i5.1_sc*ua_dis$area_ua)
# plot the values of albedo across the study area
library(ggplot2)
i5.1_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=i5.1_sc),colour=NA) + 
  scale_fill_gradient(low='lightyellow4',high='lightyellow1',breaks=c(min(ua_dis$i5.1_sc),max(ua_dis$i5.1_sc)),
                      na.value='transparent') + 
  labs(fill='Albedo',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# calculate the mean albedo per district
ua_mean_i5.1<-aggregate(ua_dis$i5.1,list(ua_dis$navn),sum)
# rename the column names to enable subsequent merge
names(ua_mean_i5.1)<-c('navn','i5.1')
# add the values of i5.1 to the districts data frame
dis_df<-merge(dis_df,ua_mean_i5.1,by='navn')
# weight the values of albedo per district by dividing by the area of the districts
dis_df$i5.1<-dis_df$i5.1/dis_df$area
## i5.2
# allocate values of Kc to the UA classes
library(dplyr)
ua_dis<-ua_dis %>%
  mutate(.,i5.2_sc=with(.,case_when(
    (grepl('^11',code_2018) | grepl('^121',code_2018) | grepl('^124',code_2018) | grepl('^4',code_2018)) ~ 0.40,
    (grepl('^112',code_2018) | grepl('^113',code_2018) | grepl('^142',code_2018)) ~ 0.30,
    (grepl('^122',code_2018) | grepl('^33',code_2018)) ~ 0.35,
    grepl('^123',code_2018) ~ 0.50,
    (grepl('^131',code_2018) | grepl('^133',code_2018) | grepl('^134',code_2018)) ~ 0.36,
    grepl('^141',code_2018) ~ 0.32,
    grepl('^21',code_2018) ~ 1.33,
    grepl('^22',code_2018) ~ 0.82,
    grepl('^23',code_2018) ~ 0.90,
    (grepl('^24',code_2018) | grepl('^25',code_2018)) ~ 1.1,
    grepl('^31',code_2018) ~ 1.37,
    grepl('^32',code_2018) ~ 1.37,
    grepl('^5',code_2018) ~ 0.69
  )))
# compute a weighted value of Kc by multipliying its values by the UA areas
ua_dis$i5.2<-as.double(ua_dis$i5.2_sc*ua_dis$area_ua)
# plot the values of Kc across the study area
library(ggplot2)
i5.2_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=i5.2_sc),colour=NA) + 
  scale_fill_gradient(low='lightblue1',high='lightblue4',breaks=c(min(ua_dis$i5.2_sc),max(ua_dis$i5.2_sc)),
                      na.value='transparent') + 
  labs(fill=(bquote('Crop coefficient ('*K[c]*')')),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# calculate the mean Kc per district
ua_mean_i5.2<-aggregate(ua_dis$i5.2,list(ua_dis$navn),sum)
# rename the column names to enable subsequent merge
names(ua_mean_i5.2)<-c('navn','i5.2')
# add the values of i5.2 to the districts data frame
dis_df<-merge(dis_df,ua_mean_i5.2,by='navn')
# weight the values of Kc per district by dividing by the area of the districts
dis_df$i5.2<-dis_df$i5.2/dis_df$area
## i5.3
# read raster with the elevation of buildings
library(raster)
build<-raster('DK001L2_KOBENHAVN_UA2012_DHM_v020/Data/DK001L2_KOBENHAVN_UA2012_DHM_v020.tif')
# crop to the extent of the districts
build_crop<-crop(build,dis_proj)
#  mask to the shape of the districts
build_mask<-mask(build_crop,dis_proj)
# read raster with the DTM
dem<-raster('EUD_CP-DEMS_4500035000-AA.tif')
# crop to the extent of the districts
dem_crop<-crop(dem,dis_proj)
# resample to the resolution of the building elevation raster (10 m)
dem_res<-resample(dem_crop,build_mask)
# mask to the shape of the districts
dem_mask<-mask(dem_res,dis_proj)
# extend the building raster to match the extent of the DTM
build_ext<-extend(build_mask,dem_mask)
# replace NAs derived from the extension with 0s to enable subsequent sum of rasters
build_ext[is.na(build_ext[])]<-0
# sum the elevation pixels of both rasters to obtain a DSM
dsm<-dem_mask+build_ext
# compute the SVF from the DSM (parameters recommended in the literature - see ArcUHI)
library(horizon)
svf<-svf(dsm,nAngles=16,maxDist=100,verbose=F,ll=F)
# convert to data frame to enable subsequent plotting
svf_df<-as.data.frame(svf,xy=T)
# remove NAs
svf_df<-svf_df[complete.cases(svf_df),]
# plot the values of svf across the study area
library(ggplot2)
library(viridis)
i5.3_p<-ggplot() + geom_tile(data=svf_df,aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradient(low='black',high='white',breaks=c(0.09,1.00),na.value='transparent') + 
  labs(fill='Sky View Factor',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# extract SVF raster values per district to list
svf_dis<-extract(svf,dis_proj)
# calculate the mean SVF for each district
svf_mean<-lapply(svf_dis,FUN=mean,na.rm=T)
# add the SVF mean values to the districts' data frame
dis_df$i5.3<-unlist(svf_mean)

### i6
# read raster with the DEM (res = 25 m)
library(raster)
dem<-raster('EUD_CP-DEMS_4500035000-AA.tif')
# crop the DEM to the extent of the districts
dem_crop<-crop(dem,dis_proj)
# compute the slope from the cropped DEM
slope<-terrain(dem_crop,opt='slope',unit='degrees',neighbors=8)
# mask the slope to the boundaries of the districts
slope_mask<-mask(slope,dis_proj)
# read raster with the global HSG map (res = 250 m)
hsg<-raster('Global_Hydrologic_Soil_Group_1566/data/HYSOGs250m.tif')
# crop the HSG map to the extent of the districts
hsg_crop<-crop(hsg,dis)
# reproject the HSG map to ETRS89-extended
hsg_proj<-projectRaster(hsg_crop,crs=crs(dis_proj),method='ngb',res=25)
## the following 5 lines are a bit confusing. They are just to match the extent and resolution of the slope and HSG maps
# mask the HSG map to the boundaries of the districts
hsg_mask<-mask(hsg_proj,dis_proj)
# crop the HSG map to the extent of the slope map
hsg_val<-crop(hsg_proj,slope_mask)
# set the extent of the masked HSG map to that of the masked slope map (keeping the same res = 25 m)
hsg_ext<-setExtent(hsg_mask,slope_mask,keepres=T)
# assign the values of the cropped HSG map to the extended HSG map
values(hsg_ext)<-values(hsg_val)
# mask the extended HSG map to the boundaries of the districts
hsg_mask<-mask(hsg_ext,dis_proj)
# simplify the HSG map so that values 11, 12, 13 and 14 correspond to 1, 2, 3 and 4, respectively ()
values(hsg_mask)[values(hsg_mask)==11]<-1
values(hsg_mask)[values(hsg_mask)==12]<-2
values(hsg_mask)[values(hsg_mask)==13]<-3
values(hsg_mask)[values(hsg_mask)==14]<-4
# create an "empty" raster layer with the extension of the UA buffer
ua_r<-raster(ua_buf,res=25)
# convert the field with the UA code to double to enable the subsequent transformation of the UA to raster
ua_buf$code_2018<-as.double(ua_buf$code_2018)
# rasterize the polygon layer with the UA codes
ua_r<-rasterize(ua_buf,ua_r,'code_2018')
## the following 5 lines are a bit confusing. They are just to match the extent and resolution of the slope and UA maps
# mask the rasterized UA to the boundaries of the districts
ua_mask<-mask(ua_r,dis_proj)
# crop the UA map to the extent of the slope map
ua_val<-crop(ua_r,slope_mask)
# set the extent of the masked UA map to that of the masked slope map (keeping the same res = 25 m)
ua_ext<-setExtent(ua_mask,slope_mask,keepres=T)
# assign the values of the cropped UA map to the extended UA map
values(ua_ext)<-values(ua_val)
# mask the extended UA map to the boundaries of the districts
ua_mask<-mask(ua_ext,dis_proj)
# convert the inputs to data frame to facilitate the allocation of values to the different combinations of UA, HSG and slope
ua_df<-as.data.frame(ua_mask,xy=T)
hsg_df<-as.data.frame(hsg_mask,xy=T)
slope_df<-as.data.frame(slope_mask,xy=T)
# combine the three inputs into a single p0 data frame
p0_df<-cbind(ua_df,hsg_df$HYSOGs250m,slope_df$slope)
# rename the columns of the p0 data frame
names(p0_df)<-c('x','y','ua','hsg','slope')
# remove the slope NAs
p0_df<-subset(p0_df,!is.na(p0_df$slope))
# fill the HSG NAs with the closest value in HSG (precedent filled row because the coordinates are incrementally ordered)
library(tidyr)
p0_df<-p0_df %>% fill(hsg,.direction='updown')
# assign values of p0 depending on the UA, HSG and slope
library(dplyr)
p0_df<-p0_df %>%
  mutate(.,p0=with(.,case_when(
    grepl('^111',ua) ~ 1, # 111 A-B-C-D
    (grepl('^112',ua) | grepl('^113',ua) | grepl('^124',ua) | grepl('^133',ua) | grepl('^134',ua)) & 
      hsg==1 ~ 24, # 112-113-124-133-134 A
    (grepl('^112',ua) | grepl('^113',ua) | grepl('^124',ua) | grepl('^133',ua) | grepl('^134',ua)) & 
      hsg==2 ~ 14, # 112-113-124-133-134 B
    (grepl('^112',ua) | grepl('^113',ua) | grepl('^124',ua) | grepl('^133',ua) | grepl('^134',ua)) & 
      hsg==3 ~ 8, # 112-113-124-133-134 C
    (grepl('^112',ua) | grepl('^113',ua) | grepl('^124',ua) | grepl('^133',ua) | grepl('^134',ua)) & 
      hsg==4 ~ 6, # 112-113-124-133-134 D
    grepl('^121',ua) & hsg==1 ~ 6,grepl('^121',ua) & hsg==2 ~ 4,grepl('^121',ua) & (hsg==3 | hsg==4) ~ 3, # 121 A-B-C-D
    (ua==12210 | ua==12220 | grepl('^123',ua)) ~ 1, # 1221-1222-123 A-B-C-D
    ua==12230 & hsg==1 ~ 12,ua==12230 & hsg==2 ~ 7,ua==12230 & hsg==3 ~ 5,ua==12230 & hsg==4 ~ 4, # 1223 A-B-C-D
    ua==13100 & hsg==1 ~ 16,ua==13100 & hsg==2 ~ 9,ua==13100 & hsg==3 ~ 6,ua==13100 & hsg==4 ~ 5, # 131 A-B-C-D
    grepl('^141',ua) & hsg==1 ~ 53,grepl('^141',ua) & hsg==2 ~ 23, # 141 A-B
    grepl('^141',ua) & hsg==3 ~ 14,grepl('^141',ua) & hsg==4 ~ 10, # 141 C-D
    grepl('^142',ua) & hsg==1 ~ 79,grepl('^142',ua) & hsg==2 ~ 32, # 142 A-B
    grepl('^142',ua) & hsg==3 ~ 18,grepl('^142',ua) & hsg==4 ~ 13, # 142 A-B
    grepl('^21',ua) & slope>=3 & hsg==1 ~ 37,grepl('^21',ua) & slope<3 & hsg==1 ~ 47, # 21 3+- A
    grepl('^21',ua) & slope>=3 & hsg==2 ~ 20,grepl('^21',ua) & slope<3 & hsg==2 ~ 25, # 21 3+- B
    grepl('^21',ua) & slope>=3 & hsg==3 ~ 12,grepl('^21',ua) & slope<3 & hsg==3 ~ 16, # 21 3+- C
    grepl('^21',ua) & slope>=3 & hsg==4 ~ 9,grepl('^21',ua) & slope<3 & hsg==4 ~ 13, # 21 3+- D
    grepl('^22',ua) & slope>=3 & hsg==1 ~ 62,grepl('^22',ua) & slope<3 & hsg==1 ~ 75, # 22 3+- A
    grepl('^22',ua) & slope>=3 & hsg==2 ~ 28,grepl('^22',ua) & slope<3 & hsg==2 ~ 34, # 22 3+- B
    grepl('^22',ua) & slope>=3 & hsg==3 ~ 15,grepl('^22',ua) & slope<3 & hsg==3 ~ 19, # 22 3+- C
    grepl('^22',ua) & slope>=3 & hsg==4 ~ 10,grepl('^22',ua) & slope<3 & hsg==4 ~ 14, # 22 3+- D
    grepl('^23',ua) & slope>=3 & hsg==1 ~ 70,grepl('^23',ua) & slope<3 & hsg==1 ~ 120, # 23 3+- A
    grepl('^23',ua) & slope>=3 & hsg==2 ~ 33,grepl('^23',ua) & slope<3 & hsg==2 ~ 55, # 23 3+- B
    grepl('^23',ua) & slope>=3 & hsg==3 ~ 18,grepl('^23',ua) & slope<3 & hsg==3 ~ 22, # 23 3+- C
    grepl('^23',ua) & slope>=3 & hsg==4 ~ 13,grepl('^23',ua) & slope<3 & hsg==4 ~ 14, # 23 3+- D
    grepl('^24',ua) & slope>=3 & hsg==1 ~ 53,grepl('^24',ua) & slope<3 & hsg==1 ~ 80, # 24 3+- A
    grepl('^24',ua) & slope>=3 & hsg==2 ~ 23,grepl('^24',ua) & slope<3 & hsg==2 ~ 35, # 24 3+- B
    grepl('^24',ua) & slope>=3 & hsg==3 ~ 14,grepl('^24',ua) & slope<3 & hsg==3 ~ 17, # 24 3+- C
    grepl('^24',ua) & slope>=3 & hsg==4 ~ 9,grepl('^24',ua) & slope<3 & hsg==4 ~ 10, # 24 3+- D
    grepl('^31',ua) & hsg==1 ~ 90,grepl('^31',ua) & hsg==2 ~ 47, # 31 A-B
    grepl('^31',ua) & hsg==3 ~ 31,grepl('^31',ua) & hsg==4 ~ 23, # 31 C-D    
    grepl('^32',ua) & hsg==1 ~ 75,grepl('^32',ua) & hsg==2 ~ 34, # 32 A-B
    grepl('^32',ua) & hsg==3 ~ 22,grepl('^32',ua) & hsg==4 ~ 16, # 32 C-D
    grepl('^33',ua) & hsg==1 ~ 15,grepl('^33',ua) & hsg==2 ~ 8, # 33 A-B
    grepl('^33',ua) & hsg==3 ~ 6,grepl('^33',ua) & hsg==4 ~ 4, # 33 C-D
    grepl('^4',ua) ~ 2, # 4 A-B-C-D
    grepl('^5',ua) ~ 0 # 5 A-B-C-D
  )))
# plot the values of runoff threshold across the study area
library(ggplot2)
i6_p<-ggplot() + geom_tile(data=p0_df,aes(x=x,y=y,fill=p0)) + 
  scale_fill_gradient(low='lightskyblue4',high='lightskyblue1',breaks=c(0,55),na.value='transparent') + 
  labs(fill='Runoff threshold',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# keep only the columns with the values of x, y and p0 to enable subsequent conversion
p0_df<-p0_df[,c(1:2,6)]
# convert the p0 data frame to raster
p0<-rasterFromXYZ(p0_df,crs=crs(dis_proj))
# extract P0 raster values per district to list
library(raster)
p0_dis<-raster::extract(p0,dis_proj)
# calculate the mean P0 for each district
p0_mean<-lapply(p0_dis,FUN=mean,na.rm=T)
# add the p0 mean values to the districts' data frame and normalize them
dis_df$i6<-unlist(p0_mean)

### i7
# assign values of diffuse pollution index depending on the UA classes
library(dplyr)
ua_dis<-ua_dis %>%
  mutate(.,i7_sc=with(.,case_when(
    grepl('^111',code_2018) ~ 6,grepl('^112',code_2018) ~ 5.5,(grepl('^113',code_2018) | grepl('^121',code_2018)) ~ 5,
    grepl('^122',code_2018) ~ 7.5,grepl('^123',code_2018) ~ 7,grepl('^124',code_2018) ~ 7,grepl('^131',code_2018) ~ 11.5,
    (grepl('^133',code_2018) | grepl('^134',code_2018)) ~ 7,grepl('^141',code_2018) ~ 3.5,
    grepl('^142',code_2018) ~ 4,grepl('^21',code_2018) ~ 13.5,grepl('^22',code_2018) ~ 5,
    grepl('^23',code_2018) ~ 3.5,grepl('^24',code_2018) ~ 8.3,grepl('^31',code_2018) ~ 2.8,
    grepl('^32',code_2018) ~ 2.6,grepl('^33',code_2018) ~ 2,grepl('^4',code_2018) ~ 2.3,
    grepl('^5',code_2018) ~ 3
  )))
# plot the values of diffuse pollution index across the study area
library(ggplot2)
i7_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=i7_sc),colour=NA) + 
  scale_fill_gradient(low='tan4',high='tan',breaks=c(min(ua_dis$i7_sc),max(ua_dis$i7_sc))) + 
  labs(fill='Pollution load index',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# compute a weighted value of i7 by multipliying its values by the UA areas
ua_dis$i7<-as.double(ua_dis$i7_sc*ua_dis$area_ua)
# calculate the sum i7 score per district
ua_sum_i7<-aggregate(ua_dis$i7,list(ua_dis$navn),sum)
# rename the column names to enable subsequent merge
names(ua_sum_i7)<-c('navn','i7')
# add the values of i7 to the districts data frame
dis_df<-merge(dis_df,ua_sum_i7,by='navn')
# weight the values of i7 per district by dividing by the area of the districts
dis_df$i7<-dis_df$i7/dis_df$area

### i8
# assign values of FA depending on the UA classes
library(dplyr)
ua_dis<-ua_dis %>%
  mutate(.,fa=with(.,case_when(
    grepl('^111',code_2018) ~ 0.05,grepl('^112',code_2018) ~ 0.30,(grepl('^113',code_2018) | grepl('^121',code_2018)) ~ 0.05,
    grepl('^122',code_2018) ~ 0.25,grepl('^123',code_2018) ~ 0.00,grepl('^124',code_2018) ~ 0.10,grepl('^131',code_2018) ~ 0.05,
    (grepl('^133',code_2018) | grepl('^134',code_2018)) ~ 0.00,grepl('^141',code_2018) ~ 0.25,grepl('^142',code_2018) ~ 0.05,
    grepl('^21',code_2018) ~ 0.05,grepl('^22',code_2018) ~ 0.60,grepl('^23',code_2018) ~ 0.20,grepl('^24',code_2018) ~ 0.50,
    grepl('^31',code_2018) ~ 0.60,grepl('^32',code_2018) ~ 1.00,grepl('^33',code_2018) ~ 0.20,grepl('^4',code_2018) ~ 0.50,
    grepl('^5',code_2018) ~ 0.00
  )))
# assign values of NS depending on the UA classes
ua_dis<-ua_dis %>%
  mutate(.,ns=with(.,case_when(
    grepl('^111',code_2018) ~ 0.10,grepl('^112',code_2018) ~ 0.30,(grepl('^113',code_2018) | grepl('^121',code_2018)) ~ 0.10,
    grepl('^122',code_2018) ~ 0.30,grepl('^123',code_2018) ~ 0.30,grepl('^124',code_2018) ~ 0.30,grepl('^131',code_2018) ~ 0.30,
    (grepl('^133',code_2018) | grepl('^134',code_2018)) ~ 0.10,grepl('^141',code_2018) ~ 0.30,grepl('^142',code_2018) ~ 0.30,
    grepl('^21',code_2018) ~ 0.20,grepl('^22',code_2018) ~ 0.40,grepl('^23',code_2018) ~ 0.60,grepl('^24',code_2018) ~ 0.40,
    grepl('^31',code_2018) ~ 0.80,grepl('^32',code_2018) ~ 0.90,grepl('^33',code_2018) ~ 0.30,grepl('^4',code_2018) ~ 0.30,
    grepl('^5',code_2018) ~ 0.00
  )))
# plot the values of dFA across the study area
library(ggplot2)
i8.1_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=fa),colour=NA) + 
  scale_fill_gradient(low='palegreen1',high='palegreen4',breaks=c(min(ua_dis$fa),max(ua_dis$fa)),
                      labels=c(sprintf("%#0.2f",round(min(ua_dis$fa),digits=2)),
                               sprintf("%#0.2f",round(max(ua_dis$fa),digits=2)))) + 
  labs(fill='Floral Availability',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# plot the values of NS across the study area
i8.2_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=ns),colour=NA) + 
  scale_fill_gradient(low='wheat1',high='wheat4',breaks=c(min(ua_dis$ns),max(ua_dis$ns)),
                      labels=c(sprintf("%#0.2f",round(min(ua_dis$ns),digits=2)),
                               sprintf("%#0.2f",round(max(ua_dis$ns),digits=2)))) + 
  labs(fill='Nesting Suitability',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# compute a weighted value of FA/NS by multipliying the values by the UA areas
ua_dis$i8<-as.double(ua_dis$fa*ua_dis$area_ua+ua_dis$ns*ua_dis$area_ua)
# calculate the mean FA/NS score per district
ua_mean_i8<-aggregate(ua_dis$i8,list(ua_dis$navn),sum)
# rename the column names to enable subsequent merge
names(ua_mean_i8)<-c('navn','i8')
# add the values of i8 to the districts data frame
dis_df<-merge(dis_df,ua_mean_i8,by='navn')
# weight the values of FA/NS per district by dividing by the area of the districts
dis_df$i8<-dis_df$i8/dis_df$area

#### SUPPORTING SERVICES ####

### i9
# subset the UA classes assimilable to GI
gi<-subset(ua_dis,grepl('^14',ua_dis$code_2018) | grepl('^21',ua_dis$code_2018) |
             grepl('^22',ua_dis$code_2018) | grepl('^23',ua_dis$code_2018) |
             grepl('^24',ua_dis$code_2018) | grepl('^31',ua_dis$code_2018) |
             grepl('^32',ua_dis$code_2018))
# create a column to classify GI areas according to the districts
gi$value<-ifelse(gi$navn=='Valby',1,
                 ifelse(gi$navn=='Osterbro',2,
                        ifelse(gi$navn=='Amager Ost',3,
                               ifelse(gi$navn=='Amager Vest',4,
                                      ifelse(gi$navn=='Vesterbro-Kongens Enghave',5,
                                             ifelse(gi$navn=='Norrebro',6,
                                                    ifelse(gi$navn=='Indre By',7,
                                                           ifelse(gi$navn=='Bronshoj-Husum',8,
                                                                  ifelse(gi$navn=='Bispebjerg',9,10)))))))))
# create a dummy raster with the extension of the GI layer and a resolution of 10 m
library(raster)
gi_r<-raster(gi,res=10)
# rasterize the polygon layer with the GI areas to enable calculating landscape metrics
gi_r<-rasterize(gi,gi_r,'value')
# determine the patches according to which the GI areas are distributed
library(landscapemetrics)
gi_pat<-get_patches(gi_r,directions=8,to_disk=T)
# compute all the landscape metrics available at a patch level
gi_l.met<-calculate_lsm(gi_r,level='patch')
# extract the values of patch area ("Area and edge metric")
gi_p.area<-subset(gi_l.met,metric=='area')
# compute the sum of patch areas per district
gi_p.area<-aggregate(gi_p.area$value,by=list(Category=gi_p.area$class),FUN=sum)
# extract the values of euclidean distance among patches ("Aggregation metric")
gi_p.enn<-subset(gi_l.met,metric=='enn')
# compute the sum of euclidean distance among patches per district
gi_p.enn<-aggregate(gi_p.enn$value,by=list(Category=gi_p.enn$class),FUN=sum)
# extract the values of contiguity index within the patches ("Shape metric")
gi_p.cont<-subset(gi_l.met,metric=='contig')
# extract the mean of contiguity index per district
gi_p.cont<-aggregate(gi_p.cont$value,by=list(Category=gi_p.cont$class),FUN=mean)
# add the ratio of patch areas to the total area of the districts
dis_df$i9.1<-gi_p.area$x/dis_df$area
# add the ratio of euclidean distances among patches to the total perimeter of the districts
dis_df$i9.2<-gi_p.enn$x/dis_df$perimeter
# add the mean of contiguity index
dis_df$i9.3<-gi_p.cont$x
# create a mosaic with the raster layers including the patches for all districts
mosaic.p<-do.call(merge,cbind(gi_pat[["layer_1"]]))
# create a color palette with (433) distinct colors
color<-grDevices::colors()[grep('gr(a|e)y',grDevices::colors(),invert=T)]
# convert to data frame to enable subsequent plotting
mosaic.p_df<-as.data.frame(mosaic.p,xy=T)
# remove NAs
mosaic.p_df<-subset(mosaic.p_df,!is.na(mosaic.p_df$layer))
# plot the patches
library(ggplot2)
i9_p<-ggplot() + geom_sf(data=dis_proj,colour='black',lwd=0,fill=alpha('white',0)) + 
  geom_tile(data=mosaic.p_df,aes(x=x,y=y,fill=layer)) +
  scale_fill_gradientn(colors=sample(color,max(gi_l.met$id)),na.value=NA,breaks=c(min(gi_l.met$id),max(gi_l.met$id)),
                       limits=c(min(gi_l.met$id),max(gi_l.met$id))) + 
  labs(fill='Landscape patches',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))

### i10
# read data with the number of species occurrences in the study area in 2012
species_gbif<-read.csv(file='0145821-220831081235567.csv',sep='\t')
# identify rows where the fields indiviualCount are either 0 or NA and occurrenceStatus is absent
absence_rows<-which(species_gbif$individualCount == 0 | is.na(species_gbif$individualCount) | 
                      species_gbif$occurrenceStatus %in% c('absent','Absent','ABSENT','ausente','Ausente','AUSENTE'))
# remove the absence rows from the species occurrence data frame
species_gbif<-species_gbif[-absence_rows,]
# remove any row whose coordinates are incomplete
library(scrubr)
species_gbif<-coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(species_gbif))))
# remove any record reporting coordinate uncertainty (location error, spatial resolution) larger than 5000 m
species_gbif<-coord_uncertain(species_gbif,coorduncertainityLimit=5000)
# transform the species occurrence data frame to sf
library(sf)
species_sf<-st_as_sf(species_gbif,coords=c('decimalLongitude','decimalLatitude'),remove=F)
# assign the crs of the districts to the species occurrence data frame
st_crs(species_sf)<-st_crs(dis)
# remove the rows whose combination of coordinates and taxon key are repeated 
# we seek for biodiversity, not species richness. Also, these rows might stem from multiple sources
species_sf<-species_sf[!duplicated(species_sf[c(22:23,34)]),]
# transform species occurrence sf to st to enable subsequent point count
species_st<-as(species_sf,'Spatial')[dis,]
# convert to data frame to enable subsequent plotting
species_df<-cbind(species_st@data,species_st@coords)
# subset the data frame to keep only the two most common species 
library(tidyverse)
species_main_12<-species_df %>% 
  add_count(taxonKey) %>% 
  filter(dense_rank(-n)<3)
# subset the data frame to keep only the third and fourth most common species
species_main_34<-species_df %>% 
  add_count(taxonKey) %>% 
  filter(dense_rank(-n)>2 & dense_rank(-n)<5)
# plot the distribution of the most common species across the districts
library(ggplot2)
i10.1_p<-ggplot() + geom_sf(data=st_as_sf(dis),aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  geom_point(aes(x=species_main_12$decimalLongitude,y=species_main_12$decimalLatitude,
                 fill=as.factor(species_main_12$taxonKey),shape=as.factor(species_main_12$taxonKey)),colour=alpha('white',0)) +
  scale_shape_manual(values=c(22,23),labels=unique(species_main_12$taxonKey),na.value=NA,name='Taxon key') +
  scale_fill_manual(values=c('goldenrod3','darkseagreen4'),
                    labels=unique(species_main_12$taxonKey),na.value=NA,name='Taxon key') +
  labs(point='Taxon key',colour='Districts') + 
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm'))
i10.2_p<-ggplot() + geom_sf(data=st_as_sf(dis),aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  geom_point(aes(x=species_main_34$decimalLongitude,y=species_main_34$decimalLatitude,
                                   fill=as.factor(species_main_34$taxonKey),shape=as.factor(species_main_34$taxonKey)),
                               colour=alpha('white',0)) +
  scale_shape_manual(values=c(21,24),labels=unique(species_main_34$taxonKey),na.value=NA,name='Taxon key') +
  scale_fill_manual(values=c('darkorange4','deepskyblue4'),
                    labels=unique(species_main_34$taxonKey),na.value=NA,name='Taxon key') +
  labs(point='Taxon key',colour='Districts') + 
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm'))

# arrange the points with the observations according to the districts
species_dis<-over(species_st,dis)
# transform to table to get the number of occurrences (different coordinates and taxon key) per district
species_dis<-table(species_dis$navn)
# convert the number of occurrences per district to data frame
species_dis<-as.data.frame(species_dis)
# rename the columns of the data frame to enable subsequent merge
names(species_dis)<-c('navn','i10')
# add the values of i10 to the districts data frame
dis_df<-merge(dis_df,species_dis,by='navn')

### i11
# read the Natura 2000, Emerald Network and CDDA areas for 2012
n2k<-st_read('natural_sites/Natura2000_end2021.gpkg',layer='NaturaSite_polygon')
emerald<-st_read('natural_sites/Emerald_2021.gpkg',layer='EmeraldSite_polygon')
cdda<-st_read('natural_sites/CDDA_2022_v01_public_EuropeEPSG3035.gpkg',layer='ProtectedSite')
# clip the protected sites to the boundaries enclosed by the districts
n2k_dis<-st_intersection(n2k,dis_proj)
emerald_dis<-st_intersection(emerald,dis_proj)
cdda_dis<-st_intersection(cdda,dis_proj)
# keep only the geometry column for plotting
n2k_p<-n2k_dis[,ncol(n2k_dis)]
emerald_p<-emerald_dis[,ncol(emerald_dis)]
cdda_p<-cdda_dis[,ncol(cdda_dis)]
# add a column with the designation of natural site to enable subsequent merging
n2k_p$designation<-'Natura 2000'
# emerald_p$designation<-'Emerald'
cdda_p$designation<-'CDDA'
# merge the three kinds of natural sites to enable subsequent plotting
nat_sites<-rbind(n2k_p,emerald_p,cdda_p)
# plot the natural sites across the districts
library(ggplot2)
i11_p<-ggplot() + geom_sf(data=nat_sites,mapping=aes(fill=designation),colour=NA) + 
  scale_fill_manual(values=c('forestgreen','lawngreen'),
                    na.value='transparent') + 
  labs(fill='Natural sites',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_legend(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# calculate the area of protected sites
n2k_dis$area_i11.1<-as.double(st_area(n2k_dis))
emerald_dis$area_i11.2<-as.double(st_area(emerald_dis))
cdda_dis$area_i11.3<-as.double(st_area(cdda_dis))
# calculate the sum protected sites per district
n2k_sum<-aggregate(n2k_dis$area_i11.1,list(n2k_dis$navn),sum)
# emerald_sum<-aggregate(emerald_dis$area_i11.2,list(emerald_dis$navn),sum)
cdda_sum<-aggregate(cdda_dis$area_i11.3,list(cdda_dis$navn),sum)
# rename the columns to enable subsequent merge
names(n2k_sum)<-c('navn','i11.1')
# names(emerald_sum)<-c('navn','i11.2')
names(cdda_sum)<-c('navn','i11.3')
# add the values of i11 to the districts data frame
dis_df<-merge(dis_df,n2k_sum,by='navn',all=T)
# dis_df<-merge(dis_df,emerald_sum,by='navn')
dis_df<-merge(dis_df,cdda_sum,by='navn',all=T)
# sum the areas of protected sites across the districts
library(dplyr)
dis_df<-dis_df%>%mutate(dis_df,i11=rowSums(cbind(i11.1,i11.3),na.rm=T))

#### CULTURAL SERVICES ####

### i12
# subset the UA classes assimilable to GI
gi<-subset(ua_dis,grepl('^14',ua_dis$code_2018) | grepl('^21',ua_dis$code_2018) |
             grepl('^22',ua_dis$code_2018) | grepl('^23',ua_dis$code_2018) |
             grepl('^24',ua_dis$code_2018) | grepl('^31',ua_dis$code_2018) |
             grepl('^32',ua_dis$code_2018))
# create a list with the GI classes for each district
gi_spl<-split(gi,gi$navn)
# create a list with all the UA classes for each district
ua_spl<-split(ua_dis,ua_dis$navn)
# compute the centroids for the GI and UA classes for each district
gi_cent<-lapply(1:length(gi_spl),function(i){
  gCentroid(as(gi_spl[[i]],'Spatial'),byid=T)
})
ua_cent<-lapply(1:length(ua_spl),function(i){
  gCentroid(as(ua_spl[[i]],'Spatial'),byid=T)
})
# calculate the Euclidean distance between each GI class and each UA class for each district
library(proxy)
cl_gi<-lapply(1:length(ua_cent),function(i){
  dist(ua_cent[[i]]@coords,gi_cent[[i]]@coords,'euclidean')
})
# determine the distance from each UA class to the closest GI class
cl_gi.min<-lapply(1:length(cl_gi),function(i){
  as.double(matrixStats::rowMins(cl_gi[[i]]))
})
# add a column to the UA class per district data with the minimum distances
for (i in 1:length(ua_spl)){
  ua_spl[[i]]$cl_gi.min<-cl_gi.min[[i]]
}
# weight the minimum distances to GI by the population estimates for each UA class
for (i in 1:length(ua_spl)){
  ua_spl[[i]]$cl_gi.w<-ua_spl[[i]]$cl_gi/ua_spl[[i]]$Pop2018
}
# transform Infs and NaNs to NAs
for (i in 1:length(ua_spl)){
  ua_spl[[i]]$cl_gi.w[is.infinite(ua_spl[[i]]$cl_gi.w)]<-NA
  ua_spl[[i]]$cl_gi.w[is.nan(ua_spl[[i]]$cl_gi.w)]<-NA
}
# convert the list with the minimum distances from UA classes to GI classes to data frame to enable subsequent plotting
ua_spl.df<-do.call(rbind.data.frame,ua_spl)
# convert distances to km
ua_spl.df$cl_gi.min<-ua_spl.df$cl_gi.min/1000
# plot the minimum distances from each UA class to the closest GI class
library(ggplot2)
i12_p<-ggplot() + geom_sf(data=ua_spl.df,mapping=aes(fill=cl_gi.min),colour=NA) + 
  scale_fill_gradient(low='darkolivegreen4',high='darkolivegreen1',breaks=c(min(ua_spl.df$cl_gi.min),max(ua_spl.df$cl_gi.min)),
                      labels=c(sprintf("%0.2f",round(min(ua_spl.df$cl_gi.min),digits=3)),
                               sprintf("%0.2f",round(max(ua_spl.df$cl_gi.min),digits=3)))) + 
  labs(fill='Distance to GI (km)',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# calculate the mean minimum distance per UA class for each district
cl_gi.mean<-lapply(1:length(ua_spl),function(i){
  mean(ua_spl[[i]][["cl_gi.w"]],na.rm=T)
})
# create a data frame with the mean minimum distances per district
cl_gi.mean<-data.frame(names(ua_spl),as.double(cl_gi.mean))
# rename the columns of the data frame to enable subsequent merge
names(cl_gi.mean)<-c('navn','i12')
# add the values of i13 to the districts data frame
dis_df<-merge(dis_df,cl_gi.mean,by='navn',all=T)

### i13
# read raster with the DEM (res = 25 m)
library(raster)
dem<-raster('EUD_CP-DEMS_4500035000-AA.tif')
# crop the DEM to the extent of the districts
dem_crop<-crop(dem,dis_proj)
# compute the slope from the cropped DEM
slope<-terrain(dem_crop,opt='slope',unit='degrees',neighbors=8)
# mask the slope to the boundaries of the districts
slope_mask<-mask(slope,dis_proj)
# convert to data frame to enable subsequent plotting
slope_df<-as.data.frame(slope_mask,xy=T)
# remove NAs
slope_df<-slope_df[complete.cases(slope_df),]
# plot the slope across the study area
library(ggplot2)
i13_p<-ggplot() + geom_tile(data=slope_df,aes(x=x,y=y,fill=slope)) + 
  scale_fill_gradientn(colours=terrain.colors(5),breaks=c(min(slope_df$slope),max(slope_df$slope)),
                       labels=c(sprintf("%0.2f",round(min(slope_df$slope),digits=3)),
                                sprintf("%0.2f",round(max(slope_df$slope),digits=3))),na.value='transparent') + 
  labs(fill='Slope (º)',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# extract slope raster values per district to list
slope_dis<-raster::extract(slope_mask,dis_proj)
# calculate the mean slope for each district
slope_mean<-lapply(slope_dis,FUN=mean,na.rm=TRUE)
# add the slope mean values to the districts' data frame
dis_df$i13<-unlist(slope_mean)

### i14
##i14.1
# load the roads in Copenhagen
library(tidyverse)
library(osmdata)
library(sf)
library(httr2)
roads<-getbb('Copenhagen') %>% opq() %>% add_osm_feature('highway') %>% osmdata_sf()
# crop roads to the boundaries enclosed by the buffer area
roads_dis<-st_intersection(roads[['osm_lines']],st_as_sf(dis))
# reproject districts' centroids to match the projection of UA
roads_proj<-roads_dis%>%st_transform(3035)
# subset the types of roads of interest
roads_proj<-roads_proj[grepl('motorway',roads_proj$highway) | grepl('trunk',roads_proj$highway) |
                         grepl('primary',roads_proj$highway) | grepl('secondary',roads_proj$highway) |
                         grepl('tertiary',roads_proj$highway),]
# extract the necessary fields
roads_proj<-roads_proj[,c('osm_id','name','highway','id','navn','geometry')]
# create an empty raster with a resolution of 25 m to calculate distances from its cells to the roads
library(raster)
r<-raster(dis_proj,res=25)
# convert the raster to points
p<-as(r,'SpatialPoints')
# intersect with the districts to enable subsequent calculations per district
p<-st_intersection(dis_proj,st_as_sf(p))
# subset the roads depending on the type
motorway<-roads_proj[grepl('motorway',roads_proj$highway),]
trunk<-roads_proj[grepl('trunk',roads_proj$highway),]
primary<-roads_proj[grepl('primary',roads_proj$highway),]
secondary<-roads_proj[grepl('secondary',roads_proj$highway),]
tertiary<-roads_proj[grepl('tertiary',roads_proj$highway),]
# convert to list to loop throughout it in next steps
highway<-list(motorway,trunk,primary,secondary,tertiary)
# calculate the nearest type of road from each point in the empty raster
nearest<-lapply(1:length(highway),function(i){
  st_nearest_feature(p,highway[[i]])
})
# compute the distances from the nearest roads and the points
dist<-lapply(1:length(nearest),function(i){
  st_distance(p,highway[[i]][nearest[[i]],],by_element=T)
})
# convert the distances to data frame and rename
dist<-as.data.frame(dist)
names(dist)<-c('dist_motorway','dist_trunk','dist_primary','dist_secondary','dist_tertiary')
# convert the spatial points to data frame
road_dist<-st_as_sf(p)
# combine the points (coordinates, etc.) and their corresponding distances
road_dist<-cbind(road_dist,dist)
# add two columns with the X and Y coordinates to enable subsequent plotting
road_dist<-as.data.frame(cbind(st_coordinates(road_dist),road_dist[,1:(ncol(road_dist)-1)]))
# create a weight of vectors by ranking (5 - highest weight; 1 - lowest weight)
w<-c(5,4,3,2,1)
w<-w/max(w)
# allocate the ranked weight to each road type (from most to least weight: motorway > trunk > primary > secondary > tertiary)
road_dist[,9:13]<-mapply(`*`,road_dist[,9:13],w)
# convert distances to km
road_dist$dist_tertiary<-road_dist$dist_tertiary/1000
# plot the distances from each pixel in the study area to the closest tertiary road
library(ggplot2)
i14.1_p<-ggplot() + geom_tile(data=road_dist,aes(x=X,y=Y,fill=dist_tertiary)) + 
  scale_fill_distiller(palette='Spectral',breaks=c(min(road_dist$dist_tertiary),max(road_dist$dist_tertiary)),
                       labels=c(sprintf("%0.2f",round(min(road_dist$dist_tertiary),digits=3)),
                                sprintf("%0.2f",round(max(road_dist$dist_tertiary),digits=3))),na.value='transparent') + 
  labs(fill='Distance to tertiary roads (km)',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# determine the sum scores per pixel by aggregating all the types of roads
road_dist$dist_total<-rowSums(road_dist[,9:13])
# calculate the sum distance to roads per district
i14.1<-aggregate(road_dist$dist_total,list(road_dist$navn),sum)
# rename the columns to enable the subsequent merge with the districts data frame
colnames(i14.1)<-c('navn','i14.1')
# add the values of i114.1 to the districts data frame
dis_df<-merge(dis_df,i14.1,by='navn')
##i14.2
# load the railways in Copenhagen
rails<-getbb('Copenhagen') %>% opq() %>% add_osm_feature('railway') %>% osmdata_sf()
# crop railways to the boundaries enclosed by the buffer area
rails_dis<-st_intersection(rails[['osm_lines']],st_as_sf(dis))
# reproject railways' centroids to match the projection of UA
rails_proj<-rails_dis%>%st_transform(3035)
# subset the types of railways of interest
rails_proj<-rails_proj[grepl('rail',rails_proj$railway) | grepl('light_rail',rails_proj$railway),]
# extract the necessary fields 
rails_proj<-rails_proj[,c('osm_id','name','railway','id','navn','geometry')]
# subset the railways depending on the type
rail<-rails_proj[grepl('rail',rails_proj$railway),]
light_rail<-rails_proj[grepl('light_rail',rails_proj$railway),]
# convert to list to loop throughout it in next steps
railway<-list(rail,light_rail)
# calculate the nearest type of road from each point in the empty raster
nearest<-lapply(1:length(railway),function(i){
  st_nearest_feature(p,railway[[i]])
})
# compute the distances from the nearest roads and the points
dist<-lapply(1:length(nearest),function(i){
  st_distance(p,railway[[i]][nearest[[i]],],by_element=T)
})
# convert the distances to data frame and rename
dist<-as.data.frame(dist)
names(dist)<-c('dist_rail','dist_lightrail')
# convert the spatial points to data frame
rail_dist<-st_as_sf(p)
# combine the points (coordinates, etc.) and their corresponding distances
rail_dist<-cbind(rail_dist,dist)
# add two columns with the X and Y coordinates to enable subsequent plotting
rail_dist<-as.data.frame(cbind(st_coordinates(rail_dist),rail_dist[,1:(ncol(rail_dist)-1)]))
# create a weight of vectors by ranking (2 - highest weight; 1 - lowest weight)
w<-c(2,1)
w<-w/max(w)
# allocate the ranked weight to each railway type (from most to least weight: railway > light railway)
rail_dist[,9:10]<-mapply(`*`,rail_dist[,9:10],w)
# convert distances to km
rail_dist$dist_rail<-rail_dist$dist_rail/1000
# plot the distances from each pixel in the study area to the closest railway
library(ggplot2)
i14.2_p<-ggplot() + geom_tile(data=rail_dist,aes(x=X,y=Y,fill=dist_rail)) + 
  scale_fill_distiller(palette='Spectral',breaks=c(min(rail_dist$dist_rail),max(rail_dist$dist_rail)),
                       labels=c(sprintf("%0.2f",round(min(rail_dist$dist_rail),digits=3)),
                                sprintf("%0.2f",round(max(rail_dist$dist_rail),digits=3))),na.value='transparent') + 
  labs(fill='Distance to railways (km)',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# determine the sum scores per pixel by aggregating all the types of railways
rail_dist$dist_total<-rowSums(rail_dist[,9:10])
# calculate the sum distance to rails per district
i14.2<-aggregate(rail_dist$dist_total,list(rail_dist$navn),sum)
# rename the columns to enable the subsequent merge with the districts data frame
colnames(i14.2)<-c('navn','i14.2')
# add the values of i14.2 to the districts data frame
dis_df<-merge(dis_df,i14.2,by='navn')
##i14.3
# load the amenities in Copenhagen
leisure<-getbb('Copenhagen') %>% opq() %>% add_osm_feature('amenity') %>% osmdata_sf()
# crop amenities to the boundaries enclosed by the buffer area
leisure_dis<-st_intersection(leisure[['osm_points']],st_as_sf(dis))
# reproject districts' centroids to match the projection of UA
leisure_proj<-leisure_dis%>%st_transform(3035)
# subset the types of roads of interest
leisure_proj<-leisure_proj[grepl('bar',leisure_proj$amenity) | grepl('casino',leisure_proj$amenity) | 
                             grepl('events_venue',leisure_proj$amenity) | grepl('nightclub',leisure_proj$amenity) | 
                             grepl('pub',leisure_proj$amenity),]
# extract the necessary fields 
leisure_proj<-leisure_proj[,c('osm_id','name','amenity','id','navn','geometry')]
# subset the amenities depending on the type
bar<-leisure_proj[grepl('bar',leisure_proj$amenity),]
casino<-leisure_proj[grepl('casino',leisure_proj$amenity),]
events_venue<-leisure_proj[grepl('events_venue',leisure_proj$amenity),]
nightclub<-leisure_proj[grepl('nightclub',leisure_proj$amenity),]
pub<-leisure_proj[grepl('pub',leisure_proj$amenity),]
# convert to list to loop throughout it in next steps
leisure<-list(bar,casino,events_venue,nightclub,pub)
# calculate the nearest type of amenity from each point in the empty raster
nearest<-lapply(1:length(leisure),function(i){
  st_nearest_feature(p,leisure[[i]])
})
# compute the distances from the nearest amenities and the points
dist<-lapply(1:length(nearest),function(i){
  st_distance(p,leisure[[i]][nearest[[i]],],by_element=T)
})
# convert the distances to data frame and rename
dist<-as.data.frame(dist)
names(dist)<-c('dist_bar','dist_casino','dist_events_venue','dist_nightclub','dist_pub')
# convert the spatial points to data frame
leisure_dist<-st_as_sf(p)
# combine the points (coordinates, etc.) and their corresponding distances
leisure_dist<-cbind(leisure_dist,dist)
# add two columns with the X and Y coordinates to enable subsequent plotting
leisure_dist<-as.data.frame(cbind(st_coordinates(leisure_dist),leisure_dist[,1:(ncol(leisure_dist)-1)]))
# convert units (m) to simply (general) double
leisure_dist[,9:13]<-mapply(`*`,leisure_dist[,9:13],1)
# convert distances to km
leisure_dist$dist_pub<-leisure_dist$dist_pub/1000
# plot the distances from each pixel in the study area to the closest tertiary road
library(ggplot2)
i14.3_p<-ggplot() + geom_tile(data=leisure_dist,aes(x=X,y=Y,fill=dist_pub)) + 
  scale_fill_distiller(palette='Spectral',breaks=c(min(leisure_dist$dist_pub),max(leisure_dist$dist_pub)),
                       labels=c(sprintf("%0.2f",round(min(leisure_dist$dist_pub),digits=3)),
                                sprintf("%0.2f",round(max(leisure_dist$dist_pub),digits=3))),na.value='transparent') + 
  labs(fill='Distance to pubs (km)',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# determine the sum scores per pixel by aggregating all the types of amenities
leisure_dist$dist_total<-rowSums(leisure_dist[,9:13])
# calculate the sum distance to amenities per district
i14.3<-aggregate(leisure_dist$dist_total,list(leisure_dist$navn),sum)
# rename the columns to enable the subsequent merge with the districts data frame
colnames(i14.3)<-c('navn','i14.3')
# add the values of i14.3 to the districts data frame
dis_df<-merge(dis_df,i14.3,by='navn')
## i14.1+i14.2+14.3
# create a vector of weights with the noise thresholds for each source
w<-c(1/53,1/54,1/70)
w<-w/sum(w)
# allocate the weight to each sub-indicator (from most to least weight: roads > railways > leisure)
dis_df[,c('i14.1','i14.2','i14.3')]<-dis_df[,c('i14.1','i14.2','i14.3')]*w[col(dis_df[,c('i14.1','i14.2','i14.3')])]

### i15
# allocate values of LCN to the UA classes
library(dplyr)
ua_dis<-ua_dis %>%
  mutate(.,i15_sc=with(.,case_when(
    (grepl('^111',code_2018) | grepl('^1121',code_2018) | grepl('^1122',code_2018) | 
       grepl('^12',code_2018) | grepl('^13',code_2018)) ~ 0,
    (grepl('^1123',code_2018) | grepl('^1124',code_2018) | grepl('^14',code_2018) | grepl('^33',code_2018)) ~ 10,
    (grepl('^21',code_2018) | grepl('^25',code_2018)) ~ 30,grepl('^22',code_2018) ~ 40,grepl('^23',code_2018) ~ 50,
    grepl('^24',code_2018) ~ 60,grepl('^32',code_2018) ~ 70,
    (grepl('^31',code_2018) | grepl('^4',code_2018) | grepl('^5',code_2018)) ~ 100,
  )))
# plot the values of LCN across the study area
library(ggplot2)
i15_p<-ggplot() + geom_sf(data=ua_dis,mapping=aes(fill=i15_sc),colour=NA) + 
  scale_fill_gradient(low='darkolivegreen1',high='darkolivegreen4',breaks=c(min(ua_dis$i15_sc),max(ua_dis$i15_sc))) + 
  labs(fill='Naturalness',colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(4475425,4487696)) + scale_y_continuous(limits=c(3614989,3627896)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# compute a weighted value of i15 by multipliying its values by the UA areas
ua_dis$i15<-as.double(ua_dis$i15_sc*ua_dis$area_ua)
# calculate the sum i15 score per district
ua_sum_i15<-aggregate(ua_dis$i15,list(ua_dis$navn),sum)
# rename the column names to enable subsequent merge
names(ua_sum_i15)<-c('navn','i15')
# add the values of i15 to the districts data frame
dis_df<-merge(dis_df,ua_sum_i15,by='navn')
# weight the values of i15 per district by dividing by the area of the districts
dis_df$i15<-dis_df$i15/dis_df$area

#### MCDM ####
# normalize and aggregate the composite indicators
dis_df$i5<-dis_df$i5.1/max(dis_df$i5.1)+dis_df$i5.2/max(dis_df$i5.2)+dis_df$i5.3/max(dis_df$i5.3)
dis_df$i9<-dis_df$i9.1/max(dis_df$i9.1)+dis_df$i9.2/max(dis_df$i9.2)+dis_df$i9.3/max(dis_df$i9.3)
# calculate the mean minimum distance to GI per district (not weighted per population)
dis_df$i14.4<-unlist(lapply(1:length(cl_gi.min),function(i){
  mean(cl_gi.min[[i]],na.rm=T)
}))
dis_df$navn[dis_df$navn=='Vesterbro-Kongens Enghave']<-'Vesterbro'
dis_df$i14<-dis_df$i14.1/max(dis_df$i14.1)+dis_df$i14.2/max(dis_df$i14.2)+dis_df$i14.3/max(dis_df$i14.3)+min(dis_df$i14.4)/dis_df$i14.4
# subset and sort the data frame with the indicators
dis_df<-dis_df[,c(1,7,8,9,10,31,14,15,16,32,20,23,24,25,33,29)]
# transform the indicator about the species (counts - integer) to numeric
dis_df$i10<-as.numeric(unlist(dis_df$i10))
# subset the data frame with the indicators by dropping the geometry column
library(sf)
dis_l<-st_drop_geometry(dis_df)

## MEREC
# normalize the data according to the MEREC method
dis_l[,c(2:7,9:12,15:16)]<-apply(dis_l[,c(2:7,9:12,15:16)],2,function(x){min(x)/x})
dis_l[,c(8,13:14)]<-apply(dis_l[,c(8,13:14)],2,function(x){x/max(x)})
# subset the data according to the ES groups
dis_l<-list(dis_l[2:4],dis_l[5:9],dis_l[10:12],dis_l[13:16])
# calculate the overall performance of the districts
s<-lapply(1:length(dis_l),function(i){
  log(1+((1/ncol(dis_l[[i]]))*rowSums(abs(log(dis_l[[i]])))))
})
# calculate the overall performance of the districts by removing each indicator
s1<-c()
for (i in 1:length(dis_l)){
  s1[[i]]<-lapply(1:ncol(dis_l[[i]]),function(j){
    log(1+((1/ncol(dis_l[[i]]))*rowSums(abs(log(subset(dis_l[[i]],select=-c(j)))))))
  })
}
# compute the absolute deviations
E<-c()
for (i in 1:length(s1)){
  E[[i]]<-lapply(1:length(s1[[i]]),function(j){
    sum(abs(s1[[i]][[j]]-s[[i]]))
  })
}
# determine the weights of the indicators
w<-lapply(1:length(E),function(i){
  unlist(E[[i]])/sum(unlist(E[[i]]))
})

## TOPSIS
library(topsis)
# subset the data according to the ES groups
dis_l<-st_drop_geometry(dis_df)
dis_l<-list(dis_l[2:4],dis_l[5:9],dis_l[10:12],dis_l[13:16])
# remove the geomtry field
d<-lapply(1:length(dis_l),function(i){
  as.matrix(st_drop_geometry(dis_l[[i]]))
})
# set the benefit and cost indicators
i<-list(c('+','+','+'),c('+','+','+','-','+'),c('+','+','+'),c('-','-','+','+'))
# apply the topsis method
rc<-lapply(1:length(d),function(j){
  topsis(d[[j]],w[[j]],i[[j]])
})
# obtain values of RC per ES group
dis_df$rc_prov<-rc[[1]]$score
dis_df$rc_reg<-rc[[2]]$score
dis_df$rc_supp<-rc[[3]]$score
dis_df$rc_cult<-rc[[4]]$score
# aggregate the values of RC with equal weights for the ES groups
dis_df$rc_es<-dis_df$rc_prov*0.25+dis_df$rc_reg*0.25+dis_df$rc_supp*0.25+dis_df$rc_cult*0.25
# repeat the process for equal weights within the ES groups
d_eq<-as.matrix(st_drop_geometry(dis_df[,2:16]))
# set the benefit and cost indicators
i_eq<-c('+','+','+','+','+','+','-','+','+','+','+','-','-','+','+')
# set equal weights per ES group
w_eq<-c(1/4*1/3,1/4*1/3,1/4*1/3,
         1/4*1/5,1/4*1/5,1/4*1/5,1/4*1/5,1/4*1/5,
         1/4*1/3,1/4*1/3,1/4*1/3,
         1/4*1/4,1/4*1/4,1/4*1/4,1/4*1/4)
# apply the topsis method
rc_eq<-topsis(d_eq,w_eq,i_eq)
# obtain values of RC
dis_df$rc_eq<-rc_eq$score

#### PLOTS ####
# set the path where my plots are to be saved
library(ggpubr)
setwd('path where my plots are to be saved')
# save the provisioning services plots
set.seed(123)
jpeg('i_prov.jpg',width=13.5,height=24,units='cm',res=300)
ggarrange(i1_p,i2_p,i3_p,
          labels=c('(a)','(b)','(c)'),font.label=list(size=10,face='plain'),
          ncol=1,nrow=3,align='hv')
dev.off()
# save the regulating services plots (1/2)
set.seed(123)
jpeg('i_reg.1.jpg',width=13.6,height=15.4,units='cm',res=300)
ggarrange(i4_p,i5.1_p,i5.2_p,i5.3_p,
          labels=c('(a)','(b)','(c)','(d)'),font.label=list(size=10,face='plain'),
          ncol=2,nrow=2,align='hv')
dev.off()
# save the regulating services plots (2/2)
set.seed(123)
jpeg('i_reg.2.jpg',width=13.6,height=15.4,units='cm',res=300)
ggarrange(i6_p,i7_p,i8.1_p,i8.2_p,
          labels=c('(a)','(b)','(c)','(d)'),font.label=list(size=10,face='plain'),
          ncol=2,nrow=2,align='hv')
dev.off()
# save the supporting services plots
set.seed(123)
jpeg('i_supp.jpg',width=13.6,height=15.4,units='cm',res=300)
ggarrange(i9_p,i10.1_p,i10.2_p,i11_p,
          labels=c('(a)','(b)','(c)','(d)'),font.label=list(size=10,face='plain'),
          ncol=2,nrow=2,align='hv')
dev.off()
# save the cultural services plots
set.seed(123)
jpeg('i_cult.jpg',width=13.6,height=23.1,units='cm',res=300)
ggarrange(i12_p,i13_p,i14.1_p,i14.2_p,i14.3_p,i15_p,
          labels=c('(a)','(b)','(c)','(d)','(e)','(f)'),font.label=list(size=10,face='plain'),
          ncol=2,nrow=3,align='hv')
dev.off()
# plot the values of RC per district
library(ggplot2)
# only provisioning ES
p_prov<-ggplot(dis_df) + geom_sf(aes(fill=rc_prov)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_prov),max(dis_df$rc_prov)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_prov),digits=3)),
                      sprintf("%0.2f",round(max(dis_df$rc_prov),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_prov,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
         geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# only regulating ES
p_reg<-ggplot(dis_df) + geom_sf(aes(fill=rc_reg)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_reg),max(dis_df$rc_reg)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_reg),digits=3)),
                               sprintf("%0.2f",round(max(dis_df$rc_reg),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_reg,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# only supporting ES
p_supp<-ggplot(dis_df) + geom_sf(aes(fill=rc_supp)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_supp),max(dis_df$rc_supp)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_supp),digits=3)),
                               sprintf("%0.2f",round(max(dis_df$rc_supp),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_supp,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# only cultural ES
p_cult<-ggplot(dis_df) + geom_sf(aes(fill=rc_cult)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_cult),max(dis_df$rc_cult)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_cult),digits=3)),
                               sprintf("%0.2f",round(max(dis_df$rc_cult),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_cult,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# all ES (MEREC)
p_es<-ggplot(dis_df) + geom_sf(aes(fill=rc_es)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_es),max(dis_df$rc_es)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_es),digits=3)),
                               sprintf("%0.2f",round(max(dis_df$rc_es),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_es,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# all ES (equal weights)
p_eq<-ggplot(dis_df) + geom_sf(aes(fill=rc_eq)) +
  scale_fill_gradient(low='gray',high='greenyellow',breaks=c(min(dis_df$rc_eq),max(dis_df$rc_eq)),
                      labels=c(sprintf("%0.2f",round(min(dis_df$rc_eq),digits=3)),
                               sprintf("%0.2f",round(max(dis_df$rc_eq),digits=3)))) +
  geom_sf_text(aes(label=sprintf("%0.2f",round(rc_eq,digits=3))),colour='black',size=2.5) +
  labs(fill=(bquote(RC[i])),colour='Districts') + 
  geom_sf(data=dis_proj,aes(color='A'),fill=alpha('white',0),show.legend='polygon',lwd=0.4) +
  scale_color_manual(values=c('A'='tomato'),labels=NULL) +
  scale_x_continuous(limits=c(12.457,12.647)) + scale_y_continuous(limits=c(55.615,55.73)) + labs(x='') + labs(y='') +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.text=element_text(size=9,margin=margin(t=-22.5)),
        legend.title=element_text(size=9),legend.position='bottom',legend.spacing=unit(-0.2,'cm'),legend.box='vertical',
        legend.key.size=unit(0.2,'cm'),plot.margin=margin(0.1,0,-0.1,0,'cm')) + 
  guides(fill=guide_colorbar(order=1,title.vjust=1),color=guide_legend(order=2,title.vjust=1))
# save the RC plots
set.seed(123)
jpeg('topsis.jpg',width=13.6,height=23.1,units='cm',res=300)
ggarrange(p_prov,p_reg,p_supp,p_cult,p_es,p_eq,
          labels=c('(a)','(b)','(c)','(d)','(e)','(f)'),font.label=list(size=10,face='plain'),
          ncol=2,nrow=3,align='hv')
dev.off()
