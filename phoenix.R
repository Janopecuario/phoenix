#0.PREPARACIÓN####
packages<-c("tidyverse","sp","rgdal",
            "raster","DHARMa","mgcv","fields","viridis","leaflet",
            "htmltools","htmlwidgets")
sapply(packages,require,character.only=TRUE,quietly=TRUE)
projectionUTM<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
projectiongeo<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
grancanaria<-readOGR("~/rhamnus/islas.shp")
grancanaria<-subset(grancanaria, nombre=="GRAN CANARIA") %>% spTransform(projectiongeo)
mdt<-readOGR("Height_Malla.shp") %>% spTransform(projectiongeo)
mdtRUTM<-mdt %>% as.data.frame() %>% select(c(2:4)) %>% 
  rasterFromXYZ(crs=projectionUTM) 
mdtR<-projectRaster(from=mdtRUTM,crs=projectiongeo)
aspect<-raster::terrain(mdtR,opt="aspect",unit="degrees") %>% cos()
mdtgeo<-readOGR("heightgeo.shp") %>% raster() %>% plot()
mdtR<-mdtgeo %>% spTransform(projectiongeo) %>% 
  as.data.frame() %>% select(c(2:4)) %>% 
  rasterFromXYZ(crs=projectiongeo)

terreno<-mdtR %>% as.data.frame(xy=TRUE)
terreno<-aspect %>% as.data.frame() %>% cbind(terreno,.) %>% 
  rename(z=h_mean,asp=layer)

centroides<-readOGR("Centroides_Malla_H.shp")
centroidesUTM<-spTransform(centroides,projectiongeo) %>% 
  as.data.frame() %>% select(c(2:4)) %>% 
  rasterFromXYZ(crs=projectionUTM)
centroidesgeo<-projectRaster(from=centroidesUTM,crs=projectiongeo)
aspect<-raster::terrain(centroidesgeo,opt="aspect",unit="degrees") %>% cos()

mdt25<-raster("136_MDT25_GC.tif") %>% 
  projectRaster(crs=projectiongeo) %>% 
  crop(grancanaria) %>% mask(grancanaria)
aspect<-raster::terrain(mdt25,opt="aspect",unit="degrees") %>% 
  cos()  

terreno<-mdt25 %>% as.data.frame(xy=TRUE)
terreno<-aspect %>% as.data.frame() %>% cbind(terreno,.) %>% 
  rename(z=X136_MDT25_GC,asp=layer)

res<-res(mdtR)
mdt500<-raster("mdt500.tif") 
#mdt500<-projectRaster(from=mdt500,crs=projectiongeo)
aspect<-raster::terrain(mdt500,opt="aspect",unit="degrees") %>% 
  cos()  
terreno<-mdt500 %>% as.data.frame(xy=TRUE)
terreno<-aspect %>% as.data.frame() %>% cbind(terreno,.) %>% 
  rename(z=mdt500,asp=layer)

#tmean####

tmean<-read_delim("TEMP.MEDIA MENSUAL.CSV",delim=";") %>% 
  subset(NOM_PROV=="LAS PALMAS") %>% 
  subset(x<15*-1 & y>27.3) %>%
  select(-c(5:8,10:12)) %>% 
  select(-c("NOMBRE","ALTITUD","Media","NOM_PROV")) %>% 
  group_by(Indicativo) %>% 
  summarise_all(mean,na.rm=TRUE)

coordinates(tmean)=~x+y
crs(tmean)<-projectiongeo
tmeanUTM <- spTransform(tmean, projectionUTM) %>% 
  as.data.frame() %>% 
  relocate(x,y) %>% select(-c(Indicativo,Year)) %>% 
  na.omit()

tmean_mdt<-raster::extract(mdt500,tmeanUTM[1:2])
tmean_aspect<-raster::extract(aspect,tmeanUTM[2:3])
tmeanUTM<-cbind(tmeanUTM,tmean_mdt,tmean_aspect) %>% 
  rename(z=tmean_mdt,asp=tmean_aspect) %>% tibble()
                    
#bucle de temperatura media####
meses<-names(tmeanUTM[3:14])
for(m in 3:14){
  mes<-meses[m-2]
  tmean_sub<-tmeanUTM %>% 
    dplyr::select(x,y,z,asp,m) #%>% na.omit()
  predictores<-tmeanUTM %>% dplyr::select(x,y,z,asp) %>% 
    replace_na(list(asp=0))
  responsetemp<-tmeanUTM %>% select(m)
  datatemp<-cbind(responsetemp,predictores) %>% rename(response=1)
  #tmeanenero<-Tps(x=predictores,Y=tmeanUTM$enero,miles=F)
  print(paste("modeling",mes))
  gamtemp<-mgcv::gam(response~s(z)+s(y)+s(x),data=datatemp)
  predicttemp<-cbind(terreno,predict(gamtemp,terreno)) %>% 
    rename(prediction=5) %>% select(1,2,5) %>% 
    rasterFromXYZ(crs=projectionUTM)
  print(paste("Writing",mes))
  writeRaster(predicttemp,paste0("tmean",mes,".tiff"),overwrite=TRUE)
  #predicteneroUTM<-projectRaster(from=predictenero,crs=projectionUTM)
  #writeRaster(predicteneroUTM,"tmeaneneroUTM.tiff",overwrite=TRUE)  
}

#tmax####

tmax<-read_delim("TEMP.MAX.MENSUAL.CSV",delim=";") %>% 
  subset(NOM_PROV=="LAS PALMAS") %>% 
  subset(x<15*-1 & y>27.3) %>%
  select(-c(5:7,9:11)) %>% 
  select(-c("NOMBRE","ALTITUD","NOM_PROV")) %>% 
  group_by(Indicativo) %>% 
  summarise_all(mean,na.rm=TRUE)

coordinates(tmax)=~x+y
crs(tmax)<-projectiongeo
tmaxUTM <- spTransform(tmax, projectionUTM) %>% 
  as.data.frame() %>% 
  relocate(x,y) %>% select(-c(Indicativo,Year)) %>% 
  na.omit()

tmax_mdt<-raster::extract(mdt500,tmaxUTM[1:2])
tmax_aspect<-raster::extract(aspect,tmaxUTM[2:3])
tmaxUTM<-cbind(tmaxUTM,tmax_mdt,tmax_aspect) %>% 
  rename(z=tmax_mdt,asp=tmax_aspect) %>% tibble()

#bucle de temperatura máxima
meses<-names(tmeanUTM[3:14])
for(m in 3:14){
  nmes<-m-2
  mes<-meses[m-2]
  tmax_sub<-tmaxUTM %>% 
    dplyr::select(x,y,z,asp,m) #%>% na.omit()
  predictores<-tmaxUTM %>% dplyr::select(x,y,z,asp) %>% 
    replace_na(list(asp=0))
  responsetemp<-tmaxUTM %>% select(m)
  datatemp<-cbind(responsetemp,predictores) %>% rename(response=1)
  #tmaxenero<-Tps(x=predictores,Y=tmaxUTM$enero,miles=F)
  print(paste("modeling",mes))
  gamtemp<-mgcv::gam(response~s(z)+s(y)+s(x),data=datatemp)
  predicttemp<-cbind(terreno,predict(gamtemp,terreno)) %>% 
    rename(prediction=5) %>% select(1,2,5) %>% 
    rasterFromXYZ(crs=projectionUTM)
  print(paste("Writing",mes))
  writeRaster(predicttemp,paste0("tmax",nmes,".tiff"),overwrite=TRUE)
}

#temp min####
tmin<-read_delim("TEMP.MIN.MENSUAL.CSV",delim=";") %>% 
  subset(NOM_PROV=="LAS PALMAS") %>% 
  subset(x<15*-1 & y>27.3) %>%
  select(-c("NOMBRE","z","NOM_PROV","Year")) %>% 
  group_by(Indicativo) %>% 
  #na.omit() %>% 
  summarise_all(mean,na.rm=TRUE)

coordinates(tmin)=~x+y
crs(tmin)<-projectiongeo
tminUTM <- spTransform(tmin, projectionUTM) %>% 
  as.data.frame() %>% 
  relocate(x,y) %>% select(-c(Indicativo)) %>% 
  na.omit()

tmin_mdt<-raster::extract(mdt500,tminUTM[1:2])
tmin_aspect<-raster::extract(aspect,tminUTM[2:3])
tminUTM<-cbind(tminUTM,tmin_mdt,tmin_aspect) %>% 
  rename(z=tmin_mdt,asp=tmin_aspect) %>% tibble()

#bucle de temperatura minima
meses<-names(tmeanUTM[3:14])

for(m in 3:14){
  mes<-meses[m-2]
  nmes<-m-2
  tmin_sub<-tminUTM %>% 
    dplyr::select(x,y,z,asp,m) #%>% na.omit()
  predictores<-tminUTM %>% dplyr::select(x,y,z,asp) %>% 
    replace_na(list(asp=0))
  responsetemp<-tminUTM %>% select(m)
  datatemp<-cbind(responsetemp,predictores) %>% rename(response=1)
  #tminenero<-Tps(x=predictores,Y=tminUTM$enero,miles=F)
  print(paste("modeling",mes))
  gamtemp<-mgcv::gam(response~s(z)+s(y)+s(x),data=datatemp)
  predicttemp<-cbind(terreno,predict(gamtemp,terreno)) %>% 
    rename(prediction=5) %>% select(1,2,5) %>% 
    rasterFromXYZ(crs=projectionUTM)
  print(paste("Writing",mes))
  writeRaster(predicttemp,paste0("tmin",nmes,".tiff"),overwrite=TRUE)
}

#prec####
prec<-read_delim("PRECIP.MENSUALES.CSV",delim=";") %>% 
  
  subset(NOM_PROV=="LAS PALMAS") %>% 
  subset(x<15*-1 & y>27.3) %>%
  select(-c("NOMBRE","NOM_PROV","Year")) %>% 
  select(-c("LO","NG","IT","UD","LA","TI","TUD")) %>% 
  group_by(Indicativo) %>% 
  #na.omit() %>% 
  summarise_all(mean,na.rm=TRUE)

coordinates(prec)=~x+y
crs(prec)<-projectiongeo
precUTM <- spTransform(prec, projectionUTM) %>% 
  as.data.frame() %>% 
  relocate(x,y) %>% select(-c(Indicativo)) %>% 
  na.omit()

prec_mdt<-raster::extract(mdt500,precUTM[1:2])
prec_aspect<-raster::extract(aspect,precUTM[2:3])
precUTM<-cbind(precUTM,prec_mdt,prec_aspect) %>% 
  rename(z=prec_mdt,asp=prec_aspect) %>% tibble()

#bucle de preci máxima
meses<-names(tmeanUTM[3:14])

for(m in 3:14){
  mes<-meses[m-2]
  nmes<-m-2
  prec_sub<-precUTM %>% 
    dplyr::select(x,y,z,asp,m) #%>% na.omit()
  predictores<-precUTM %>% dplyr::select(x,y,z,asp) %>% 
    replace_na(list(asp=0))
  responsetemp<-precUTM %>% select(m)
  datatemp<-cbind(responsetemp,predictores) %>% rename(response=1)
  #precenero<-Tps(x=predictores,Y=precUTM$enero,miles=F)
  print(paste("modeling",mes))
  gamtemp<-mgcv::gam(response~s(z)+s(y)+s(x),data=datatemp)
  predicttemp<-cbind(terreno,predict(gamtemp,terreno)) %>% 
    rename(prediction=5) %>% select(1,2,5) %>%
    mutate(prediction=replace(prediction,prediction<0,0)) %>% 
    rasterFromXYZ(crs=projectionUTM)
  print(paste("Writing",mes))
  writeRaster(predicttemp,paste0("prec",nmes,".tiff"),overwrite=TRUE)
}
#biovariables####
?biovars

tmaxlayers<-list.files(pattern="tmax")
tmax<-stack()
for(tm in tmaxlayers){
  tmaxtemp<-raster(tm)
  tmax<-stack(tmax,tmaxtemp)
}

tminlayers<-list.files(pattern="tmin")
tmin<-stack()
for(tm in tminlayers){
  tmintemp<-raster(tm)
  tmin<-stack(tmin,tmintemp)
}

tmeanlayers<-list.files(pattern="tmean")
tmean<-stack()
for(tm in tmeanlayers){
  tmeantemp<-raster(tm)
  tmean<-stack(tmean,tmeantemp)
}

preclayers<-list.files(pattern="prec")
prec<-stack()
for(tm in preclayers){
  prectemp<-raster(tm)
  prec<-stack(prec,prectemp)
}

library(dismo)
biovariables<-biovars(prec,tmin,tmax)
# writeRaster(biovariables, 
#             filename=names(biovariables), 
#             bylayer=TRUE,format="GTiff")

for(b in names(biovariables)){
  tempbiovar<-subset(biovariables,b)
  writeRaster(tempbiovar,paste0(b,".tiff"))
}


plot(gamenero)
limGC<-extent(GCgeo)
plot(GCgeo)
GCdf<-data.frame(GC) %>% mutate(LONG=LONG/10000,LAT=LAT/10000) %>% 
  dplyr::select(-c(XUTM30,YUTM30,PROV_NOMBR,NOMHOJA,ALTI_ANE,PROV_ID,
            NUM_CUENCA,HOJA_ID,GR_CUENCA_,TIPO_CORRI,AMBITO,
            AMBITO_ID,CORRIENTE,CDR1,CDR2,optional,COMENTARIO,
            coords.x1,coords.x2)) %>% rename(Indicativo=IND_INM) %>% 
  filter(ANOINI>0)

tmean<-read_delim("TEMP.MEDIA MENSUAL.CSV",delim=";",skip=1)%>%
  rename(year=7) %>% 
  subset(NOM_PROV=="LAS PALMAS") %>%na.omit() %>%  mutate(x=LONGITUD/-100000,y=LATITUD/10000) %>% 
  mutate(LONGITUD=NULL,LATITUD=NULL)  %>% subset(x<15*-1 & y>27.3) %>% 
  group_by(Indicativo,NOMBRE)


tmean<- tmean %>% summarise(across(enero:diciembre,mean),n=n(),across(x:y,max))

prec<-read_delim("PRECIP.MENSUALES.CSV",delim=";") %>%
 rename(year=7) %>% subset(NOM_PROV=="LAS PALMAS")
  na.omit() %>% group_by(Indicativo) %>% 
  mutate(across(enero:diciembre,as.numeric)) %>%
  mutate(x=LONGITUD/-100000,y=LATITUD/10000) %>% 
  mutate(LONGITUD=NULL,LATITUD=NULL) %>% 
  subset(x<15*-1 & y>27.3) %>% 
  summarise(across(enero:diciembre,mean),n=n(),across(x:y,max)) %>% 
    mutate(total=sum(enero:diciembre)) %>% mutate(across(enero:diciembre,as.numeric))

GCprec<-full_join(GCdf,prec,by="Indicativo",suffix=c("_GC","_AEMET"))
coordenadasprec<-read_delim("coordsprec.csv",delim=";") %>% group_by(NOMBRE) %>% na.omit() %>% 
  summarise(across(3:4,mean)) %>% subset(x>-16 & x<15*-1)

coordenadasprec[1,1]<-"AGUIMES EL MILANO"
ggplot(coordenadasprec,aes(x,y))+geom_point()
GCdf<-GCdf %>% rename(Indicativo=IND_INM)
GCdf[c("FID_","INDCTV","TIPO")]<-NULL
GCdf[c("SGE","TIPO_CORR_","TOTANOS","TOTANOSC")]<-NULL
GCdf[c("MUNI_ID")]<-NULL
GCtmean<-full_join(GCdf,tmean,by="Indicativo",keep=TRUE,suffix=c("_GC","_AEMET"))
GCtmean<-GCtmean %>% relocate(Indicativo_AEMET,.after=Indicativo_GC) %>% 
relocate(NOMBRE,.after = LUGAR)
  View(GCtmean)
GCtmean %>% na.omit() %>% View()
GCtmean %>% select(Indicativo_AEMET) %>% na.omit() %>% dim()
GCtmean %>% select(Indicativo_GC) %>% na.omit() %>% dim()

SpatialPointsDataFrame(coords=tmean[c("x","y")],data=tmean) %>% plot()
points(GC)


#1.1 Crear mapa web####
mapaweb<-leaflet() %>% 
  addTiles(group="Open Street Maps") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Ortoimágenes") %>% 
  addWMSTiles(baseUrl="http://www.madrid.org/cartografia/ide/wms/WMS_ORTOIMAGENES.xml",
              layers="ORTO_50CM_20092",
              group = "Open Street Maps",
              options = WMSTileOptions(format = "image/png", transparent = F)) %>%
  setView(lng = -15.3, lat = 28.3, zoom = 12)%>% 
  addMarkers(data=GCdf,
             #group="Actuaciones LIFE CAÑADAS",
             #icon=iconoLIFE,
             lng=GCdf$x,lat=GCdf$y, 
             popup = GCdf$LUGAR) %>% 
  addCircleMarkers(data=coordenadasprec,
             #group="Actuaciones LIFE CAÑADAS",

              #icon=quakeIcons,
              lng=coordenadasprec$x,lat=coordenadasprec$y,
              #popup = tmean$NOMBRE             ) %>%

             # icon=greenLeafIcon,
             # lng=tmean$x,lat=tmean$y#,
             #popup = ~htmlEscape(coordenadasprec$NOMBRE)
             ) %>%

  addLayersControl(
    baseGroups = c("Open Street Maps","Ortoimágenes"),
    #overlayGroups = c("Vías Pecuarias","Unidad de Arcosas", "Actuaciones LIFE CAÑADAS"),
    options = layersControlOptions(collapsed = TRUE)
  )
saveWidget(mapaweb,file="mapaweb.html")
#1.Relativizar coordenadas####

predictores<-GCdf %>% filter(MEDANO>0)%>% select(x,y,ALTI) 
precipitacion<-GCdf%>% filter(MEDANO>0) %>%select(MEDANO)
z=predictores["ALTI"]
prec<-fields::Tps(x=predictores, Y=precipitacion, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
            miles = TRUE, method = "GCV", GCV = TRUE) #función thinplate
prec2<-fields::Tps(x=predictores[c("x","y")], Y=precipitacion, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
                  miles = TRUE, method = "GCV", GCV = TRUE) #función thinplate
surface(prec2)
par(mfrow(1,2))
surface(prec)
surface(prec2)
ggplot()
zi<-xi-min/max-min




#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)

#ÑAPA GC####
setwd("~/Spalmensis")
gridded(meuse.grid) <- ~x+y
meuse.raster <- raster(meuse.grid)
plot(meuse.raster)
gcfiles<-list.files(pattern="grancanaria")
gcstack<-stack()
for(i in gcfiles){
  print(i)
  raster(i) %>% aggregate(10) %>%
    mask(grancanaria) %>% 
    writeRaster(paste0("500",i),overwrite=TRUE)
    
  
}
raster(paste0("500",gcfiles[1])) %>% plot()
#1.1. Cotejar coordenadas ISA Gran Canaria
GCelev<-raster("grancanaria.asc")
crs(GCelev)<-projectionUTM
GCelev<-GCelev%>% 
  projectRaster(crs=projectiongeo)


archipielago<-
  GC<-readOGR("estacionesAEMET_GC.shp")
GCgeo<-spTransform(GC,projectiongeo)


limGC<-extent(GC)
plot(GC)
GCdf<-data.frame(GCgeo) #%>% mutate(LONG=LONG/10000,LAT=LAT/10000)
GCdf$x<-coordinates(GCgeo)[,1]
GCdf$y<-coordinates(GCgeo)[,2]
