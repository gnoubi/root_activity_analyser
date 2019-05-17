library(RMariaDB)
library(DBI)
library(gWidgets)
library(RMySQL)
library(dbConnect)
library(sf)

dbConnection <- dbConnect(RMariaDB::MariaDB(), user='root', password='root2019', host='127.0.0.1', port=3306, dbname='db_arbre')



myQuery1 <- paste0("SELECT id_suj, ref_site, type_essence, (case when isnull(type_essence) then 'undefined' else substr(type_essence,1,3) end) as genre, concat(id_suj,'-',(case when isnull(type_essence) then 'undefined' else substr(type_essence,1,3) end)) as id_suj_ess  FROM SUJET where type_strate = 'arbre'")
queryresult1<- dbGetQuery(dbConnection, myQuery1)
bulk_suj <- as.vector(unlist(queryresult1["id_suj"]))
structured_reprise_sujet <- queryresult1
rownames(structured_reprise_sujet) <- structured_reprise_sujet$id_suj_ess

structured_sujet <- data.frame("id_suj" = bulk_suj)


tps_prec=format(Sys.time(),'%s')

#nrow(structured_sujet)
for (s  in 1:443)  {
  
  tps_prec=as.numeric(format(Sys.time(),'%s'))
  
  idsujet <- structured_sujet[s,"id_suj"]
  cat(idsujet,'...')
  
  #if (idsujet %in% c(508,510,513,519,520,522,525,527,528)) next
  
  cat('OK')
  
  #récuperation de toutes les dates de mesures
  myQuery <- paste0("SELECT distinct date_mes FROM db_arbre.MESURE where id_suj = '",idsujet, "' order by date_mes asc")
  #myQuery <- paste0("SELECT distinct  date_mes FROM db_arbre.MESURE where id_suj =",idsujet, " group by date_mes order by date_mes asc")
  queryresult<- dbGetQuery(dbConnection, myQuery)
  bulk_date <- as.vector(unlist(queryresult["date_mes"]))
  structured_data <- data.frame("date_mes" = bulk_date)
  
  etape="etape1:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  
  # on pivote les sondes et T en vecteurs (colonnes)
  sondes_liste=c('T','1','2','3')
  
  # Remplir les colonnes des sondes et de T
  for(sonde in sondes_liste)
  {
    myQuery <- paste0("SELECT date_mes, affiche FROM db_arbre.MESURE where id_suj = '",idsujet,"' and suj_sond='",sonde,"'")
    queryresult<- dbGetQuery(dbConnection, myQuery)
    names(queryresult)[names(queryresult) == "affiche"] <- sonde
    structured_data <- merge(structured_data, queryresult, by="date_mes")
    
  }
  
  etape="etape2:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  #dbDisconnect(dbConnection)
  nb_rows <- nrow(structured_data)
  
  # on calcule les derived pour chaque sonde
  derived <- data.frame(matrix(NA, nrow = (nb_rows - 1), ncol = 5))
  colnames(derived) <- c("date_mes",sondes_liste)
  
  for(i in 1:nrow(derived))
  {
    delay <- (structured_data[i + 1,"date_mes"] - structured_data[i,"date_mes"] ) / (3600 * 24)
    derived[i,"date_mes"] <- structured_data[i,"date_mes"]
    res <- c(structured_data[i,"date_mes"])
    for(sonde in sondes_liste)
    {
      if(delay!=0) derived[i,sonde] <-(structured_data[i + 1,sonde] - structured_data[i,sonde]) / delay
    }
    
  }
  
  #Pour supprimer les lignes avec des NA
  derived = derived[!is.na(rowSums(derived)),]
  
  
  etape="etape3:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  #on affiche les courbes des derived pour chaque sonde
  
  plot(derived[,'date_mes'] ,derived[,'1'], type='l', col='red', xaxt='n', xlab='')
  plot(derived[,'date_mes'] ,derived[,'2'], type='l', col='blue', xaxt='n', xlab='')
  plot(derived[,'date_mes'] ,derived[,'3'], type='l', col='black', xaxt='n', xlab='')
  
  
  
  #------------------------------------------------------------------------------------------------------------------------------------I
  
  #sonde S_1
  
  #on crée la polyligne contenant l'évolution temporelle de la dérivée des tensions
  lptS1 <- matrix(NA, nrow = nrow(derived), ncol = 2)
  lptS1[,1] <- derived[,'date_mes']
  lptS1[,2] <- derived[,'1']
  l1S1 = st_linestring(lptS1)
  
  
  #on crée la polyligne à 2 cb/jour
  lpts2S1 = matrix(1:4,  2)
  lpts2S1[1,1]<- 0
  lpts2S1[1,2]<- 2
  lpts2S1[2,1]<- derived[nrow(derived),'date_mes']
  lpts2S1[2,2]<- 2
  l2S1 = st_linestring(lpts2S1)
  
  
  #juste un petit affichage rapide
  lS1 = vector("list", 2)
  lS1[[1]] = l1S1
  lS1[[2]] = l2S1
  s1 = st_sfc(lS1)
  plot(s1, col = "red")
  
  #on réalise les intersection entre les deux polylignes
  sf1 = st_sf(s1)
  j1 = st_intersection(sf1)
  
  #on récupère la liste des points d'intersection dans la multi-géometrie
  list_point1 <- st_collection_extract(j1[2,"geometry"],"POINT")
  
  #on récupère les coordonnées des points sous la forme d'une matrice
  inter1 <- st_coordinates(list_point1)
  
  # determination des intervalles d'activités 
  event_date1 <- c(min(structured_data[,"date_mes"]),inter1[,"X"],max(structured_data[,"date_mes"]) )
  activity_sequence1 <- data.frame(matrix(NA, nrow = length(event_date1) - 1, ncol = 9))
  colnames(activity_sequence1) <- c("start","end","activity","Tmax","Tmin","Tmean", "S_1max", "S_1min","S_1mean")
  
  
  etape="etape4:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  for(i in 1:(length(event_date1) - 1))
  {
    tmp1 <- derived[derived$date_mes >= event_date1[i]  & derived$date_mes <= event_date1[i + 1]  ,]
    activity_sequence1[i,"start"] <- event_date1[i]
    activity_sequence1[i,"end"] <- event_date1[i+1]
    if(tmp1[1,3] > 2) {
      activity_sequence1[i,"activity"] <- 1
    }
    else
    {
      activity_sequence1[i,"activity"] <- 0
    }
  }
  
  etape="etape5:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  for(i in 1:(nrow(activity_sequence1))){
    tmpp1 <- structured_data[structured_data$date_mes >= activity_sequence1[i,"start"]  & structured_data$date_mes <= activity_sequence1[i,"end"]  ,]
    tmx1 <- max(tmpp1[,"T"])
    tmin1 <- min(tmpp1[,"T"])
    tmean1 <- mean(tmpp1[,"T"],trim = 0.5)
    S1mx <- max(tmpp1[,"1"])
    S1min <- min(tmpp1[,"1"])
    S1mean <- mean(tmpp1[,"1"],trim = 0.5)
    activity_sequence1[i,'Tmax'] <- tmx1
    activity_sequence1[i,'Tmin'] <- tmin1
    activity_sequence1[i,'Tmean'] <- tmean1
    activity_sequence1[i,'S_1max'] <- S1mx
    activity_sequence1[i,'S_1min'] <- S1min
    activity_sequence1[i,'S_1mean'] <- S1mean
    
  }
  
  etape="etape6:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  #on affiche un plot derived sonde 1 avec polyline
  x_range1 <- c(min(derived[,'date_mes']),max(derived[,'date_mes']))
  y_range1 <- c(-10,max(derived[,'1']))
  plot(derived[,'date_mes'], derived[,'1'], type = "n", xlim = range(x_range1), ylim = range(y_range1), xlab = "", ylab = "")
  lines(derived[,'date_mes'], derived[,'1'], col = "blue")
  points(activity_sequence1[,'start'], activity_sequence1[,"activity"]*40, col = "green")
  abline(h=2, col='red')
  points(inter1[,"X"], inter1[,"Y"], col = "red")
  
  
  
  activite_S1 <- data.frame(matrix(NA, nrow = nrow(structured_data), ncol = 5))
  colnames(activite_S1) <- c("date_mes","activite_S1","Tmax","Tmin","Tmean")
  
  
  for(i in 1:(nrow(activite_S1))){
    activite_S1[i,"date_mes"] <- structured_data[i,"date_mes"]
    tmps1 <- activity_sequence1[structured_data[i,"date_mes"] >= activity_sequence1$start,]
    tmp2s1 <- tmps1[structured_data[i,"date_mes"] <= tmps1$end,]
    if(nrow(tmp2s1) == 1)
    {
      activite_S1[i,"activite_S1"] <-  tmp2s1[,"activity"]
      activite_S1[i,"Tmax"] <-  tmp2s1[,"Tmax"]
      activite_S1[i,"Tmin"] <-  tmp2s1[,"Tmin"]
      activite_S1[i,"Tmean"] <-  tmp2s1[,"Tmean"]    
    }
  }
  
  etape="etape7:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  #------------------------------------------------------------------------------------------------------------------------------------I
  
  
  #Sonde S_2
  
  #on crée la polyligne contenant l'évolution temporelle de la dérivée des tensions
  
  lptS2 <- matrix(NA, nrow = nrow(derived), ncol = 2)
  lptS2[,1] <- derived[,'date_mes']
  lptS2[,2] <- derived[,'2']
  l1S2 = st_linestring(lptS2)
  
  
  #on crée la polyligne à 2 cb/jour
  if (max (derived$`2`) < 2) next
  lpts2S2 = matrix(1:4,  2)
  lpts2S2[1,1]<- 0
  lpts2S2[1,2]<- 2
  lpts2S2[2,1]<- derived[nrow(derived),'date_mes']
  lpts2S2[2,2]<- 2
  l2S2 = st_linestring(lpts2S2)
  
  
  #juste un petit affichage rapide
  
  lS2 = vector("list", 2)
  lS2[[1]] = l1S2
  lS2[[2]] = l2S2
  s2 = st_sfc(lS2)
  plot(s2, col = "red")
  
  #on réalise les intersection entre les deux polylignes
  
  sf2 = st_sf(s2)
  j2 = st_intersection(sf2)
  
  #on récupère la liste des points d'intersection dans la multi-géometrie
  
  list_point2 <- st_collection_extract(j2[2,"geometry"],"POINT")
  
  #on récupère les coordonnées des points sous la forme d'une matrice
  
  inter2 <- st_coordinates(list_point2)
  
  # determination des intervalles d'activités 
  event_date2 <- c(min(structured_data[,"date_mes"]),inter2[,"X"],max(structured_data[,"date_mes"]) )
  activity_sequence2 <- data.frame(matrix(NA, nrow = length(event_date2) - 1, ncol = 9))
  colnames(activity_sequence2) <- c("start","end","activity","Tmax","Tmin","Tmean", "S_2max", "S_2min","S_2mean")
  
  for(j in 1:(length(event_date2) - 1))
  {
    tmp2 <- derived[derived$date_mes >= event_date2[j]  & derived$date_mes <= event_date2[j + 1]  ,]
    activity_sequence2[j,"start"] <- event_date2[j]
    activity_sequence2[j,"end"] <- event_date2[j+1]
    if(tmp2[1,4] > 2) {
      activity_sequence2[j,"activity"] <- 1
    }
    else
    {
      activity_sequence2[j,"activity"] <- 0
    }
  }
  
  
  for(j in 1:(nrow(activity_sequence2))){
    tmpp2 <- structured_data[structured_data$date_mes >= activity_sequence2[j,"start"]  & structured_data$date_mes <= activity_sequence2[j,"end"]  ,]
    tmx2 <- max(tmpp2[,"T"])
    tmin2 <- min(tmpp2[,"T"])
    tmean2 <- mean(tmpp2[,"T"],trim = 0.5)
    S2mx <- max(tmpp2[,"2"])
    S2min <- min(tmpp2[,"2"])
    S2mean <- mean(tmpp2[,"2"],trim = 0.5)
    activity_sequence2[j,'Tmax'] <- tmx2
    activity_sequence2[j,'Tmin'] <- tmin2
    activity_sequence2[j,'Tmean'] <- tmean2
    activity_sequence2[j,'S_2max'] <- S2mx
    activity_sequence2[j,'S_2min'] <- S2min
    activity_sequence2[j,'S_2mean'] <- S2mean
    
  }
  
  
  #on affiche un plot derived sonde 1 avec polyline
  x_range2 <- c(min(derived[,'date_mes']),max(derived[,'date_mes']))
  y_range2 <- c(-10,max(derived[,'2']))
  plot(derived[,'date_mes'], derived[,'2'], type = "n", xlim = range(x_range2), ylim = range(y_range2), xlab = "", ylab = "")
  lines(derived[,'date_mes'], derived[,'2'], col = "blue")
  points(activity_sequence2[,'start'], activity_sequence2[,"activity"]*10, col = "green")
  abline(h=2, col='red')
  points(inter2[,"X"], inter2[,"Y"], col = "red")
  
  
  
  activite_S2 <- data.frame(matrix(NA, nrow = nrow(structured_data), ncol = 5))
  colnames(activite_S2) <- c("date_mes","activite_S2","Tmax","Tmin","Tmean")
  
  
  for(i in 1:(nrow(activite_S2))){
    activite_S2[i,"date_mes"] <- structured_data[i,"date_mes"]
    tmps2 <- activity_sequence2[structured_data[i,"date_mes"] >= activity_sequence2$start,]
    tmp2s2 <- tmps2[structured_data[i,"date_mes"] <= tmps2$end,]
    if(nrow(tmp2s2) == 1)
    {
      activite_S2[i,"activite_S2"] <-  tmp2s2[,"activity"]
      activite_S2[i,"Tmax"] <-  tmp2s2[,"Tmax"]
      activite_S2[i,"Tmin"] <-  tmp2s2[,"Tmin"]
      activite_S2[i,"Tmean"] <-  tmp2s2[,"Tmean"]    
    }
  }
  
  
  etape="etape8:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  #------------------------------------------------------------------------------------------------------------------------------------I
  
  
  #Sonde S_3
  
  #on crée la polyligne contenant l'évolution temporelle de la dérivée des tensions
  
  lptS3 <- matrix(NA, nrow = nrow(derived), ncol = 2)
  lptS3[,1] <- derived[,'date_mes']
  lptS3[,2] <- derived[,'3']
  l1S3 = st_linestring(lptS3)
  
  
  #on crée la polyligne à 2 cb/jour
  
  
  if (max (derived$`3`) < 2) next
  lpts2S3 = matrix(1:4,  2)
  lpts2S3[1,1]<- 0
  lpts2S3[1,2]<- 2
  lpts2S3[2,1]<- derived[nrow(derived),'date_mes']
  lpts2S3[2,2]<- 2
  l2S3 = st_linestring(lpts2S3)
  
  
  #juste un petit affichage rapide
  
  lS3 = vector("list", 2)
  lS3[[1]] = l1S3
  lS3[[2]] = l2S3
  s3 = st_sfc(lS3)
  plot(s3, col = "red")
  
  #on réalise les intersection entre les deux polylignes
  
  sf3 = st_sf(s3)
  j3 = st_intersection(sf3)
  
  #if (st_bbox(j3)$ymax<2) next
  
  #on récupère la liste des points d'intersection dans la multi-géometrie
  
  list_point3 <- st_collection_extract(j3[2,"geometry"],"POINT")
  
  #on récupère les coordonnées des points sous la forme d'une matrice
  
  inter3 <- st_coordinates(list_point3)
  
  # determination des intervalles d'activités 
  
  event_date3 <- c(min(structured_data[,"date_mes"]),inter3[,"X"],max(structured_data[,"date_mes"]) )
  activity_sequence3 <- data.frame(matrix(NA, nrow = length(event_date3) - 1, ncol = 9))
  colnames(activity_sequence3) <- c("start","end","activity","Tmax","Tmin","Tmean", "S_3max", "S_3min","S_3mean")
  
  for(k in 1:(length(event_date3) - 1))
  {
    tmp3 <- derived[derived$date_mes >= event_date3[k]  & derived$date_mes <= event_date3[k + 1]  ,]
    activity_sequence3[k,"start"] <- event_date3[k]
    activity_sequence3[k,"end"] <- event_date3[k+1]
    if(tmp3[1,5] > 2) {
      activity_sequence3[k,"activity"] <- 1
    }
    else
    {
      activity_sequence3[k,"activity"] <- 0
    }
  }
  
  # Détermination des mesures (max min et mean) pour chaque intervalle d'activité 
  
  for(k in 1:(nrow(activity_sequence3))){
    tmpp3 <- structured_data[structured_data$date_mes >= activity_sequence3[k,"start"]  & structured_data$date_mes <= activity_sequence3[k,"end"]  ,]
    tmx3 <- max(tmpp3[,"T"])
    tmin3 <- min(tmpp3[,"T"])
    tmean3 <- mean(tmpp3[,"T"],trim = 0.5)
    S3mx <- max(tmpp3[,"3"])
    S3min <- min(tmpp3[,"3"])
    S3mean <- mean(tmpp3[,"3"],trim = 0.5)
    activity_sequence3[k,'Tmax'] <- tmx3
    activity_sequence3[k,'Tmin'] <- tmin3
    activity_sequence3[k,'Tmean'] <- tmean3
    activity_sequence3[k,'S_3max'] <- S3mx
    activity_sequence3[k,'S_3min'] <- S3min
    activity_sequence3[k,'S_3mean'] <- S3mean
    
  }
  
  
  #on affiche un plot derived sonde 1 avec polyline
  x_range3 <- c(min(derived[,'date_mes']),max(derived[,'date_mes']))
  y_range3 <- c(-10,max(derived[,'3']))
  plot(derived[,'date_mes'], derived[,'3'], type = "n", xlim = range(x_range2), ylim = range(y_range2), xlab = "", ylab = "")
  lines(derived[,'date_mes'], derived[,'3'], col = "blue")
  points(activity_sequence3[,'start'], activity_sequence3[,"activity"]*10, col = "green")
  abline(h=2, col='red')
  points(inter3[,"X"], inter3[,"Y"], col = "red")
  
  
  activite_S3 <- data.frame(matrix(NA, nrow = nrow(structured_data), ncol = 5))
  colnames(activite_S3) <- c("date_mes","activite_S3","Tmax","Tmin","Tmean")
  
  
  for(i in 1:(nrow(activite_S3))){
    activite_S3[i,"date_mes"] <- structured_data[i,"date_mes"]
    tmps3 <- activity_sequence3[structured_data[i,"date_mes"] >= activity_sequence3$start,]
    tmp2s3 <- tmps3[structured_data[i,"date_mes"] <= tmps3$end,]
    if(nrow(tmp2s3) == 1)
    {
      activite_S3[i,"activite_S3"] <-  tmp2s3[,"activity"]
      activite_S3[i,"Tmax"] <-  tmp2s3[,"Tmax"]
      activite_S3[i,"Tmin"] <-  tmp2s3[,"Tmin"]
      activite_S3[i,"Tmean"] <-  tmp2s3[,"Tmean"]    
    }
  }
  
  etape="etape9:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  
  time2015 <- 1420066800
  time2016 <- 1451602800
  time2017 <- 1483225200
  time2018 <- 1514761200
  time2019 <- 1546297200
  
  
  structured_dataDateS1 <- within(activity_sequence1, {date_structStar = format(as.POSIXct(activity_sequence1$start, origin="1970-01-01"),"%W")
  })
  
  structured_dataDateS11 <- subset(structured_dataDateS1, structured_dataDateS1$activity>0 )
  
  structured_dataDateS111 <- subset(structured_dataDateS11, structured_dataDateS11$start> time2017)
  structured_dataDateS1111 <- subset(structured_dataDateS11, structured_dataDateS11$start> time2018)
  structured_dataDateS11111 <- subset(structured_dataDateS11, structured_dataDateS11$start> time2019)
  
  
  structured_dataDateS2 <- within(activity_sequence2, {date_structStar = format(as.POSIXct(activity_sequence2$start, origin="1970-01-01"),"%W")
  })
  
  structured_dataDateS12 <- subset(structured_dataDateS2, structured_dataDateS2$activity>0 )
  
  structured_dataDateS121 <- subset(structured_dataDateS12, structured_dataDateS12$start> time2017)
  structured_dataDateS1211 <- subset(structured_dataDateS12, structured_dataDateS12$start> time2018)
  structured_dataDateS12111 <- subset(structured_dataDateS12, structured_dataDateS12$start> time2019)
  
  structured_dataDateS3 <- within(activity_sequence3, {date_structStar = format(as.POSIXct(activity_sequence3$start, origin="1970-01-01"),"%W")
  })
  
  structured_dataDateS13 <- subset(structured_dataDateS3, structured_dataDateS3$activity>0 )
  
  structured_dataDateS131 <- subset(structured_dataDateS13, structured_dataDateS13$start> time2017)
  structured_dataDateS1311 <- subset(structured_dataDateS13, structured_dataDateS13$start> time2018)
  structured_dataDateS13111 <- subset(structured_dataDateS13, structured_dataDateS13$start> time2019)
  
  
  derived_dataDate <- within(derived, {
    date_derived <- as.Date(as.POSIXct ( derived$date_mes, origin = "1970-01-01"))
  })
  
  etape="etape fin:"
  tps_actu = as.numeric(format(Sys.time(),'%s'))
  cat(etape, tps_actu - tps_prec, "sec \n")
  tps_prec=tps_actu
  
  
  #ajout des dates de premieres activités de chaque année
  tmp11 <- structured_data[structured_data$date_mes >= structured_dataDateS11$start[1]  & structured_data$date_mes <= structured_dataDateS11$end[1]  ,]
  actS1 <- structured_dataDateS11[1,'date_structStar']
  actS11 <- structured_dataDateS111[1,'date_structStar']
  actS111 <- structured_dataDateS1111[1,'date_structStar']
  actS1111 <- structured_dataDateS11111[1,'date_structStar']
  
  tmp22 <- structured_data[structured_data$date_mes >= structured_dataDateS12$start[1]  & structured_data$date_mes <= structured_dataDateS12$end[1]  ,]
  actS2 <- structured_dataDateS12[1,'date_structStar']
  actS22 <- structured_dataDateS121[1,'date_structStar']
  actS222 <- structured_dataDateS1211[1,'date_structStar']
  actS2222 <- structured_dataDateS12111[1,'date_structStar']
  
  tmp33 <- structured_data[structured_data$date_mes >= structured_dataDateS13$start[1]  & structured_data$date_mes <= structured_dataDateS13$end[1]  ,]
  actS3 <- structured_dataDateS13[1,'date_structStar']
  actS33 <- structured_dataDateS131[1,'date_structStar']
  actS333 <- structured_dataDateS1311[1,'date_structStar']
  actS3333 <- structured_dataDateS13111[1,'date_structStar']
  
  
  
  structured_reprise_sujet[s,'date_mes_1ere_actS1'] <- tmp11$date_mes[1]
  structured_reprise_sujet[s,'date_activité_S1A1'] <- actS1
  structured_reprise_sujet[s,'date_activité_S1A2'] <- actS11
  structured_reprise_sujet[s,'date_activité_S1A3'] <- actS111
  structured_reprise_sujet[s,'date_activité_S1A4'] <- actS1111
  
  if (is.na(actS2)  || is.na(actS3)) next
  structured_reprise_sujet[s,'date_mes_1ere_actS2'] <- tmp22$date_mes[1]
  structured_reprise_sujet[s,'date_activité_S2A1'] <- actS2
  structured_reprise_sujet[s,'date_activité_S2A2'] <- actS22
  structured_reprise_sujet[s,'date_activité_S2A3'] <- actS222
  structured_reprise_sujet[s,'date_activité_S2A4'] <- actS2222
  
  structured_reprise_sujet[s,'date_mes_1ere_actS3'] <- tmp33$date_mes[1]
  structured_reprise_sujet[s,'date_activité_S3A1'] <- actS3
  structured_reprise_sujet[s,'date_activité_S3A2'] <- actS33
  structured_reprise_sujet[s,'date_activité_S3A3'] <- actS333
  structured_reprise_sujet[s,'date_activité_S3A4'] <- actS3333
  
}
dbDisconnect(dbConnection)

structured_reprise_sujet[,"date_mes_1ere_actS1"] <- format(as.POSIXct(structured_reprise_sujet[,"date_mes_1ere_actS1"], origin="1970-01-01"),"%W") 
structured_reprise_sujet[,"date_mes_1ere_actS2"] <- format(as.POSIXct(structured_reprise_sujet[,"date_mes_1ere_actS2"], origin="1970-01-01"),"%W") 
structured_reprise_sujet[,"date_mes_1ere_actS3"] <- format(as.POSIXct(structured_reprise_sujet[,"date_mes_1ere_actS3"], origin="1970-01-01"),"%W") 

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(missMDA)

Data_test_Before_clust <- subset(structured_reprise_sujet[1:200,], select = c(genre,date_activité_S1A1,date_mes_1ere_actS1,date_activité_S2A1,date_mes_1ere_actS2,date_activité_S3A1,date_mes_1ere_actS3 ))

Data_test_clust <- subset(structured_reprise_sujet[1:50,], select = c(genre,date_activité_S1A1,date_activité_S2A1,date_activité_S3A1 ))

#Data_test_clust[,"genre"] <- as.factor(Data_test_clust[,"genre"]) 
#Data_test_clust[,"genre"] <- as.numeric(Data_test_clust[,"genre"])
Data_test_clust[,"date_activité_S1A1"] <- as.numeric(Data_test_clust[,"date_activité_S1A1"])
Data_test_clust[,"date_activité_S2A1"] <- as.numeric(Data_test_clust[,"date_activité_S2A1"])
Data_test_clust[,"date_activité_S3A1"] <- as.numeric(Data_test_clust[,"date_activité_S3A1"])

#Temporaire !!!! a modifier
#Suppression des lignes avec NA
Data_test_clust=Data_test_clust[!is.na(Data_test_clust[,'date_activité_S3A1']),]

summary(Data_test_clust)

#methode CAH
clustCHA <- MCA(Data_test_clust, quali.sup = 1, ncp = 5)
#plot.PCA(clustCHA, choix="ind", habillage= 1 )
clust.hcps <- HCPC(clustCHA)

#affichage des propriétés du cluster
names(clust.hcps) #les objets interessant du cluster
clust.hcps$data.clust # la table à cluster
clust.hcps$desc.var  # les variables qui decrivent les objets
clust.hcps$desc.axes # descriptions par les axes 
clust.hcps$desc.ind  # les objets par rapport au centre de classe
clust.hcps$dist


#methode de ward
clust.dist <- dist(Data_test_clust, method = "euclidean")
clust <- hclust(clust.dist, method = "ward.D")
plot(clust)









#write.csv(structured_reprise_sujet,file = "date_reprise_des_sondes.csv")