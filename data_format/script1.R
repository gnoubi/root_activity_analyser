library(RMariaDB)
library(DBI)
library(gWidgets)
library(RMySQL)
library(dbConnect)
library(sf)
library(dplyr)

dbConnection <- dbConnect(RMariaDB::MariaDB(), user='root', password='root', host='127.0.0.1', port=8889, dbname='kosso')
idsujet= 1006

#récuperation de toutes les dates de mesures
myQuery <- paste0("SELECT distinct  date_mes FROM kosso.MESURE where id_suj =",idsujet, " group by date_mes order by date_mes asc")
queryresult<- dbGetQuery(dbConnection, myQuery)
bulk_date <- as.vector(unlist(queryresult["date_mes"]))
structured_data <- data.frame("date_mes" = bulk_date)

sondes_liste=c('T','1','2','3')

for(sonde in sondes_liste)
{
  myQuery <- paste0("SELECT date_mes, affiche FROM kosso.MESURE where id_suj =",idsujet," and suj_sond='",sonde,"'")
  queryresult<- dbGetQuery(dbConnection, myQuery)
  names(queryresult)[names(queryresult) == "affiche"] <- sonde
  structured_data <- merge(structured_data, queryresult, by="date_mes")
  
}
dbDisconnect(dbConnection)
nb_rows <- nrow(structured_data)

derived <- data.frame(matrix(NA, nrow = (nb_rows - 1), ncol =(length(sondes_liste)+ 1)))
colnames(derived) <- c("date_mes",sondes_liste)

for(i in 1:nrow(derived))
{
  delay <- (structured_data[i + 1,"date_mes"] - structured_data[i,"date_mes"] ) / (3600 * 24)
  derived[i,"date_mes"] <- structured_data[i,"date_mes"]
  res <- c(structured_data[i,"date_mes"])
  for(sonde in sondes_liste)
  {
    derived[i,sonde] <-(structured_data[i + 1,sonde] - structured_data[i,sonde]) / delay
  }
}

#on affiche les courbes
plot(derived[,'date_mes'] ,derived[,'1'], type='l', col='red', xaxt='n', xlab='')
plot(derived[,'date_mes'] ,derived[,'2'], type='l', col='blue', xaxt='n', xlab='')
plot(derived[,'date_mes'] ,derived[,'3'], type='l', col='black', xaxt='n', xlab='')


#la suite est uniquement pour la sonde 1 /// tu dois le faire évoluer pour les 3 sondes... mais pas tres compliqué

#on crée la polyligne contenant l'évolution temporelle de la dérivée des tensions
lpt1 <- matrix(NA, nrow = nrow(derived), ncol = 2)
lpt1[,1] <- derived[,'date_mes']
lpt1[,2] <- derived[,'1']
l11 = st_linestring(lpt1)


#on crée la polyligne à 2 cb/jour
lpts22[1,1]<- 0
lpts22[1,2]<- 2
lpts22[2,1]<- derived[nrow(derived),'date_mes']
lpts22[2,2]<- 2
l22 = st_linestring(lpts22)


#juste un petit affichage rapide
l1 = vector("list", 2)
l1[[1]] = l11
l1[[2]] = l22
s1 = st_sfc(l1)
plot(s1, col = "red")

#on réalise les intersection entre les deux polylignes
sf1 = st_sf(s1)
j = st_intersection(sf1)

#on récupère la liste des points d'intersection dans la multi-géometrie
list_point <- st_collection_extract(j[2,"geometry"],"POINT")

#on récupère les coordonnées des points sous la forme d'une matrice
inter <- st_coordinates(list_point)
event_date <- inter[,"X"]



#on affiche un plot
x_range <- c(min(derived[,'date_mes']),max(derived[,'date_mes']))
y_range <- c(-10,max(derived[,'1']))
plot(derived[,'date_mes'], derived[,'1'], type = "n", xlim = range(x_range), ylim = range(y_range), xlab = "", ylab = "")
lines(derived[,'date_mes'], derived[,'1'], col = "blue")
points(inter[,"X"], inter[,"Y"], col = "red")

activity_sequence <- data.frame(matrix(NA, nrow = length(event_date) - 1, ncol = 6))
colnames(activity_sequence) <- c("start","end","activity","tmax","tmin","tmean")

for(i in 1:(length(event_date) - 1)){
  tmp <- derived[derived$date_mes >= event_date[i]  & derived$date_mes <= event_date[i + 1]  ,]
  activity_sequence[i,"start"] <- event_date[i]
  activity_sequence[i,"end"] <- event_date[i+1]
  if(tmp[1,3] > 2) {
    activity_sequence[i,"activity"] <- 1
  }
  else
  {
    activity_sequence[i,"activity"] <- 0
  }
}


for(i in 1:(nrow(activity_sequence))){
  tmp <- structured_data[structured_data$date_mes >= activity_sequence[i,"start"]  & structured_data$date_mes <= activity_sequence[i,"end"]  ,]
  tmx <- max(tmp[,"T"])
  tmin <- min(tmp[,"T"])
  tmean <- mean(tmp[,"T"])
  activity_sequence[i,'tmax'] <- tmx
  activity_sequence[i,'tmean'] <- tmean
  activity_sequence[i,'tmin'] <- tmin
}

#on affiche un plot
x_range <- c(min(derived[,'date_mes']),max(derived[,'date_mes']))
y_range <- c(-10,max(derived[,'1']))
plot(derived[,'date_mes'], derived[,'1'], type = "n", xlim = range(x_range), ylim = range(y_range), xlab = "", ylab = "")
lines(derived[,'date_mes'], derived[,'1'], col = "blue")
points(activity_sequence[,'start'], activity_sequence[,"activity"]*40, col = "green")
points(inter[,"X"], inter[,"Y"], col = "red")

