library(tidyverse)
library(data.table)

# ---------- LOADING DATASET ---------#
#reading data using data.table(its faster)
ships<-fread("ships.csv")

#making more appropriate column names and changing them to lowercase
ships<-ships %>% rename(port_reported = PORT,port_assigned = port,
                        ship_type_id = SHIPTYPE,ship_type_name = ship_type,
                        ship_name = SHIPNAME) %>% 
  rename_with(tolower) %>%
  select(ship_id,ship_name,ship_type_id,ship_type_name,datetime,
         lat,lon,port_reported) %>%
  mutate(super_unique_id = paste(ship_id,ship_name,ship_type_id,ship_type_name,sep = ","))

ships$ship_name<-ifelse(ships$ship_name %in% c("SAT AIS","[SAT-AIS]"),
                           ships$super_unique_id,ships$ship_name)

# ---------- HAVERSINE FORMULA TO GET DISTANCE ---------#
my_dist <- function(long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6378137
  d <- R*c
  return(d)
}

# ------ EXTRACTING OBS WITH MAX DIST AND THE ONE BEFORE ------ #
final_set <- function(df,superid) {
  #first,filter the specific ship and calculate the dist
  df2 <- df %>% filter(super_unique_id == superid) %>%
    arrange(datetime) %>%
    mutate(haversine_dist = my_dist(lag(lon),lag(lat),lon,lat),
           obs = row_number(),
           tot = sum(haversine_dist,na.rm = TRUE)) 
  #second,filter the row with max haversine distance
  df3 <-df2 %>% filter(haversine_dist == max(haversine_dist,na.rm = TRUE))
  #third,if there is a case of multiple rows with the same max haversine value
  #choose the one with max date
  df3 <-df3 %>% filter(datetime == max(datetime,na.rm = TRUE))
  #get the obs value of the max haversine + max datetime value
  max_dist_obs<-df3$obs
  #finally,filter this max row and the row just before it
  map_data<-df2 %>% filter(obs %in% c(max_dist_obs-1,max_dist_obs))
  return(map_data)
}

