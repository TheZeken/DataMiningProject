library(RgoogleMaps) 
library(sp)          # Classes and Methods for Spatial Data
library(ggplot2)     # Implementation of the Grammar of Graphics
library(ggmap)       # Spatial Visualization with ggplot2
library(maps)        # Draw Geographical Maps

## Differents data sets opening
caracteristiques_2015 <- read.csv("C:/Users/jerem/Desktop/M1/Data Mining/Project/caracteristiques_2015.csv", na.strings= '')
lieux_2015 <- read.csv("C:/Users/jerem/Desktop/M1/Data Mining/Project/lieux_2015.csv", na.strings= '')
vehicules_2015 <- read.csv("C:/Users/jerem/Desktop/M1/Data Mining/Project/vehicules_2015.csv", na.strings= '')
usagers_2015 <- read.csv("C:/Users/jerem/Desktop/M1/Data Mining/Project/usagers_2015.csv", na.strings= '')

## Put the Latitude and Longitude in the correct norms

fct <- function(x){
  y <- x/100000
  return(y)
}

caracteristiques_2015$lat <- sapply(caracteristiques_2015$lat,fct)
caracteristiques_2015$long <- sapply(caracteristiques_2015$long,fct)

## Change the number of months by their names
ChangeMonth <- function(x){
  if(x == 1)
    return("January")
  else if (x == 2)
    return("February")
  else if (x == 3)
    return("March")
  else if (x == 4)
    return("April")
  else if (x == 5)
    return("May")
  else if (x == 6)
    return("June")
  else if (x == 7)
    return("July")
  else if (x == 8)
    return("August")
  else if (x == 9)
    return("September")
  else if (x == 10)
    return("October")
  else if (x == 11)
    return("November")
  else if (x == 12)
    return("December")
}

caracteristiques_2015$mois <- sapply(caracteristiques_2015$mois,ChangeMonth)

##Change the number corresponding to weather by their names
ChangeWeather <- function(x){
  if(x == 1)
    return("Normal")
  else if (x == 2)
    return("Light Rain ")
  else if (x == 3)
    return("Heavy Rain")
  else if (x == 4)
    return("Snow - Hail")
  else if (x == 5)
    return("Fog - Smoke")
  else if (x == 6)
    return("Heavy wind")
  else if (x == 7)
    return("Brilliant Weather")
  else if (x == 8)
    return("Cloudy Day")
  else if (x == 9)
    return("Other")
}

caracteristiques_2015$atm <- sapply(caracteristiques_2015$atm,ChangeWeather)

## Change the number corresponding to light type by their names
ChangeLight <- function(x){
  if(x == 1)
    return("Daylight")
  else if (x == 2)
    return("Dusk or dawn")
  else if (x == 3)
    return("night without street lighting")
  else if (x == 4)
    return("night with street lighting but not working")
  else if (x == 5)
    return("night with street lighting ")
}

caracteristiques_2015$lum <- sapply(caracteristiques_2015$lum,ChangeLight)

##Change the number corresponding to gender by their names
ChangeSexe <- function(x){
  if(x == 1)
    return("Male")
  else if (x == 2)
    return("Female")
}

##Change the number corresponding to the categories of the people by their names
ChangeCatu <- function(x){
  if(x == 1)
    return("Conductor")
  else if (x == 2)
    return("Passenger")
  else if (x == 3)
    return("Pedestrian")
  else if (x == 4)
    return("Pedestrian with roller or scooter")
}

usagers_2015$sexe <- sapply(usagers_2015$sexe,ChangeSexe)
usagers_2015$catu <- sapply(usagers_2015$catu,ChangeCatu)

##Change the number corresponding to the place in the car of the people by their names
ChangePlace <- function(x){
  if(x == 1)
    return("Conductor")
  else if (x == 2)
    return("Front Right")
  else if (x == 3)
    return("Back Right")
  else if (x == 4)
    return("Back Left")
  else if (x == 5)
    return("Back Middle")
  else if (x == 6)
    return("Front Middle")
  else if (x == 7)
    return("Middle left")
  else if (x == 8)
    return("Middle Middle")
  else if (x == 9)
    return("Middle Right")
}

usagers_2015$place <- sapply(usagers_2015$place,ChangePlace)


##Change the number corresponding to Localisation by their names
ChangeAgg <- function(x){
  if(x == 1)
    return("Outside built-up areas")
  else if (x == 2)
    return("Inside built-up areas")
}

caracteristiques_2015$agg <- sapply(caracteristiques_2015$agg,ChangeAgg)

##Change the number corresponding to Localisation by their names
ChangeCol <- function(x){
  if(x == 1)
    return("2 Véhicles - Front")
  else if (x == 2)
    return("2 Véhicles - From the rear")
  else if (x == 3)
    return("2 Véhicles - From the side ")
  else if (x == 4)
    return("3 Véhicles and In chains")
  else if (x == 5)
    return("3 Véhicles and multiples collision")
  else if (x == 6)
    return("Whith collision")
  else if (x == 7)
    return("Without collision")
}

caracteristiques_2015$col <- sapply(caracteristiques_2015$col,ChangeCol)

##Change the number corresponding to categories of roads by their names
ChangeCatr <- function(x){
  if(x == 1)
    return("Highway")
  else if (x == 2)
    return("Nationals Roads")
  else if (x == 3)
    return("Departemental roads")
  else if (x == 4)
    return("Communal roads")
  else if (x == 5)
    return("Outside the Public roads")
  else if (x == 6)
    return("Parking")
  else if (x == 7)
    return("Other")
}

lieux_2015$catr <- sapply(lieux_2015$catr,ChangeCatr)

##We change the number corresponding to the seriousness of the accident by their names
ChangeGrav <- function(x){
  if(x == 1)
    return("Unharmed")
  else if (x == 2)
    return("Killed")
  else if (x == 3)
    return("Wounded and Hospitalized")
  else if (x == 4)
    return("Light wounded")
}

usagers_2015$grav <- sapply(usagers_2015$grav,ChangeGrav)

## Now that our data are clean, we can start working on them
dataset_cara_lieux <- merge(caracteristiques_2015,lieux_2015,by="Num_Acc")
dataset_without_vehicles <- merge(dataset_cara_lieux,usagers_2015,by="Num_Acc")
dataset_global <- merge(dataset_without_vehicles,vehicules_2015,by="Num_Acc")
cara_usagers <-  merge(caracteristiques_2015,usagers_2015,by="Num_Acc")


## We want to create a map of France with all the accidents
## Create a dataset without observations with latitude or longitude equals to 0
dataset_cara_lieux_coord <- dataset_cara_lieux[dataset_cara_lieux$lat != '0',]
dataset_cara_lieux_coord <- dataset_cara_lieux [dataset_cara_lieux$long != '0',]

France_map <- get_map(location="France", 
                      maptype="satellite", 
                      zoom = 6)
ggmap(France_map, 
      extent = "device") + 
  geom_point(aes(x = dataset_cara_lieux_coord$long, 
                 y = dataset_cara_lieux_coord$lat), 
             colour = "red", 
             alpha = 0.1, size = 1, data = dataset_cara_lieux_coord)

## We want to know in which month there are the biggest number of accident during 2015

qplot(dataset_cara_lieux$mois, xlab= "Month", main= "Accidents by month") + 
  scale_y_continuous("Number of Accidents")

## We want to know during which weather we are more likely to have an accident
qplot(dataset_cara_lieux$atm, xlab= "Weather", main= "Accidents by Weather") + 
  scale_y_continuous("Number of Accidents")

## We want to know with which type of light we are more likely to have an accident
qplot(dataset_cara_lieux$lum, xlab= "Type of Light", main= "Accidents by Type of Light") + 
  scale_y_continuous("Number of Accidents")

## We create a subset with only conductor, in order to dscover new behaviours
dataset_conductors <- subset(cara_usagers, catu == "Conductor")

qplot(dataset_conductors$sexe, xlab= "Gender", main= "Accidents by Gender") + 
  scale_y_continuous("Number of Accidents")

## We want to know the seriousness of accident
qplot(cara_usagers$grav, xlab= "Type of Wounds", main= "Accidents by Wounds") + 
  scale_y_continuous("Number of Accidents")

## We want to know the seriousness of accident
dataset_dead <- subset(cara_usagers, grav == "Killed")

qplot(dataset_dead$place, xlab= "Place", main= "Most dangerous place") + 
  scale_y_continuous("Number of Accidents")

##We want to know which city btw Paris, Lyon and Marseille is the most dangerous to drive in

Paris_banlieue_map <- get_map(location="Paris", 
                      maptype="satellite", 
                      zoom = 10)
ggmap(Paris_banlieue_map, 
      extent = "device") + 
  geom_point(aes(x = dataset_cara_lieux_coord$long, 
                 y = dataset_cara_lieux_coord$lat), 
             alpha = 0.9, colour="red",
             size = 1, data = dataset_cara_lieux_coord)
## 51277 - 48821 = 2 456 accidents in Paris and in the suburb
Paris_map <- get_map(location="Paris", 
                              maptype="satellite", 
                              zoom = 12)
ggmap(Paris_map, 
      extent = "device") + 
  geom_point(aes(x = dataset_cara_lieux_coord$long, 
                 y = dataset_cara_lieux_coord$lat), 
             alpha = 0.9, colour="red",
             size = 1, data = dataset_cara_lieux_coord)
## 51277 - 50943 = 334 accidents in Paris

##Map Marseille
Marseille_map <- get_map(location="Marseille", 
                     maptype="satellite", 
                     zoom = 12)
ggmap(Marseille_map, 
      extent = "device") + 
  geom_point(aes(x = dataset_cara_lieux_coord$long, 
                 y = dataset_cara_lieux_coord$lat), 
             alpha = 0.9, colour="red",
             size = 1, data = dataset_cara_lieux_coord)
## 51277 - 49314 = 1963 accidents in Marseille

##Map Lyon

Lyon_map <- get_map(location="Lyon", 
                         maptype="satellite", 
                         zoom = 12)
ggmap(Lyon_map, 
      extent = "device") + 
  geom_point(aes(x = dataset_cara_lieux_coord$long, 
                 y = dataset_cara_lieux_coord$lat), 
             alpha = 0.9, colour="red",
             size = 1, data = dataset_cara_lieux_coord)

## 51277 - 50723 = 554 accidents in Lyon


