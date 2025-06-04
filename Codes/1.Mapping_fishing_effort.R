library(tidyverse)
library(sf)
library(raster)

fishing_dat = read.csv("Input_files/Imran_datasheet.csv")
geocoded_loc=read.csv("Input_files/Geocoded_lat.csv")#locations geocoded manually from Gmaps
fishing_dat=merge(fishing_dat, geocoded_loc[,c(1,5,6)], by.x = "Catch_site", by.y = "Location")
fishing_dat = fishing_dat[-which(fishing_dat$Vessel_Number==690),]#outside boat

#Checking the accuracy of fisher depth perception
true_fish_loc = read.csv("Input_files/Fishing_locations.csv")
true_fish_loc=true_fish_loc[which(!is.na(true_fish_loc$Depth_recorded_wav)),]
gebco=raster("GIS/GEBCO_29_Oct_2022/gebco_2022_n26.0_s6.0_w67.0_e92.0.tif")
true_fish_loc$true_depth = -1 * extract(gebco, cbind(true_fish_loc$deg_lon, true_fish_loc$deg_lat))
true_fish_loc = true_fish_loc[which(true_fish_loc$true_depth>=0 & true_fish_loc$true_depth<50),]
dep_mod=lm(true_depth~Depth_recorded_wav, true_fish_loc[which(true_fish_loc$ID_type != "GPS - fisher"),])
summary(dep_mod)

#Classified boats by size as follows: a) Small, b) Medium - 1 day (Kingfish vs Mackerel) vs multiple days (Kingfish), c) Large - Trawlers

fishing_dat = fishing_dat[which(fishing_dat$Vessel_type!=""),]
fishing_dat$Catch_depth_m = predict(dep_mod, data.frame(Depth_recorded_wav = fishing_dat$Catch_depth_wav))
fishing_dat$Date = as.Date(fishing_dat$Date, "%d-%m-%Y")
fishing_dat$season = ifelse(fishing_dat$Date < as.Date("01-02-2023", "%d-%m-%Y"), "Post-monsoon", 
                            ifelse(fishing_dat$Date < as.Date("01-06-2023", "%d-%m-%Y"), "Pre-monsoon",
                                   "Monsoon"))
fishing_dat = fishing_dat[which(!is.na(fishing_dat$season)),]

#Re-assigning closest known trawling locations
fishing_dat$Catch_site[which(fishing_dat$Vessel_type == "Large" & fishing_dat$Catch_site == "Kunkeshwar")] = "Devgadh"
fishing_dat$Catch_site[which(fishing_dat$Vessel_type == "Large" & fishing_dat$Catch_site == "Devbagh")] = "Lighthouse"
fishing_dat$Catch_site[which(fishing_dat$Vessel_type == "Large" & fishing_dat$Catch_site == "Nevati rock")] = "Lighthouse"
fishing_dat$Catch_site[which(fishing_dat$Vessel_type == "Large" & fishing_dat$Catch_site == "Redi")] = "Vengurla"
fishing_dat = fishing_dat[-which(fishing_dat$Vessel_type == "Large" & fishing_dat$Catch_site %in% c("Ratnagiri", "Ganpatipule", "Purnagad","Kulsunde","Pawas","Redi")),]

#Categorising according to gear
fishing_dat$Gear = NA
fishing_dat$Gear[which(fishing_dat$Gear_used == "Gillnet" & fishing_dat$Vessel_type == "Small" & fishing_dat$Mesh_size_mm <= 50)] = "Gillnet: Small-mesh"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Gillnet" & fishing_dat$Vessel_type == "Small" & fishing_dat$Mesh_size_mm > 50)] = "Gillnet: Large-mesh"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Hook and line" & fishing_dat$Vessel_type == "Small")] = "Hook & line"
fishing_dat$Gear[which(fishing_dat$Vessel_type == "Small non-motorised")] = "Small non-motorised"
fishing_dat$Gear[which(fishing_dat$Vessel_type == "Medium")] = "Gillnet: 125"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Trawler" & fishing_dat$Vessel_type == "Large")] = "Trawler"
fishing_dat$Gear[which(fishing_dat$Gear == "Gillnet: 125" | fishing_dat$Gear == "Small non-motorised")] = "Gillnet: Large-mesh"
fishing_dat$Gear[which(fishing_dat$Gear == "Hook & line")] = "Hook and line"
 
#Number of unique boats in each category
fishing_dat |> group_by(Vessel_Number) |> filter(row_number()==1) |> group_by(Vessel_type, season) |> count()#|> filter(Data_level == "Site")

fishing_dat_uni=fishing_dat |> group_by(Vessel_Number) |> filter(row_number()==1) #each boat has been recorded as multiple rows

#Estimating lat grid sizes for all sites i.e., how big of an area do fishers mean when they refer to a village/landmark? this reference will vary by boat size.
fishing_loc_lat = read.csv("Input_files/Fishing_locations_lat.csv")
fishing_loc_lat = fishing_loc_lat[-which(is.na(fishing_loc_lat$lat_deg_updated)),]
fishing_loc_lat = fishing_loc_lat[!duplicated(cbind(fishing_loc_lat$ID,fishing_loc_lat$lat_deg_updated)),]
fishing_loc_lat |> filter(Location_updated != "") |> group_by(Source) |> count()
geocoded_loc=read.csv("Input_files/Geocoded_lat.csv")
geocoded_loc=geocoded_loc[,c(1,5,6,7,8)]

#grid size for small boats
x=fishing_loc_lat |> filter(Vessel_type == "Small") |> group_by(Location) |> summarise("lat_mean" = mean(lat_deg_updated), "lat_sd" = sd(lat_deg_updated), "lat_max" = max(lat_deg_updated), "lat_min" = min(lat_deg_updated))
lat_sd=mean(x$lat_sd, na.rm=T)
geocoded_loc$Centroid_lat_ub_small=geocoded_loc$Centroid_lat+lat_sd
geocoded_loc$Centroid_lat_lb_small=geocoded_loc$Centroid_lat-lat_sd

#grid size for small non-motorised boats
lat_sd=0.003# set up from speaking to fishers?
geocoded_loc$Centroid_lat_ub_small_nm=geocoded_loc$Centroid_lat+lat_sd
geocoded_loc$Centroid_lat_lb_small_nm=geocoded_loc$Centroid_lat-lat_sd

#grid size for medium boats
x=fishing_loc_lat |> filter(Vessel_type == "Medium") |> group_by(Location) |> summarise("lat_mean" = mean(lat_deg_updated), "lat_sd" = sd(lat_deg_updated), "lat_max" = max(lat_deg_updated), "lat_min" = min(lat_deg_updated))
lat_sd=mean(x$lat_sd, na.rm=T)
geocoded_loc$Centroid_lat_ub_medium=geocoded_loc$Centroid_lat+lat_sd
geocoded_loc$Centroid_lat_lb_medium=geocoded_loc$Centroid_lat-lat_sd

#write.csv(geocoded_loc, "Input_files/Geocoded_lat_full.csv", row.names = F)

#grid size for large boats: use min, max lat and not sd because trawling areas (lats) are fixed: interviewed fishers + GPS locations; this is already entered in the goecoded_loc.csv; Fishers start trawling at Malvan and then move or stay depending on fish catch.

##########################
#Estimating overall space use (latitude) by boats. Each village's lat grid is defined by boundaries estimated by the above code. The frequency of villages visited for fishing is translated to latitudinal values at a resolution of 0.01 degrees

are.equal <- function(x, y, tol = .Machine$double.eps^0.5) abs(x - y) < tol # this function is to overcome R's inherent issue in comparing values. Try .3+.6 == .9

lat_range_est = function(sm, geocoded_loc, season_1, vessel_type) #This function will estimate lat range for different boat types
{
  ves_lat_range = ifelse(vessel_type == "LT", 5, ifelse(vessel_type == "SM", 7, ifelse(vessel_type == "SNM", 9, 11)))
  sm = sm |> filter(season == season_1, !is.na(Centroid_lat))
  x = data.frame(table(sm$Catch_site))
  x = merge(x,geocoded_loc, by.x="Var1", by.y = "Location")
  t = 0
  for(i in 1:nrow(x))
    t=c(t, rep(seq(round(x[i,ves_lat_range+1],2), round(x[i,ves_lat_range],2), by = 0.01), x$Freq[i]))
  t=t[-1]
  # den_plot = (density(t))
  # plot(den_plot)
  # den_plot$x = round(den_plot$x,2)#1 decimal place = 1 km
  # #den_plot$y = den_plot$y/sum(den_plot$y)
  # la = data.frame("lat_bin" = seq(round(range(c(x[,ves_lat_range+1], x[,ves_lat_range]), na.rm=T),2)[1], round(range(c(x[,ves_lat_range+1], x[,ves_lat_range]), na.rm=T),2)[2], 0.01))
  # for(i in 1:nrow(la))
  #   la$Freq[i] = sum(den_plot$y[which(are.equal(den_plot$x, la$lat_bin[i]))])/sum(x$Freq)#normalising for number of boats sampled
  # la$Freq = la$Freq/sum(la$Freq)
  # if(vessel_type == "LT") # This is because trawlers are fishing in each latitudinal grid while other boats are fishing at one point somewhere in a larger grid
  #   {
  la = data.frame(table(t))
  la$Freq = la$Freq/sum(x$Freq)
  colnames(la) = c("lat_bin", "Freq")
    # }
  la$lat_bin = as.numeric(as.character(la$lat_bin))-16.05
  return(la)
}

dep_range_est = function(sm, season_1) # This function will estimate depth use for different boat types
{
  sm = sm |> filter(season == season_1)
  den_plot = density(na.omit(sm$Catch_depth_m), adjust=1)
  plot(den_plot)
  den_plot$x = round(den_plot$x,0)
  #den_plot$y = den_plot$y/sum(den_plot$y)
  de=data.frame("dep_bin" = seq(round(range(sm$Catch_depth_m, na.rm=T),0)[1],round(range(sm$Catch_depth_m, na.rm=T),0)[2],1))
  for(i in 1:nrow(de))
    de$Freq[i] = sum(den_plot$y[which(den_plot$x == de$dep_bin[i])])
  de$Freq = de$Freq/sum(de$Freq)
  return(de)
}

season_1 = c("Post-monsoon","Pre-monsoon","Monsoon")[1] # Change this and rerun the code below for fishing patterns in different seasons
#1.
#For small motorised boats (#gillnet mesh size <50mm = mackerel, >50 = others)
sm = fishing_dat_uni |> filter(Vessel_type == "Small", Mesh_size_mm <= 50, Landing_site %in% c("Malvan","Dandi"), Gear_used == "Gillnet")
sm = sm[-which(sm$Centroid_lat<16.04 | sm$Centroid_lat>16.07),]# Removing outliers; these are locations where fishers go only when they are informed of fish presence there, so is not generalisable
smmla = lat_range_est(sm, geocoded_loc, season_1, "SM")
smmde = dep_range_est(sm, season_1)

#2.
#For small motorised larger gillnet, should be added to non-motorised FE estimates later
sm = fishing_dat_uni |> filter(Vessel_type == "Small", Mesh_size_mm > 50, Landing_site %in% c("Malvan","Dandi"), Gear_used == "Gillnet")
sm = sm[-which(sm$Centroid_lat>16.2),]#outliers, boats only go that far when there is news of fish, so trends are not generalisable
smlgla= lat_range_est(sm, geocoded_loc, season_1, "SM")
smlgde = dep_range_est(sm, season_1)

#3.
#For small motorised boats Hook and line
sm = fishing_dat_uni |> filter(Vessel_type == "Small", Landing_site %in% c("Malvan","Dandi"), Gear_used == "Hook and line")
sm = sm[-which(sm$Centroid_lat<16.04 | sm$Centroid_lat>16.07),]# Removing outliers; these are locations where fishers go only when they are informed of fish presence there, so is not generalisable
smmhlla = lat_range_est(sm, geocoded_loc, season_1, "SM")
smmhlde = dep_range_est(sm, season_1)
#smmhlla = smmhlde = NA #for pre-monsoon

#4.
#For small non-motorised boats
smnmla=data.frame("lat_bin" = 16.05-16.05, "Freq" = 1) # info after speaking to fishers?
sm = fishing_dat_uni |> filter(Vessel_type == "Small non-motorised")#<50 = bangda, >50 = others
smnmde = dep_range_est(sm, season_1)

#5.
#For medium motorised kingfish boats
sm = fishing_dat_uni |> filter(Vessel_type == "Medium", Landing_site %in% c("Malvan","Dandi"), Mesh_size_mm == 125)
sm = sm[-which(sm$Vessel_Number == 33 | sm$Vessel_Number == 64),]#outliers, probably non-Malvan boats
sm = sm[-which(sm$Centroid_lat>16.2),]#outliers, boats only go that far when there is news of fish, so trends are not generalisable
#sm=sm[-which(is.na(sm$Centroid_lat)),]#removing for lat range est
mmsjla = lat_range_est(sm, geocoded_loc, season_1, "MM")
mmsjde = dep_range_est(sm, season_1)

#6.
#For Trawler
sm = fishing_dat_uni |> filter(Vessel_type == "Large", Landing_site %in% c("Malvan","Dandi"), Gear_used == "Trawler")
lmtla = lat_range_est(sm, geocoded_loc, season_1, "LT")
lmtde = dep_range_est(sm, season_1)

#######################
#Estimating average grid-wise probability of fishing at the study site
coast_boats = read.csv("Input_files/Coastline_boats_WideForm.csv")
coast_boats = coast_boats[-which(is.na(coast_boats$Small)),]# removing empty rows. has nothing to do with 'Small' boats
study_area = read.csv("GIS/Study_area_Malvan.csv")
colnames(study_area)[6] = "depth"
study_area$depth = round(study_area$depth * -1, 0)

FE = data.frame("grid_id" = study_area$id)
FE$SM = FE$SMLG = FE$SMHL = FE$SNM = FE$MSJ = FE$LT = 0 # Creating columns for fishing effort; removed FE$MNSJ

#Extrapolating fishing effort to study area using above information

#These ratios represent the proportion of boats with particular gear active/sampled during each season
#ratios are in terms of number of boats sampled per day (for days when those boats were sampled. e.g., trawler vs small boats)
rat_SM = ifelse(season_1 == "Post-monsoon", (9.16-5/30)/12.14, ifelse(season_1 == "Pre-monsoon", (9.16-6/29)/12.14,12.14/12.14))#these 5 and 6 boats are from the main catch sheet
rat_SMLG = ifelse(season_1 == "Post-monsoon", (5/30)/12.14, ifelse(season_1 == "Pre-monsoon", (6/29)/12.14,0/12.14))
rat_SMHL = ifelse(season_1 == "Post-monsoon", 1.03/12.14, ifelse(season_1 == "Pre-monsoon", 0,0))
rat_SNM = ifelse(season_1 == "Post-monsoon", 0.1/0.43, ifelse(season_1 == "Pre-monsoon", 0.07/0.43,0.43/0.43))
rat_MM = ifelse(season_1 == "Post-monsoon", 7.03/7.03, ifelse(season_1 == "Pre-monsoon", 2.4/7.03,2.7/7.03))
rat_LT = ifelse(season_1 == "Post-monsoon", 10.95/11.96, ifelse(season_1 == "Pre-monsoon", 11.96/11.96,2/11.96))


#important information: Grid value increases by 1 when going south
#important information: Grid value increases by 318 when going east
FE_est = function(lat_use, depth_use, study_area, study_area_lat_dim, boat_use_ratio, coast_boats, Gear_type, FE)
{
  col_num = which(colnames(FE) == Gear_type)
for(la in 1:nrow(lat_use))
{
  la_id = id + lat_use$lat_bin[la]/0.01 # 1 cell = 0.01 degrees
  row = la_id %% study_area_lat_dim
  if(row==0)
    row=study_area_lat_dim
  lo_id = data.frame("lon_cells" = seq(row, max(study_area$id), by = study_area_lat_dim), "depth" = study_area$depth[seq(row, max(study_area$id), by = study_area_lat_dim)])
  lo_id = lo_id[lo_id$depth %in% depth_use$dep_bin,]
  lo_id = lo_id[order(lo_id$depth),]
  for(j in 1:nrow(lo_id))
  {
    FE[which(FE$grid_id == lo_id$lon_cells[j]), col_num] = FE[which(FE$grid_id == lo_id$lon_cells[j]), col_num] + (lat_use$Freq[la] * depth_use$Freq[which(depth_use$dep_bin == lo_id$depth[j])] * coast_boats[i] * boat_use_ratio)
  }
}
  return(FE)
}

for(i in 1:nrow(coast_boats)) # Each cell
{
  id =  coast_boats$Grid_ID_SA[i]
  #1. Small gillnets
  FE = FE_est(lat_use = smmla, depth_use = smmde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_SM, coast_boats = coast_boats$Small, Gear_type = "SM", FE = FE)

  #2. Small larger gillnet
  FE = FE_est(lat_use = smlgla, depth_use = smlgde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_SMLG, coast_boats = coast_boats$Small, Gear_type = "SMLG", FE = FE)

  #3. Small Hook and line, block this code for pre monsoon season
  FE = FE_est(lat_use = smmhlla, depth_use = smmhlde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_SMHL, coast_boats = coast_boats$Small, Gear_type = "SMHL", FE = FE)

  #4. Small non-motorised
  FE = FE_est(lat_use = smnmla, depth_use = smnmde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_SNM, coast_boats = coast_boats$Non.motorised, Gear_type = "SNM", FE = FE)

  #5. Medium kingfish net
  FE = FE_est(lat_use = mmsjla, depth_use = mmsjde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_MM, coast_boats = coast_boats$Surmai, Gear_type = "MSJ", FE = FE)

  #6. Trawler
  FE = FE_est(lat_use = lmtla, depth_use = lmtde, study_area = study_area, study_area_lat_dim = 318, boat_use_ratio = rat_LT, coast_boats = coast_boats$Trawler, Gear_type = "LT", FE = FE)
}

#Removing grids that are artefacts of the analyses
FE$LT[1:56984] = 0
#FE$MNSJ[c(1:52204, 59717:59721, 59400:59403, 52611, 72657)] = 0
FE$MSJ[c(1:53480, 59717:59721, 59400:59403, 60036:60039, 53543:53567, 53867, 72657)] = 0
FE$SM[1:63036] = FE$SMHL[1:63036] = FE$SMLG[1:63036] = 0

#FE is in terms of average number of boats fishing in that grid per day
FE$Total = FE$LT+FE$SMLG+FE$MSJ+FE$SNM+FE$SM+FE$SMHL
FE$Gillnet = FE$SMLG+FE$MSJ+FE$SNM+FE$SM
FE$Large_gillnet = FE$SMLG+FE$SNM+FE$MSJ
summary(FE)
write.csv(FE, "Results/FE_strandardised_M.csv", row.names = F)