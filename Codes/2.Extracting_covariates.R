#This code extracts environmental covariates for the fishing data
library(tidyverse)
library(raster)
fishing_dat = read.csv("Input_files/Imran_datasheet.csv")
fishing_dat = fishing_dat[-which(fishing_dat$Vessel_Number==690),]#outside boat

#Checking the accuracy of fisher depth perception
true_fish_loc = read.csv("Input_files/Fishing_locations.csv")
true_fish_loc=true_fish_loc[which(!is.na(true_fish_loc$Depth_recorded_wav)),]
gebco=raster("Input_files/Rasters/GEBCO_29_Oct_2022/gebco_2022_n26.0_s6.0_w67.0_e92.0.tif")
true_fish_loc$true_depth = -1 * raster::extract(gebco, cbind(true_fish_loc$deg_lon, true_fish_loc$deg_lat))
true_fish_loc = true_fish_loc[which(true_fish_loc$true_depth>=0 & true_fish_loc$true_depth<50),]
dep_mod=lm(true_depth~Depth_recorded_wav, true_fish_loc[which(true_fish_loc$ID_type != "GPS - fisher"),])
fishing_dat$Catch_depth_m = predict(dep_mod, data.frame(Depth_recorded_wav = fishing_dat$Catch_depth_wav))


fishing_dat = fishing_dat[which(fishing_dat$Vessel_type!=""),]
fishing_dat$Date = as.Date(fishing_dat$Date, "%d-%m-%Y")
fishing_dat$season = ifelse(fishing_dat$Date < as.Date("01-02-2023", "%d-%m-%Y"), "Post-monsoon", 
                            ifelse(fishing_dat$Date < as.Date("01-06-2023", "%d-%m-%Y"), "Pre-monsoon", "Monsoon"))
fishing_dat = fishing_dat[which(!is.na(fishing_dat$season)),]


fishing_dat$group = "Fish"

#Classification 1
fishing_dat$group[which(fishing_dat$Species == "Blacktip shark" | fishing_dat$Species == "Spadenose shark" | fishing_dat$Species == "Hammerhead shark" | fishing_dat$Species == "Bamboo shark" | fishing_dat$Species == "Tiger shark" | fishing_dat$Species == "Bull shark" | fishing_dat$Species == "Graceful shark" | fishing_dat$Species == "Milk shark")] = "Sharks"

fishing_dat$group[which(fishing_dat$Species == "Widenose guitarfish" | fishing_dat$Species == "Sharpnose guitarfish" | fishing_dat$Species == "Bowmouth guitarfish")] = "Skates"

fishing_dat$group[which(fishing_dat$Species == "Longheaded eagle ray" | fishing_dat$Species == "Eagle ray" | fishing_dat$Species == "Spotted eagle ray" | fishing_dat$Species == "Scaly whipray" | fishing_dat$Species == "Whitespotted whipray" | fishing_dat$Species == "Leopard whipray" | fishing_dat$Species == "Reticulated whipray" | fishing_dat$Species == "Ray sp." | fishing_dat$Species == "Devil ray sp.")] = "Rays"

#Categorising according to gear
fishing_dat$Gear = NA
fishing_dat$Gear[which(fishing_dat$Gear_used == "Gillnet" & fishing_dat$Vessel_type == "Small" & fishing_dat$Mesh_size_mm <= 50)] = "Gillnet: Small-mesh"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Gillnet" & fishing_dat$Vessel_type == "Small" & fishing_dat$Mesh_size_mm > 50)] = "Gillnet: Large-mesh"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Hook and line" & fishing_dat$Vessel_type == "Small")] = "Hook & line"
fishing_dat$Gear[which(fishing_dat$Vessel_type == "Small non-motorised")] = "Small non-motorised"
fishing_dat$Gear[which(fishing_dat$Vessel_type == "Medium")] = "Gillnet: 125"
fishing_dat$Gear[which(fishing_dat$Gear_used == "Trawler" & fishing_dat$Vessel_type == "Large")] = "Trawler"

#Fishing effort
fishing_dat$Duration_days = NA
fishing_dat$Duration_days[which(fishing_dat$Vessel_type == "Small")] = 
fishing_dat$Duration_days[which(fishing_dat$Vessel_type == "Small non-motorised")] = 1
fishing_dat$Duration_days[which(fishing_dat$Vessel_type == "Medium")] = fishing_dat$Fishing_attempt_number[which(fishing_dat$Vessel_type == "Medium")]# each day is recorded as one attempt
fishing_dat$Duration_days[which(fishing_dat$Gear == "Trawler")] = 
  fishing_dat$Fishing_attempt_number[which(fishing_dat$Gear == "Trawler")]/3 # each day has 3 fishing attempts
fishing_dat$Duration_days = ceiling(fishing_dat$Duration_days)
vessel_drn = fishing_dat |> group_by(Vessel_Number) |> summarise(last(Duration_days))
fishing_dat=merge(fishing_dat, vessel_drn, by = "Vessel_Number")
fishing_dat$Duration_days = fishing_dat$`last(Duration_days)`
fishing_dat = fishing_dat[,-ncol(fishing_dat)]
#View(fishing_dat[which(is.na(fishing_dat$Duration_days)),])
#Catch per day
fishing_dat$CPUE = fishing_dat$Total_weight_kg/fishing_dat$Duration_days

fishing_clean = fishing_dat[,c(1,7,9,17,23:27,31:35)]
fishing_clean[,c(1,5,7:10,14)] = sapply(fishing_clean[,c(1,5,7:10,14)],as.numeric)
#expand by adding rows that have 0 elasmobranch catch
vn = unique(fishing_clean$Vessel_Number)
groups = unique(fishing_clean$group)
for(i in 1:length(vn))
{
  row = which(fishing_clean$Vessel_Number == vn[i])
  groups_caught = unique(fishing_clean$group[row])
  fishing_clean = bind_rows(fishing_clean, bind_cols(
                                         "Vessel_Number" = vn[i],
                                         "Sampling" = fishing_clean$Sampling[row[1]],
                                         "Catch_site" = fishing_clean$Catch_site[row[1]],
                                         "Vessel_type" = fishing_clean$Vessel_type[row[1]],
                                        "Catch_depth_m" = fishing_clean$Catch_depth_m[row[1]],
                                         "Species" = as.character(NA),
                                         "Total_weight_kg" = 0,
                                         "Average_length_cm" = NA,
                                         "No_of_Individuals" = 0,
                                         "CPUE" = 0,
                                         "season" = fishing_clean$season[row[1]],
                                         "group" = groups[-which(groups %in% groups_caught)],
                                         "Gear" = fishing_clean$Gear[row[1]],
                                         "Duration_days" = fishing_clean$Duration_days[row[1]]))
}
fishing_clean = fishing_clean[order(fishing_clean$Vessel_Number),]

#Subsetting for elasmobranchs and cleaning dataset
fishing_clean = fishing_clean[which(fishing_clean$group != "Fish"),]
#fishing_clean = fishing_clean[-which(is.na(fishing_clean$Total_weight_kg)),]
fishing_clean = fishing_clean[-which(is.na(fishing_clean$No_of_Individuals)),]
fishing_clean = fishing_clean[-which(is.na(fishing_clean$Catch_depth_m)),]
fishing_clean = fishing_clean[-which(is.na(fishing_clean$Duration_days)),]
fishing_clean = fishing_clean[-which(is.na(fishing_clean$Gear)),]
fishing_clean = fishing_clean[-which(fishing_clean$Catch_site == ""),]
fishing_clean$Catch_site[which(fishing_clean$Catch_site == "Kavda rock")] = "Rock garden"
fishing_clean$Catch_site[which(fishing_clean$Catch_site == "Juva")] = "Talashil"
fishing_clean$Catch_site[which(fishing_clean$Catch_site == "Kulsunde")] = "Vijaydurg"
fishing_clean$Catch_site[which(fishing_clean$Catch_site == "Nate")] = "Vijaydurg"
fishing_clean = fishing_clean[-which(fishing_clean$Catch_site %in% c("Betul", "Goa", "Dhavpani","Panaji")),]
fishing_clean = fishing_clean[-which((fishing_clean$Catch_site %in% c("Ganpatipule","Kunkeshwar","Pawas","Purnagad","Ratnagiri","Vijaydurg","Talashil","Rock garden","Nevati rock")) & fishing_clean$Vessel_type == "Large"),]

#Extracting data from rasters
ras_files = list.files("./Input_files/Rasters/Processed/", ".tif$", full.names = T)
hab = stack(ras_files[c(4,5,6,10)])
ocn = stack(ras_files[c(1,2,3,7,8,9,11,12,13)])

depth = raster("./Input_files/Rasters/Processed/Depth.tif")
depth = as.data.frame(depth, xy=T)
depth[,1:2] = sapply(depth[,1:2], round, digits=2)
depth$Depth = depth$Depth * -1
geocoded_loc = read.csv("Input_files/Geocoded_lat_full.csv")
vn = unique(fishing_clean$Vessel_Number)
fishing_final = data.frame()
for(i in 1:length(vn))
{
  row = which(fishing_clean$Vessel_Number == vn[i])
  loc = fishing_clean$Catch_site[row[1]]
  dep = round(fishing_clean$Catch_depth_m[row[1]])
  seas = fishing_clean$season[row[1]]
  #lat
  vessel_type = fishing_clean$Vessel_type[row[1]]
  col = ifelse(vessel_type == "Large", 5, 
        ifelse(vessel_type == "Medium", 11,
        ifelse(vessel_type == "Small", 7, 9)))
  lat_lb = geocoded_loc[which(geocoded_loc$Location == loc),col]
  lat_ub = geocoded_loc[which(geocoded_loc$Location == loc),col-1]
  lats = seq(round(lat_lb,2), round(lat_ub,2), by = 0.01)
  #lon
  lons=NULL
  while(length(lons) == 0)
  {
    if(is.na(dep))
      break
    depth_area = depth[depth$y %in% lats,]
    lons = unique(depth_area$x[which(depth_area$Depth == dep)])
    dep=dep+1
  }
  #All locations
  locs = expand.grid(lons, lats)
  colnames(locs) = c("lat","lon")
  fishing_final = rbind(fishing_final, cbind(Vessel_Number = vn[i], n_cells = nrow(locs), data.frame(cbind(raster::extract(hab,locs), raster::extract(ocn,locs))) |> summarise_all(median, na.rm=T), data.frame(cbind(raster::extract(hab,locs), raster::extract(ocn,locs))) |> summarise_all(sd, na.rm=T)))
}
colnames(fishing_final) = c("Vessel_Number","n_cells","Depth","Dist_mainland_km","Distance_RiverMouth_km","Slope","chl_M","chl_PostM","chl_PreM","EKE_M","EKE_PostM","EKE_PreM","SST_M","SST_PostM","SST_PreM","Depth_sd","Dist_mainland_km_sd","Distance_RiverMouth_km_sd","Slope_sd","chl_M_sd","chl_PostM_sd","chl_PreM_sd","EKE_M_sd","EKE_PostM_sd","EKE_PreM_sd","SST_M_sd","SST_PostM_sd","SST_PreM_sd")
fishing_final$Depth = fishing_final$Depth * -1
fishing_final = merge(fishing_clean, fishing_final, by="Vessel_Number")

fishing_final$chl = NA
fishing_final$chl[which(fishing_final$season == "Post-monsoon")] = fishing_final$chl_PostM[which(fishing_final$season == "Post-monsoon")]
fishing_final$chl[which(fishing_final$season == "Pre-monsoon")] = fishing_final$chl_PreM[which(fishing_final$season == "Pre-monsoon")]
fishing_final$chl[which(fishing_final$season == "Monsoon")] = fishing_final$chl_PreM[which(fishing_final$season == "Monsoon")]

fishing_final$chl_sd = NA
fishing_final$chl_sd[which(fishing_final$season == "Post-monsoon")] = fishing_final$chl_PostM_sd[which(fishing_final$season == "Post-monsoon")]
fishing_final$chl_sd[which(fishing_final$season == "Pre-monsoon")] = fishing_final$chl_PreM_sd[which(fishing_final$season == "Pre-monsoon")]
fishing_final$chl_sd[which(fishing_final$season == "Monsoon")] = fishing_final$chl_PreM_sd[which(fishing_final$season == "Monsoon")]

fishing_final$eke = NA
fishing_final$eke[which(fishing_final$season == "Post-monsoon")] = fishing_final$EKE_PostM[which(fishing_final$season == "Post-monsoon")]
fishing_final$eke[which(fishing_final$season == "Pre-monsoon")] = fishing_final$EKE_PreM[which(fishing_final$season == "Pre-monsoon")]
fishing_final$eke[which(fishing_final$season == "Monsoon")] = fishing_final$EKE_PreM[which(fishing_final$season == "Monsoon")]

fishing_final$eke_sd = NA
fishing_final$eke_sd[which(fishing_final$season == "Post-monsoon")] = fishing_final$EKE_PostM_sd[which(fishing_final$season == "Post-monsoon")]
fishing_final$eke_sd[which(fishing_final$season == "Pre-monsoon")] = fishing_final$EKE_PreM_sd[which(fishing_final$season == "Pre-monsoon")]
fishing_final$eke_sd[which(fishing_final$season == "Monsoon")] = fishing_final$EKE_PreM_sd[which(fishing_final$season == "Monsoon")]

fishing_final$sst = NA
fishing_final$sst[which(fishing_final$season == "Post-monsoon")] = fishing_final$SST_PostM[which(fishing_final$season == "Post-monsoon")]
fishing_final$sst[which(fishing_final$season == "Pre-monsoon")] = fishing_final$SST_PreM[which(fishing_final$season == "Pre-monsoon")]
fishing_final$sst[which(fishing_final$season == "Monsoon")] = fishing_final$SST_PreM[which(fishing_final$season == "Monsoon")]

fishing_final$sst_sd = NA
fishing_final$sst_sd[which(fishing_final$season == "Post-monsoon")] = fishing_final$SST_PostM_sd[which(fishing_final$season == "Post-monsoon")]
fishing_final$sst_sd[which(fishing_final$season == "Pre-monsoon")] = fishing_final$SST_PreM_sd[which(fishing_final$season == "Pre-monsoon")]
fishing_final$sst_sd[which(fishing_final$season == "Monsoon")] = fishing_final$SST_PreM_sd[which(fishing_final$season == "Monsoon")]

fishing_final = fishing_final[,-c(20:28,33:41)]
fishing_final[,c(20:23)] = fishing_final[,c(20:23)]/fishing_final[,c(16:19)]*100
fishing_final[,c(25,27,29)] = fishing_final[,c(25,27,29)]/fishing_final[,c(24,26,28)]*100
fishing_final$Gear[which(fishing_final$Gear %in% c("Small non-motorised","Gillnet: 125"))] = "Gillnet: Large-mesh"
 
# write.csv(fishing_final,"./Results/Data_Malvan_final_CV_1km_median.csv", row.names = F)