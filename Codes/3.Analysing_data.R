#This code uses hurdle models to analyse elasmobranch catch data
library(tidyverse)

dat = read.csv("./Results/Data_combined_final_1km_median.csv")

#Cleaning and formatting data
dat = dat[,-(22:30)]
dat$Gear[which(dat$Gear == "Gillnet: 125" | dat$Gear == "Small non-motorised")] = "Gillnet: Large-mesh"
dat$Gear[which(dat$Gear == "Hook & line")] = "Hook and line"
dat$Depth[which(dat$Depth>100)] = NA #erroneous locations in Visakhapatnam
dat = dat[which(dat$Sampling != "Opportunistic"),]

#classifying species
dat$Species_grp = "Pelagic"
dat$Species_grp[which(dat$Species == "Bamboo shark" | dat$Species == "Spadenose shark" | dat$Species == "Grey Sharpnose shark" | dat$Species == "Milk shark" | dat$Species == "Leopard whipray" | dat$Species == "Reticulate whipray" | dat$Species == "Scaly whipray" | dat$Species == "Whitespotted whipray" | dat$Species == "Ray" | dat$Species == "Sting ray" | dat$Species == "Electric ray" | dat$Species == "Butterfly ray" |  dat$Species == "Skates" | dat$Species == "Sharpnose guitarfish" | dat$Species == "Widenose guitarfish" | dat$Species == "Guitarfish")] = "Benthic"

#Scaling predictors
dat_m=dat[which(dat$Location=="Malvan"),]
dat_m[,18:24] = sapply(dat_m[,18:24],scale)
dat_v=dat[which(dat$Location!="Malvan"),]
dat_v[,18:24] = sapply(dat_v[,18:24],scale)
dat=rbind(dat_m,dat_v)

#Appending data with '0' value rows for pelagic and benthic species (See file 3)
vn = unique(dat$Vessel_Number)
caught = expand.grid(unique(dat$group), unique(dat$Species_grp))
for(i in 1:length(vn))
{
  row = which(dat$Vessel_Number == vn[i])
  all_caught = data.frame(cbind(dat$group[row], dat$Species_grp[row]))
  missing = cbind(caught, present = interaction(caught) %in% interaction(all_caught))
  missing = missing[missing$present == F,]
  dat = bind_rows(dat, bind_cols(
    "Location" = dat$Location[row[1]],
    "Vessel_Number" = vn[i],
    "Sampling" = dat$Sampling[row[1]],
    "Vessel_type" = dat$Vessel_type[row[1]],
    "Catch_site" = dat$Catch_site[row[1]],
    "Catch_depth_m" = dat$Catch_depth_m[row[1]],
    "Catch_dist_km" = dat$Catch_dist_km[row[1]],
    "Species" = as.character(NA),
    "Total_weight_kg" = 0,
    "Average_length_cm" = NA,
    "No_of_Individuals" = 0,
    "CPUE" = 0,
    "season" = dat$season[row[1]],
    "group" = missing$Var1,
    "Gear" = dat$Gear[row[1]],
    "Duration_days" = dat$Duration_days[row[1]],
    "n_cells" = dat$n_cells[row[1]],
    "Depth" = dat$Depth[row[1]],
    "Dist_mainland_km" = dat$Dist_mainland_km[row[1]],
    "Distance_RiverMouth_km" = dat$Distance_RiverMouth_km[row[1]],
    "Slope" = dat$Slope[row[1]],
    "chl" = dat$chl[row[1]],
    "eke" = dat$eke[row[1]],
    "sst" = dat$sst[row[1]],
    "Species_grp" = missing$Var2
    ))
}
dat = dat[order(dat$Vessel_Number),]
dat = dat[-which(dat$Species_grp == "Pelagic" & dat$group == "Skates"),]

########################Modelling elasmobranch catch####################
library(pscl)

#Defining models according to predictions
formulaa1 = "No_of_Individuals ~ group+Species_grp+Location+Gear+Depth+Slope+eke+sst"
formulaa2 = "No_of_Individuals ~ group+Species_grp+Location*Gear+Depth+Slope+eke+sst"
formulaa3 = "No_of_Individuals ~ group+Species_grp+Gear+Location*(Depth+Slope+eke+sst)"
formulaa4 = "No_of_Individuals ~ group+Species_grp+Location*(Gear+Depth+Slope+eke+sst)"
formulaa5 = "No_of_Individuals ~ Location*(group+Species_grp+Gear+Depth+Slope+eke+sst)"

#Running hurdle models for the formulae above with both poisson and neg bin distributions
for(i in 1:5)
{
  formula_1 = get(paste0("formulaa",i))
  assign(paste0("mod3af",i), hurdle(formula = as.formula(formula_1),
                                    data = dat))
  assign(paste0("mod3bf",i), hurdle(formula = as.formula(formula_1),
                                      data = dat,
                                   dist = "negbin"))
}

#print AIC values
for(i in 1:5)
  print(AIC(get(paste0("mod3bf",i))))

mod_final = mod3bf5 # assigning the best model to a new variable for downstream clarity


#Validating the model using cross validation
library(pROC)

# Predicted counts (including zeros)
predicted_vals = predict(mod_final, type = "response")
predicted_pres = predict(mod_final, type = "prob")[, 1]
presence_obs = as.numeric(dat_pred$No_of_Individuals > 0)

#add new columns for predicted values
dat_pred = dat |> dplyr::select(No_of_Individuals,Location,group,Species_grp,Gear,Depth,Slope,eke,sst) |> na.omit() |> cbind(pred = predicted_vals, pred_p = 1- predicted_pres) |> mutate(residuals = No_of_Individuals-pred)

#Cross validation
cv_df = data=frame()
auc = rmse = r2 = NULL
for(i in 1:10)
  {
  train_index = sample(seq_len(nrow(na.omit(dat_pred))), size = 0.7 * nrow(na.omit(dat_pred)))
  train_data = na.omit(dat_pred)[train_index, ]
  test_data = na.omit(dat_pred)[-train_index, ]
  
  mod_train = hurdle(as.formula(formulaa5), data = train_data, dist = "negbin")
  
  # Predict on test set
  test_preds = predict(mod_train, newdata = test_data, type = "response")
  test_preds_pres = predict(mod_train, newdata = test_data, type = "prob")[, 1]
  
  dat_pos = test_data[test_data$No_of_Individuals > 0, ]
  pred_count = as.numeric(predict(mod_train, new_data = dat_pos, type = "count"))[1:nrow(dat_pos)]
  obs_count = dat_pos$No_of_Individuals
  
  rmse = c(rmse,sqrt(mean((obs_count - pred_count)^2)))
  r2 = c(r2,1 - sum((obs_count - pred_count)^2) / sum((obs_count - mean(obs_count))^2))
  
  #predicting presence
  presence_obs = as.numeric(test_data$No_of_Individuals > 0)
  roc_obj = roc(presence_obs, test_preds_pres)
  auc = c(auc, auc(roc_obj))
  
  cv_df = rbind(cv_df, cbind(rep = i, obs = test_data$No_of_Individuals, pred = test_preds, pres = test_preds_pres))
}

cv_df = data.frame(cv_df) |> na.omit() |> mutate(obs_pres = ifelse(obs > 0, 1, 0))

#Overall (full model)
ggplot(cv_df) + geom_point(aes(x=obs,y=pred)) + xlim(c(0,10)) + ylim(c(0,5)) + labs(x="Observed",y="Predicted") + theme_minimal(base_size = 15)

sqrt(mean((cv_df$obs - cv_df$pred)^2))
mean(abs(cv_df$obs - cv_df$pred))

# Pseudo R-squared from predictions
rss = sum((cv_df$obs - cv_df$pred)^2)
tss = sum((cv_df$obs - mean(cv_df$obs))^2)
1 - (rss / tss)#r2

#binary part
mean(auc)
#count part
mean(rmse)
mean(r2)

#getting the coefficients
coefs = data.frame(cbind(coef(mod_final),confint(mod_final)))
coefs$name = rownames(coefs)
rownames(coefs) = NULL
coefs$name = substring(coefs$name,6,nchar(coefs$name))
coefs$name[1:(nrow(coefs)/2)] = substring(coefs$name[1:(nrow(coefs)/2)],2,nchar(coefs$name[1:(nrow(coefs)/2)]))
coefs$coefs = c(rep("count",nrow(coefs)/2),rep("catch",nrow(coefs)/2))
colnames(coefs) = c("mean","lci","uci","name","coefs")
coefs$Location = NA
coefs$Location[1:12] = coefs$Location[23:34] = "Malvan"
coefs$Location[13:22] = coefs$Location[35:44] = "Visakhapatnam"

coefs$name[1:12] = coefs$name[23:34] = c("Intercept","Visakhapatnam","Sharks","Guitarfishes","Pelagic","Gillnet: small-mesh","Hook and line","Trawler","Depth","Slope","EKE","SST")
coefs$name[13:22] = coefs$name[35:44] = c("Sharks","Guitarfishes","Pelagic","Gillnet: small-mesh","Hook and line","Trawler","Depth","Slope","EKE","SST")
coefs$name = factor(coefs$name, levels = c("Intercept","Visakhapatnam","Pelagic","Sharks","Guitarfishes","Gillnet: small-mesh","Hook and line","Trawler","Depth","Slope","EKE","SST"))