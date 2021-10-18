
##Library

library(installr)
library(rlang)
library(brms)
library(readr)
library(sdmpredictors)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(geosphere)
library(maps)
library(ggrepel)
library(mapproj)
library(raster)
library(patchwork)
library(scatterpie)
library(viridis)
library(tidyverse)
library(forcats)
library(glmmTMB)
library(sjPlot)
library(performance)
library(sjstats)
library(DHARMa)
library(emmeans)
library(brms)
library(stringr)
library(rethinking)
library(tidybayes)
library(bayestest)
library(bayesplot)
library(vegan)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(data.table)
library(patchwork)

theme_set(theme_tidybayes() + panel_border())

#read in plastics dataset


dat_mesophotic_plastics_colab_scaled <- read_csv("/home/cmacdonald/Trash/dat_mesophotic_plastics_scaled.csv")[,-1] 

dat_mesophotic_plastics_colab_not_scaled <- read_csv("/home/cmacdonald/Trash/dat_mesophotic_plastics_not_scaled.csv") [,-1]


#Subset consumer plastics
dat_mesophotic_plastics_colab_scaled_consumer_plastics <- dat_mesophotic_plastics_colab_scaled %>% 
   filter(Trash_group == "plastics") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 

#Subset fishing-related plastics
dat_mesophotic_plastics_colab_scaled_fishing <- dat_mesophotic_plastics_colab_scaled %>% 
   filter(Trash_group == "fishing") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 

#Subset non-plastics
dat_mesophotic_plastics_colab_scaled_non_plastics <- dat_mesophotic_plastics_colab_scaled %>% 
   filter(Trash_group == "other") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 

#Subset all plastics
dat_mesophotic_plastics_colab_scaled_all_plastics <- dat_mesophotic_plastics_colab_scaled %>%  
   mutate(Trash_group = fct_collapse(Trash_group, 
                                     plastics = c("plastics", "fishing"))) %>% 
   filter(Trash_group == "plastics") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 

#Subset non-fishing anthropogenic debris
dat_mesophotic_plastics_colab_scaled_nonfishing <- dat_mesophotic_plastics_colab_scaled %>% 
   mutate(Trash_group = fct_collapse(Trash_group, 
                                     nonfishing = c("plastics", "other"))) %>% 
   filter(Trash_group == "nonfishing") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 

#Group all anthropogenic debris together
dat_mesophotic_plastics_colab_scaled_total <- dat_mesophotic_plastics_colab_scaled %>%
   mutate(Trash_group = fct_collapse(Trash_group, 
                                     Total = c("fishing", "plastics", "other"))) %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) 



#Country level data summaries
t1 <- dat_mesophotic_plastics_colab_scaled_non_plastics %>% group_by(Country) %>% summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), non_plastics_count = sum(Trash_count)) %>% mutate(non_plastics_Km2 = (non_plastics_count/area_sampled_m2)*1e+6)
t2 <- dat_mesophotic_plastics_colab_scaled_all_plastics %>% group_by(Country) %>% summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), all_plastics_count = sum(Trash_count)) %>% mutate(all_plastics_Km2 = (all_plastics_count/area_sampled_m2)*1e+6)
t3 <-dat_mesophotic_plastics_colab_scaled_consumer_plastics %>% group_by(Country) %>% summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), consumer_plastics_count = sum(Trash_count)) %>% mutate(consumer_plastics_Km2 = (consumer_plastics_count/area_sampled_m2)*1e+6)
t4 <-dat_mesophotic_plastics_colab_scaled_fishing %>% group_by(Country) %>% summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), fishing_plastics_count = sum(Trash_count)) %>% mutate(fishing_plastics_Km2 = (fishing_plastics_count/area_sampled_m2)*1e+6)
t5 <-dat_mesophotic_plastics_colab_scaled_total %>% group_by(Country) %>% summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), total_debris_count = sum(Trash_count)) %>% mutate(total_debris_Km2 = (total_debris_count/area_sampled_m2)*1e+6)

plyr::join_all(list(t2, t4, t3, t1, t5), type='left') 

Debris_sum_stats <- left_join(t2, t4, by = c("Country")) %>% 
   left_join(.,t3, by = c("Country"))%>% 
   left_join(.,t1, by = c("Country"))%>% 
   left_join(.,t5, by = c("Country"))  %>% 
   select(Country, length_sampled_m, area_sampled_m2, all_plastics_count, 
          fishing_plastics_count, 
          consumer_plastics_count, 
          non_plastics_count, 
          total_debris_count, all_plastics_Km2,fishing_plastics_Km2,consumer_plastics_Km2,non_plastics_Km2,total_debris_Km2)%>% 
   mutate(all_plastics_Prop = all_plastics_count/total_debris_count,
          fishing_plastics_Prop = fishing_plastics_count/total_debris_count,
          consumer_plastics_Prop = consumer_plastics_count/total_debris_count,
          non_plastics_Prop = non_plastics_count/total_debris_count)

#create datatable in whcih to calculate total summary statistics 
Debris_sum_stat_total <- data.frame()
Debris_sum_stat_total[1,1] <- "Total_all_countries" 
Debris_sum_stat_total[1,2:13] <- Debris_sum_stats[,2:13] %>% colSums(na.rm = T)
colnames(Debris_sum_stat_total) <- colnames(Debris_sum_stats[,1:13])

#calculate total summary statistics and density of anthropogenic debris per kilometer squared
Debris_sum_stat_total <- Debris_sum_stat_total %>% 
   mutate(all_plastics_Prop = all_plastics_count/total_debris_count,
          fishing_plastics_Prop = fishing_plastics_count/total_debris_count,
          consumer_plastics_Prop = consumer_plastics_count/total_debris_count,
          non_plastics_Prop = non_plastics_count/total_debris_count,
          all_plastics_Km2 = (all_plastics_count/area_sampled_m2)*1e+6,
          fishing_plastics_Km2 = (fishing_plastics_count/area_sampled_m2)*1e+6,
          consumer_plastics_Km2 = (consumer_plastics_count/area_sampled_m2)*1e+6,
          non_plastics_Km2 = (non_plastics_count/area_sampled_m2)*1e+6,
          total_debris_Km2 = (total_debris_count/area_sampled_m2)*1e+6)

#join country-level and total summary atatistis; Calculate density of anthropogenic debris per football field area
Debris_sum_stat_out <- bind_rows(Debris_sum_stats, Debris_sum_stat_total) %>% 
   mutate(all_plastics_per_football_field = (all_plastics_count/area_sampled_m2)*7140,
          fishing_plastics_per_football_field = (fishing_plastics_count/area_sampled_m2)*7140,
          consumer_plastics_per_football_field = (consumer_plastics_count/area_sampled_m2)*7140,
          non_plastics_per_football_field = (non_plastics_count/area_sampled_m2)*7140,
          total_debris_per_football_field = (total_debris_count/area_sampled_m2)*7140)

#export CSV table of summary stats
write.csv(Debris_sum_stat_out, "plastics_summary_statistics.csv", row.names = F)

max_dens <- 524 #Maximum density of plastics per football field, in Comoros
mean_dens <- 23 #density of plastics per football field, across all observations

#create dataframes of anthropogenic debris at given densities, comparitive to football field area
dat_mean <- data.frame(x = runif(n = mean_dens, min = 0, max = 105), y = runif(mean_dens, min = 0, max = 68))
dat_max <- data.frame(x = runif(n = max_dens, min = 0, max = 105), y = runif(max_dens, min = 0, max = 68))

#plot maximum and overal trash densities
plot_overall_field_dens <- ggplot(dat_mean,aes(x,y))+
   theme(panel.background = element_rect(fill = "darkgreen"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
   xlab("")+
   ylab("Football field width (m)")+
   geom_point(alpha=1, size = .5, fill = "white", colour = "white")+
   coord_fixed(ratio = 1)#football field dimensions

plot_max_field_dens <-ggplot(dat_max,aes(x,y))+
   theme(panel.background = element_rect(fill = "darkgreen"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
 xlab("Football field length (m)")+
   ylab("Football field width (m)")+
   geom_point(alpha=1, size = .5, fill = "white", colour = "white")+
   coord_fixed(ratio = 1)#football field dimensions

plot_overall_field_dens / plot_max_field_dens


#plot base maps for sites with pie chart stats ####

world <- map_data("world2")

dat_mesophotic_plastics_summ_1km <- dat_mesophotic_plastics_colab_scaled %>% 
   group_by(Country, Meso_zone2) %>% 
   summarise(Total_debris_count = sum(Trash_count), Total_area_m2 = sum(Area_m2),
             Lat_country = mean(Lat_site ), Lon_country = mean(Lon_site)) %>% 
   group_by(Country) %>% 
   mutate(Lat_country = mean(Lat_country), Lon_country = mean(Lon_country)) %>% 
   mutate(Trash_per_km2 = Total_debris_count/Total_area_m2*1e+6) %>% 
   select(Country, Meso_zone2, Lat_country, Lon_country, Trash_per_km2) %>%
   spread(Meso_zone2, value = Trash_per_km2) %>% 
   mutate(Total = MCE + Shallow) 
dat_mesophotic_plastics_summ_1km <- dat_mesophotic_plastics_summ_1km %>% 
   filter(Total > 0)


 ggplot() +
   theme_void()+
   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="goldenrod", alpha=1) +
   coord_map(xlim= c(10,350), ylim= c(-45,45)) +
   geom_scatterpie(data=dat_mesophotic_plastics_summ_1km,aes(x=Lon_country, y=Lat_country, group=Country, r=Total*.0005), cols=c("MCE", "Shallow"),color=NA, alpha=.8)+
   geom_text_repel(data=dat_mesophotic_plastics_summ_1km,aes(x=Lon_country, y=Lat_country, label=Country)) +
   geom_point(data=dat_mesophotic_plastics_summ_1km %>% filter(Country == "Seychelles"),aes(x=Lon_country, y=Lat_country),  size = 3, shape = 21) +
 #  geom_scatterpie_legend(r = dat_mesophotic_plastics_summ_1km$Total*.0005, x = 290, y = -25, labeller=function(x) x/0.0005) +
 #  coord_fixed(ratio = 1)



Debris_type_dens_per_pop <-dat_mesophotic_plastics_colab_not_scaled %>% 
   group_by(Country) %>% 
   spread(value = Trash_count, Trash_group) %>%
group_by(Country) %>%
   summarise(fishing = sum(fishing, na.rm = T)/sum(Area_m2)*1e+6, 
             plastics = sum(plastics, na.rm = T)/sum(Area_m2)*1e+6,
             other = sum(other, na.rm = T)/sum(Area_m2)*1e+6,
             Pop_mean_mag10km = log(mean(Pop10Km+1)),
             Lat = median(Lat_site),
             Lon = median(Lon_site)) %>% 
   ungroup()%>%
   mutate(Per_pop_fishing10km = fishing/Pop_mean_mag10km, 
          Per_pop_other10km = other/Pop_mean_mag10km,
          Per_pop_plastics10km = plastics/Pop_mean_mag10km,
          Per_pop_total_density_10km =  Per_pop_fishing10km + Per_pop_other10km + Per_pop_plastics10km)

Debris_type_dens_per_pop <- Debris_type_dens_per_pop %>% 
   filter(Per_pop_total_density_10km > 0)

 ggplot() +
   theme_void()+
   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="goldenrod", alpha=1) +
    coord_map(xlim= c(0,360), ylim = c(-45,45))+
   geom_scatterpie(data=Debris_type_dens_per_pop,aes(x=Lon, y=Lat, group=Country, r=Per_pop_total_density_10km *.005), cols=c("Per_pop_fishing10km", "Per_pop_plastics10km", "Per_pop_other10km"),
                   color=NA, alpha=.8) +
    # geom_scatterpie_legend(r = Debris_type_dens_per_pop $Per_pop_total_density_10km *.005, x = 260, y = -18, labeller = function(x) x/0.005) +
    #  coord_fixed(ratio = 1)
 
 
 

 29/0.005
 
 # Modeling ####

# Gam Models Depth ####
fit_smooth_all <- brm(bf(Trash_count ~ s(Depth_m) + (1|Location/Site)+offset(log(Area_m2))),
                      family = 'zero_inflated_negbinomial', 
                      data = dat_mesophotic_plastics_colab_scaled_total,
                      prior = c(set_prior("normal(0, 1)", class = "b"), set_prior("cauchy(0, 2)", class = "sd")),
                      chains = 4, 
                      iter = 10e3,# will probably want to reduce these; chains, iter, warmup args to suit processor power
                      warmup = 1e3,
                      save_pars = save_pars(all = T), 
                      control = list(adapt_delta = 0.98),
                      backend = "cmdstanr", # These last two lines allow multi-thread parallel modelling
                      threads = threading(16) # remove these last two lines if not running parallel threads
)

fit_smooth_fishing <- update(fit_smooth_all, newdata = dat_mesophotic_plastics_colab_scaled_fishing)
fit_smooth_nonfishing <- update(fit_smooth_all, newdata = dat_mesophotic_plastics_colab_scaled_nonfishing)
fit_smooth_consumer_plastics <- update(fit_smooth_all, newdata = dat_mesophotic_plastics_colab_scaled_consumer_plastics)
fit_smooth_nonplastics <- update(fit_smooth_all, newdata = dat_mesophotic_plastics_colab_scaled_non_plastics)

#Plot smoothing terms ####
plot(conditional_smooths(fit_smooth_all, spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(fit_smooth_fishing , spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(fit_smooth_nonfishing , spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(fit_smooth_consumer_plastics, spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(fit_smooth_nonplastics, spaghetti = T, ndraws = 500), rug = T)


# Depth-zone contrasts ####

mod_debris_meso_zone_all <- brms::brm(Trash_count ~ Meso_zone3+(1|Location/Site)+offset(log(Area_m2)),
                                     zi ~ Meso_zone3, 
                                     family = 'zero_inflated_negbinomial', data = dat_mesophotic_plastics_colab_scaled_total,
                                     prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
                                               prior(beta(2, 6), class = zi),
                                               prior(normal(0, 1.5), class = b)),
                                     iter = 10e3,
                                     warmup = 2e3,# will probably want to reduce these; chains, iter, warmup args to suit processor power
                                     chains = 4,
                                     thin = 3,
                                     control = list(adapt_delta = 0.98),
                                     save_all_pars = TRUE,
                                     backend = "cmdstanr",# These last two lines allow multi-thread parallel modelling
                                     threads = threading(16))# remove these last two lines if not running parallel threads

#plot the likelihood that anthropogenic litter is equally abundant among depth zones; ie depth-zone contrasts: Shallow-upper, shallow-lower, upper-lower
plot(hypothesis(mod_debris_meso_zone_all, c("Intercept - Meso_zone3Upper = 0", "Intercept - Meso_zone3Lower = 0", "(Intercept+Meso_zone3Upper) - (Intercept+Meso_zone3Lower) = 0")))
#test the hypothesis that anthropogenic litter is less abundant on shallow reefs conpared to upper and then lower mesophotic depths
hypothesis(mod_debris_meso_zone_all, c("Intercept < (Intercept + Meso_zone3Upper)", "Intercept < (Intercept + Meso_zone3Lower)"))
# test the hypothesis that anthropogenic litter is less abundant on reefs of low complexity, compared to medium and high complexity.
hypothesis(mod_debris_meso_zone_all, c("Intercept < (Intercept + Complexity_3ptIntermediate)", "Intercept < (Intercept + Complexity_3ptHigh)"))


mod_debris_meso_zone_nonfishing <- update(mod_debris_meso_zone_all, newdata = dat_mesophotic_plastics_colab_scaled_nonfishing)
plot(hypothesis(mod_debris_meso_zone_nonfishing, c("Intercept - Meso_zone3Upper = 0", "Intercept - Meso_zone3Lower = 0", "(Intercept+Meso_zone3Upper) - (Intercept+Meso_zone3Lower) = 0")))


mod_debris_meso_zone_fishing <- update(mod_debris_meso_zone_all, newdata = dat_mesophotic_plastics_colab_scaled_fishing)
plot(hypothesis(mod_debris_meso_zoneb_fishing, c("Intercept - Meso_zone3Upper = 0", "Intercept - Meso_zone3Lower = 0", "(Intercept+Meso_zone3Upper) - (Intercept+Meso_zone3Lower) = 0")))

mod_debris_meso_zone_consumer_plastics <- update(mod_debris_meso_zone_all, newdata = dat_mesophotic_plastics_colab_scaled_consumer_plastics)
plot(hypothesis(mod_debris_meso_zone_consumer_plastics, c("Intercept - Meso_zone3Upper = 0", "Intercept - Meso_zone3Lower = 0", "(Intercept+Meso_zone3Upper) - (Intercept+Meso_zone3Lower) = 0")))

mod_debris_meso_zone_nonplastics <- update(mod_debris_meso_zone_all, newdata = dat_mesophotic_plastics_colab_scaled_non_plastics)
plot(hypothesis(mod_debris_meso_zone_nonplastics, c("Intercept - Meso_zone3Upper = 0", "Intercept - Meso_zone3Lower = 0", "(Intercept+Meso_zone3Upper) - (Intercept+Meso_zone3Lower) = 0")))

#############################
### Predictive modelling ####
#############################

# All debris ####
mod_debris_pred <- brms::brm(Trash_count ~ Complexity_3pt+Reefarea_pop100+Mismanaged_pop100Km_2010+Dist_Market+Dist_MPA+Pop10Km+Meso_zone3+(1|Location/Site)+offset(log(Area_m2)), 
                            family = 'zero_inflated_negbinomial', 
                            data =  dat_mesophotic_plastics_colab_scaled_total,
                            prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
                                      prior(beta(2, 6), class = zi),
                                      prior(normal(0, 1.5), class = b)),
                            iter = 10e3,
                            warmup = 2e3,# will probably want to reduce these; chains, iter, warmup args to suit processor power
                            chains = 4,
                            thin = 1,
                            control = list(adapt_delta = 0.99),
                            save_all_pars = TRUE,
                            backend = "cmdstanr",# These last two lines allow multi-thread parallel modelling
                            threads = threading(16))# remove these last two lines if not running parallel threads

mod_debris_pred <- add_criterion(mod_debris_pred, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

#Fishing gear only####
mod_debris_pred_fishing <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_fishing)
mod_debris_pred_fishing <- add_criterion(mod_debris_pred_fishing, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)
# hypothesis(mod_debris_pred_fishing, c("Intercept < (Intercept + Complexity_3ptintermediate)", "Intercept < (Intercept + Complexity_3pthigh)"))

### non-fishing debris ####
mod_debris_pred_nonfishing <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_nonfishing)
mod_debris_pred_nonfishing <- add_criterion(mod_debris_pred_nonfishing, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

##### Consumer plastics ####
mod_debris_pred_consumer_plastics <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_consumer_plastics)
mod_debris_pred_consumer_plastics <- add_criterion(mod_debris_pred_consumer_plastics, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

##### non-plastic debris ####
mod_debris_pred_non_plastics <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_non_plastics)
mod_debris_pred_non_plastics <- add_criterion(mod_debris_pred_consumer_plastics, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

############################
# check and plot models ####
###########################
# Set model of interest #### Select as needed ####
mod_debris_ID <- mod_debris_pred; Debris_ID <- "all debris"                       #1

# mod_debris_ID <-  mod_debris_pred_fishing; Debris_ID <- "fishing debris"           #2
# mod_debris_ID <-  mod_debris_pred_nonfishing; Debris_ID <- "nonfishing debris"        #3
# mod_debris_ID <- mod_debris_pred_consumer_plastics; Debris_ID <- "consumer plastics debris"  #4
# mod_debris_ID <- mod_debris_pred_non_plastics; Debris_ID <- "non-plastic debris"  #5

#Diagnostics
loo( mod_debris_ID)
pp_check(mod_debris_ID) + xlim(0,7)
pp_check(mod_debris_ID, type = "ecdf_overlay")
rstan::check_hmc_diagnostics(mod_debris_ID$fit)
mcmc_plot(mod_debris_ID, type = "trace")
mcmc_plot(mod_debris_ID, type = "acf")

# Plot predictor effect posteriors ####
mcmc_areas(
   mod_debris_ID, 
   pars = c("b_Meso_zone3Upper", "b_Meso_zone3Lower", "b_Complexity_3ptintermediate", "b_Complexity_3pthigh","b_Mismanaged_pop100Km_2010", "b_Reefarea_pop100","b_Pop10Km","b_Dist_Market","b_Dist_MPA"),
   # transformations = mod_debris_ID$family$linkinv,
   prob = 0.75, # 80% intervals
   prob_outer = .95, # 95%
   area_method = "scaled height",
   point_est = "median"
)+
   labs(
      title = paste("Posterior distributions", Debris_ID),
      subtitle = "with medians, 75% and 95% intervals"
   )

# Effect probabilies
hypothesis(mod_debris_ID, c("Intercept < (Intercept + Meso_zone3Upper)", 
                           "Intercept < (Intercept + Meso_zone3Lower)",
                           "Intercept < (Intercept + Complexity_3ptintermediate)", 
                           "Intercept < (Intercept + Complexity_3pthigh)",
                           "Dist_MPA < 0", 
                           "Dist_Market < 0", 
                           "Reefarea_pop100< 0", 
                           "Mismanaged_pop100Km_2010< 0", 
                           "Pop10Km < 0"))

# Plot R2 posterior ####
bayes_R2(mod_debris_ID, summary = FALSE) %>% 
   mcmc_areas(prob = .90) +
   labs(
      title = paste("R2 distributions", Debris_ID),
      subtitle = "90% intervals"
   )

posterior_predict(mod_debris_ID)
posterior_interval(mod_debris_ID)


###########################################
#### Debris Composition - multivariate ####
###########################################

#Read in anthropogenic debris data with size classes
sizes_debris_dat <- read_csv("~/Trash/Sizes_trash_dat.csv")[,-1]

#create data matrix from dataframe
dat_total_debris_mat <- sizes_debris_dat %>% 
   rename(Meso_zone3 = Mesozone3) %>% 
   mutate(SiteZoneCensus = paste(Site, Meso_zone3, Local_census))%>% 
   dplyr::select(SiteZoneCensus, everything()) %>% 
   ungroup()

#retain only sites with debris recorded
dat_total_debris_useable <- dat_total_debris_mat%>% 
   mutate(Total= rowSums(dat_total_debris_mat[,-c(1:9)])) %>% 
   filter(Total> 0) %>% 
   dplyr::select(-Total) %>% 
   glimpse()

#Remove one line of data that is a large outlyer in multi dimentional space
#This one line of data comprises of a site with only 1 large piece of non-plastic debris - it effectively compresses multidimensional space into this site and all other sites clustered on each other.
dat_total_debris_useable <- dat_total_debris_useable[-20,]

#Split data in observatoins and site/environment information
dat_total_debris_obs <- dat_total_debris_useable[,-c(1:9)] 
dat_total_debris_env <- dat_total_debris_useable[,c(1:9)] 

#Create distance matrix with bray curtis dissimialities
dis <- vegdist(dat_total_debris_obs,"bray")

#Compare among-group composition by comparing within group distance to centroids to across distance to centroids
adon <- adonis(dat_total_debris_obs~Meso_zone3*Complexity_3pt,data=dat_total_debris_env,method='bray',permutations = 9999)
adon1 <- adonis(dat_total_debris_obs~Depth_m*Complexity_3pt,data=dat_total_debris_env,method='bray',permutations = 9999)

#Compare among-group similarities in composition
#by complexity levels
anosim(dis, dat_total_debris_env$Complexity_3pt) #%>% plot()
#by depth zones
anosim(dis, dat_total_debris_env$Meso_zone3) #%>% plot()

#Plot nMDS
extract.xyz <- function(obj) {
   xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
   xyz <- cbind(xy, c(obj$grid$z))
   names(xyz) <- c("x", "y", "z")
   return(xyz)
}


col_vec = c("lightblue", "purple", "blue")


ord <- metaMDS(dat_total_debris_obs)
stressplot(ord)

fit <- envfit(ord, dat_total_debris_useable[,6:8], perm = 999)
vf<-envfit(ord, dat_total_debris_obs, perm=1000)

plot(ord)
cols <- with(dat_total_debris_env, col_vec[Meso_zone3])
points(ord, pch = 19, col = cols, cex = 2)
lvl <- with(dat_total_debris_env, Meso_zone3 %>% unique())
ordihull(ord, dat_total_debris_env$Meso_zone3, col = col_vec, draw = "polygon")
legend("topright", legend = lvl,
       bty = "n", col = col_vec, pch =19)      


data.scores = as.data.frame(scores(ord))
data.scores$Complexity = dat_total_debris_useable$Complexity_3pt
data.scores$Zones = dat_total_debris_useable$Meso_zone3
spp.scrs <- as.data.frame(scores(vf, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))

p <- ggplot()+
   geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = Zones, shape = Complexity)) +
   scale_colour_manual(values = col_vec) + 
   coord_fixed() + ## need aspect ratio of 1!
   geom_segment(data = spp.scrs,
                aes(x = 0, xend = NMDS1*2, y = 0, yend = NMDS2*2),
                arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
   geom_text(data = spp.scrs, aes(x = NMDS1*2.2, y = NMDS2*2.2, label = Species),
             size = 2,check_overlap = TRUE, colour = "black")+
   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
         legend.key = element_blank(), 
         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         legend.text = element_text(size = 9, colour = "grey30")) +
   labs(colour = "Zones", shape = "Complexity")

p

