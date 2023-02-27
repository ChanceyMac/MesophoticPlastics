
##Library

library("brms")
library("readr")
library("sdmpredictors")
library("plyr")
library("tidyverse")
library("geosphere")
library("maps")
library("ggrepel")
library("mapproj")
library("raster")
library("patchwork")
library("scatterpie")
library("viridis")
library("forcats")
library("emmeans")
library("stringr")
library("tidybayes")
library("bayestest")
library("bayesplot")
library("vegan")
library("magrittr")
library("purrr")
library("forcats")
library("modelr")
library("ggdist")
library("rstan")
library("RColorBrewer")
library("data.table")
library("patchwork")
library("ggforce")
library("concaveman")
library("parallel")
library("cmdstanr")
library("marginaleffects")


theme_set(theme_tidybayes())

#read in plastics dataset

dat_mesophotic_plastics_colab_scaled <- read.csv("~/Cal Academy/Trash/MesophoticPlastics/dat_trash_scaled_all_dist.csv")[,-1] %>% 
  mutate(Complexity_3pt = factor(Complexity_3pt, levels = c("low", "intermediate", "high")),
         Meso_zone3 = factor(Meso_zone3, levels = c("Shallow", "Upper", "Lower"))
         )

dat_mesophotic_plastics_colab_not_scaled <- read.csv("~/Cal Academy/Trash/MesophoticPlastics/dat_trash_NOT_scaled_all_dist.csv") [,-1]

#Subset consumer plastics
dat_mesophotic_plastics_colab_scaled_consumer_plastics <- dat_mesophotic_plastics_colab_scaled %>% 
   dplyr::filter(Trash_group == "plastics") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Subset fishing-related plastics
dat_mesophotic_plastics_colab_scaled_fishing <- dat_mesophotic_plastics_colab_scaled %>% 
   filter(Trash_group == "fishing") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Subset non-plastics
dat_mesophotic_plastics_colab_scaled_non_plastics <- dat_mesophotic_plastics_colab_scaled %>% 
   filter(Trash_group == "other") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Subset all plastics
dat_mesophotic_plastics_colab_scaled_all_plastics <- dat_mesophotic_plastics_colab_scaled %>%  
   mutate(Trash_group = forcats::fct_collapse(Trash_group, 
                                     all_plastics = c("plastics", "fishing"))) %>% 
   filter(Trash_group == "all_plastics") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Subset non-fishing anthropogenic debris
dat_mesophotic_plastics_colab_scaled_nonfishing <- dat_mesophotic_plastics_colab_scaled %>% 
   mutate(Trash_group = fct_collapse(Trash_group, 
                                     nonfishing = c("plastics", "other"))) %>% 
   filter(Trash_group == "nonfishing") %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Group all anthropogenic debris together
dat_mesophotic_plastics_colab_scaled_total <- dat_mesophotic_plastics_colab_scaled %>%
   mutate(Trash_group = fct_collapse(Trash_group, 
                                     Total = c("fishing", "plastics", "other"))) %>% 
   dplyr::group_by(across(c(-Trash_per_m2, -Trash_count))) %>% 
   dplyr::summarise(Trash_count = sum(Trash_count), Trash_per_m2 = sum(Trash_per_m2)) %>% 
  ungroup

#Country level data summaries
t1 <- dat_mesophotic_plastics_colab_scaled_non_plastics %>% group_by(Country) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), non_plastics_count = sum(Trash_count)) %>% mutate(non_plastics_Km2 = (non_plastics_count/area_sampled_m2)*1e+6, Meso_zone2 = "All_Zones") %>% dplyr::select(Country, Meso_zone2, everything())
t2 <- dat_mesophotic_plastics_colab_scaled_all_plastics %>% group_by(Country) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), all_plastics_count = sum(Trash_count)) %>% mutate(all_plastics_Km2 = (all_plastics_count/area_sampled_m2)*1e+6, Meso_zone2 = "All_Zones") %>% dplyr::select(Country, Meso_zone2, everything())
t3 <-dat_mesophotic_plastics_colab_scaled_consumer_plastics %>% group_by(Country) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), consumer_plastics_count = sum(Trash_count)) %>% mutate(consumer_plastics_Km2 = (consumer_plastics_count/area_sampled_m2)*1e+6, Meso_zone2 = "All_Zones") %>% dplyr::select(Country, Meso_zone2, everything())
t4 <-dat_mesophotic_plastics_colab_scaled_fishing %>% group_by(Country) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), fishing_plastics_count = sum(Trash_count)) %>% mutate(fishing_plastics_Km2 = (fishing_plastics_count/area_sampled_m2)*1e+6, Meso_zone2 = "All_Zones") %>% dplyr::select(Country, Meso_zone2, everything())
t5 <-dat_mesophotic_plastics_colab_scaled_total %>% group_by(Country) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), total_debris_count = sum(Trash_count)) %>% mutate(total_debris_Km2 = (total_debris_count/area_sampled_m2)*1e+6, Meso_zone2 = "All_Zones") %>% dplyr::select(Country, Meso_zone2, everything())


#Country level data summaries among zones
t1z <- dat_mesophotic_plastics_colab_scaled_non_plastics %>% group_by(Country, Meso_zone2) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), non_plastics_count = sum(Trash_count)) %>% mutate(non_plastics_Km2 = (non_plastics_count/area_sampled_m2)*1e+6)
t2z <- dat_mesophotic_plastics_colab_scaled_all_plastics %>% group_by(Country, Meso_zone2) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), all_plastics_count = sum(Trash_count)) %>% mutate(all_plastics_Km2 = (all_plastics_count/area_sampled_m2)*1e+6)
t3z <-dat_mesophotic_plastics_colab_scaled_consumer_plastics %>% group_by(Country, Meso_zone2) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), consumer_plastics_count = sum(Trash_count)) %>% mutate(consumer_plastics_Km2 = (consumer_plastics_count/area_sampled_m2)*1e+6)
t4z <-dat_mesophotic_plastics_colab_scaled_fishing %>% group_by(Country, Meso_zone2) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), fishing_plastics_count = sum(Trash_count)) %>% mutate(fishing_plastics_Km2 = (fishing_plastics_count/area_sampled_m2)*1e+6)
t5z <-dat_mesophotic_plastics_colab_scaled_total %>% group_by(Country, Meso_zone2) %>% dplyr::summarise(length_sampled_m = sum( Length_m ),area_sampled_m2 = sum(Area_m2), total_debris_count = sum(Trash_count)) %>% mutate(total_debris_Km2 = (total_debris_count/area_sampled_m2)*1e+6)

Debris_sum_stats <- left_join(t2, t4, by = c("Country")) %>% 
   left_join(.,t3, by = c("Country")) %>% 
   left_join(.,t1, by = c("Country")) %>% 
   left_join(.,t5, by = c("Country")) %>% 
   dplyr::select(Country, Meso_zone2, length_sampled_m, area_sampled_m2, all_plastics_count, 
          fishing_plastics_count, 
          consumer_plastics_count, 
          non_plastics_count, 
          total_debris_count, all_plastics_Km2,fishing_plastics_Km2,consumer_plastics_Km2,non_plastics_Km2,total_debris_Km2)%>% 
   mutate(all_plastics_Prop = all_plastics_count/total_debris_count,
          fishing_plastics_Prop = fishing_plastics_count/total_debris_count,
          consumer_plastics_Prop = consumer_plastics_count/total_debris_count,
          non_plastics_Prop = non_plastics_count/total_debris_count)

Debris_sum_stats_z <- left_join(t2z, t4z, by = c("Country", "Meso_zone2")) %>% 
   left_join(.,t3z, by = c("Country", "Meso_zone2")) %>% 
   left_join(.,t1z, by = c("Country", "Meso_zone2")) %>% 
   left_join(.,t5z, by = c("Country", "Meso_zone2"))  %>% 
   dplyr::select(Country, Meso_zone2, length_sampled_m, area_sampled_m2, all_plastics_count, 
                 fishing_plastics_count, 
                 consumer_plastics_count, 
                 non_plastics_count, 
                 total_debris_count, all_plastics_Km2,fishing_plastics_Km2,consumer_plastics_Km2,non_plastics_Km2,total_debris_Km2)%>% 
   mutate(all_plastics_Prop = all_plastics_count/total_debris_count,
          fishing_plastics_Prop = fishing_plastics_count/total_debris_count,
          consumer_plastics_Prop = consumer_plastics_count/total_debris_count,
          non_plastics_Prop = non_plastics_count/total_debris_count)

#create datatable in which to calculate total summary statistics 
Debris_sum_stat_total <- data.frame()
Debris_sum_stat_total[1,1] <- "Total_all_countries" 
Debris_sum_stat_total[1,2] <- "All_Zones" 
Debris_sum_stat_total[1,3:14] <- Debris_sum_stats[,3:14] %>% colSums(na.rm = T)
colnames(Debris_sum_stat_total) <- colnames(Debris_sum_stats[,1:14])

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

#join country-level and total summary statistics; Calculate density of anthropogenic debris per football field area
Debris_sum_stat_out <- bind_rows(Debris_sum_stats,Debris_sum_stats_z, Debris_sum_stat_total) %>% 
   mutate(all_plastics_per_football_field = (all_plastics_count/area_sampled_m2)*7140,
          fishing_plastics_per_football_field = (fishing_plastics_count/area_sampled_m2)*7140,
          consumer_plastics_per_football_field = (consumer_plastics_count/area_sampled_m2)*7140,
          non_plastics_per_football_field = (non_plastics_count/area_sampled_m2)*7140,
          total_debris_per_football_field = (total_debris_count/area_sampled_m2)*7140)
Debris_sum_stat_out
#export CSV table of summary stats
#write.csv(Debris_sum_stat_out, "plastics_summary_statistics_review.csv", row.names = F)

max_dens <- 524 #Maximum density of plastics per football field, in Comoros
mean_dens <- 23 #density of plastics per football field, across all observations

#create data frames of anthropogenic debris at given densities, comparative to football field area
dat_mean <- data.frame(x = runif(n = mean_dens, min = 0, max = 105), y = runif(mean_dens, min = 0, max = 68))
dat_max <- data.frame(x = runif(n = max_dens, min = 0, max = 105), y = runif(max_dens, min = 0, max = 68))

#plot maximum and overall trash densities
plot_overall_field_dens <- ggplot(dat_mean,aes(x,y))+
   theme(panel.background = element_rect(fill = "darkgreen"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
   xlab("")+
   ylab("Football field width (m)")+
   geom_point(alpha=1, size = .5, fill = "white", colour = "white")+
  xlim(c(0, 105))+
   coord_fixed(ratio = 1)#keep plot scaled for football field dimensions

plot_max_field_dens <-ggplot(dat_max,aes(x,y))+
   theme(panel.background = element_rect(fill = "darkgreen"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())+
 xlab("Football field length (m)")+
   ylab("Football field width (m)")+
   geom_point(alpha=1, size = .5, fill = "white", colour = "white")+
  xlim(c(0, 105))+
   coord_fixed(ratio = 1)#keep plot scaled for football field dimensions
  
    
plot_overall_field_dens / plot_max_field_dens


#plot base maps for sites with pie chart stats ####

world <- map_data("world")

dat_coord <- dat_mesophotic_plastics_colab_scaled %>% 
   group_by(Country) %>% 
   dplyr::summarise(Lat_country = mean(Lat_site ), Lon_country = mean(Lon_site)) %>% 
   dplyr::select(Country,Lat_country, Lon_country)

dat_plastics_summ_total_1km_plot <- left_join(Debris_sum_stat_out, dat_coord) %>% filter(Meso_zone2 == "All_Zones", Country != "Total_all_countries") 
dat_total_debris_Km2 <- dat_plastics_summ_total_1km_plot %>% dplyr::select(Country, total_debris_Km2)
dat_plastics_summ_zones <- left_join(Debris_sum_stat_out, dat_coord) %>% filter(Meso_zone2 != "All_Zones") %>% 
   dplyr::select(Country, Meso_zone2, total_debris_Km2, Lat_country, Lon_country) %>% 
   group_by(Country) %>% 
   spread(value = total_debris_Km2, Meso_zone2) 

dat_plastics_summ_zones_1km_plot <- left_join(dat_plastics_summ_zones, dat_total_debris_Km2)

pie_scale = 0.0006
 ggplot() +
   theme_void()+
   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="goldenrod", alpha=1) +
     # coord_map(xlim= c(10,350), ylim= c(-45,45)) +
   geom_scatterpie(data=dat_plastics_summ_zones_1km_plot,aes(x=Lon_country, y=Lat_country, group=Country, r=total_debris_Km2*pie_scale), cols=c("MCE", "Shallow"),color=NA, alpha=.8)+
   # geom_text_repel(data=dat_plastics_summ_zones_1km_plot,aes(x=Lon_country, y=Lat_country, label=Country)) +
   geom_point(data=dat_plastics_summ_zones_1km_plot %>% filter(Country == "Seychelles"),aes(x=Lon_country, y=Lat_country),  size = 3, shape = 21) +
  geom_scatterpie_legend(r = dat_plastics_summ_zones_1km_plot$total_debris_Km2*pie_scale, x = -90, y = -45, n= 4, labeller=function(x) format(round(x/pie_scale, 0), nsmall = 0)) +
  coord_fixed(ratio = 1, ylim = c(-90,45))



Debris_type_dens_per_pop <- dat_mesophotic_plastics_colab_not_scaled %>% 
   group_by(Country) %>% 
   spread(value = Trash_count, Trash_group) %>%
group_by(Country) %>%
   dplyr::summarise(fishing = sum(fishing, na.rm = T)/sum(Area_m2)*1e+6, 
             plastics = sum(plastics, na.rm = T)/sum(Area_m2)*1e+6,
             other = sum(other, na.rm = T)/sum(Area_m2)*1e+6,
             Pop_mean_mag10km = log(mean(Pop10Km)),
             Lat = median(Lat_site),
             Lon = median(Lon_site)) %>% 
   ungroup()%>%
   mutate(fishing_scaled_10km_popmag = fishing/Pop_mean_mag10km, 
          other_scaled_10km_popmag = other/Pop_mean_mag10km,
          plastics_scaled_10km_popmag = plastics/Pop_mean_mag10km,
          total_debris_scaled_10km_popmag =  fishing_scaled_10km_popmag + other_scaled_10km_popmag + plastics_scaled_10km_popmag)

Debris_type_dens_per_pop <- Debris_type_dens_per_pop %>% 
   filter(total_debris_scaled_10km_popmag > 0)

glimpse(Debris_type_dens_per_pop)

dot_scale <- 0.006

 ggplot() +
   theme_void()+
   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="goldenrod", alpha=1) +
    # coord_map(xlim= c(0,360), ylim = c(-45,45))+
   geom_scatterpie(data=Debris_type_dens_per_pop,aes(x=Lon, y=Lat, group=Country, r=total_debris_scaled_10km_popmag*dot_scale), cols=c("fishing_scaled_10km_popmag", "other_scaled_10km_popmag", "plastics_scaled_10km_popmag"),
                   color=NA, alpha=.8) +
   # geom_text_repel(data=dat_plastics_summ_zones_1km_plot,aes(x=Lon_country, y=Lat_country, label=Country)) +
    geom_point(data=dat_plastics_summ_zones_1km_plot %>% filter(Country == "Seychelles"),aes(x=Lon_country, y=Lat_country),  size = 3, shape = 21) +
   # geom_scatterpie_legend(r = Debris_type_dens_per_pop$total_debris_scaled_10km_popmag*dot_scale, x = -115, y = -50, n= 4, labeller=function(x) format(round(x/dot_scale, 0), nsmall = 0)) +
    coord_fixed(ratio = 1, ylim = c(-55,45))
 
 
 # Modeling ####

#############################
### Predictive modelling ####
#############################

# Depth by Zones ####
# All debris ####
mod_debris_pred <- brms::brm(Trash_count ~ Complexity_3pt+Reefarea_pop100+Mismanaged_plastic_pop100Km_2010_Tons+Dist_Market+Dist_MPA+Pop10Km+Meso_zone3+(1|Location/Site)+offset(log(Area_m2)), 
                            family = 'zero_inflated_negbinomial', 
                            data =  dat_mesophotic_plastics_colab_scaled_total,
                            prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
                                      prior(beta(2, 6), class = zi),
                                      prior(normal(0, 1.5), class = b)),
                            iter = 10e3,
                            warmup = 2e3,# reduce 'chains', 'iter', and 'warmup' args to suit processor power
                            chains = 4,
                            thin = 5,
                            control = list(adapt_delta = 0.98),
                            save_all_pars = TRUE,
                            cores = 4
                            # backend = "cmdstanr",# These last two lines allow multi-thread parallel modelling
                            # threads = threading(16)# remove these last two lines if not running parallel threads
)
#Note: a small number of transects in Moorea don't have complexity values, so will be dropped from the model (NA)
mod_debris_pred <- add_criterion(mod_debris_pred, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

#plot the likelihood that anthropogenic litter is equally abundant among depth zones; ie depth-zone contrasts: Shallow-upper, shallow-lower, upper-lower
plot(hypothesis(mod_debris_pred, c("Intercept < Meso_zone3Upper", "Intercept < Meso_zone3Lower", "Meso_zone3Upper < Meso_zone3Lower")))

#test the hypothesis that anthropogenic litter is less abundant on shallow reefs compared to upper and then lower mesophotic depths
plot(hypothesis(mod_debris_pred, c("(Meso_zone3Upper+Intercept) > Intercept", "(Meso_zone3Lower+Intercept) > Intercept")))


#Fishing gear only####
mod_debris_pred_fishing <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_fishing, cores = 4)
mod_debris_pred_fishing <- add_criterion(mod_debris_pred_fishing, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

#plot the likelihood that anthropogenic litter is equally abundant among depth zones; ie depth-zone contrasts: Shallow-upper, shallow-lower, upper-lower
plot(hypothesis(mod_debris_pred_fishing, c("Intercept < Meso_zone3Upper", "Intercept < Meso_zone3Lower", "Meso_zone3Upper < Meso_zone3Lower")))

#test the hypothesis that anthropogenic litter is less abundant on shallow reefs compared to upper and then lower mesophotic depths
plot(hypothesis(mod_debris_pred_fishing, c("(Meso_zone3Upper+Intercept) > Intercept", "(Meso_zone3Lower+Intercept) > Intercept")))

##### Consumer plastics ####
mod_debris_pred_consumer_plastics <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_consumer_plastics, cores = 4)
mod_debris_pred_consumer_plastics <- add_criterion(mod_debris_pred_consumer_plastics, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

#plot the likelihood that anthropogenic litter is equally abundant among depth zones; ie depth-zone contrasts: Shallow-upper, shallow-lower, upper-lower
plot(hypothesis(mod_debris_pred_consumer_plastics, c("Intercept < Intercept+Meso_zone3Upper", "Intercept < Intercept+Meso_zone3Lower", "Intercept+Meso_zone3Upper < Intercept+Meso_zone3Lower")))

##### non-plastic debris ####
mod_debris_pred_non_plastics <- update(mod_debris_pred, newdata =  dat_mesophotic_plastics_colab_scaled_non_plastics, cores = 4)
mod_debris_pred_non_plastics <- add_criterion(mod_debris_pred_non_plastics, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

#plot the likelihood that anthropogenic litter is equally abundant among depth zones; ie depth-zone contrasts: Shallow-upper, shallow-lower, upper-lower
plot(hypothesis(mod_debris_pred_non_plastics, c("Intercept < Intercept+Meso_zone3Upper", "Intercept < Intercept+Meso_zone3Lower", "Intercept+Meso_zone3Upper < Intercept+Meso_zone3Lower")))

#test the hypothesis that anthropogenic litter is less abundant on shallow reefs compared to upper and then lower mesophotic depths
plot(hypothesis(mod_debris_pred_non_plastics, c("Intercept < Intercept+Meso_zone3Upper", "Intercept < Intercept+Meso_zone3Lower", "Intercept+Meso_zone3Upper < Intercept+Meso_zone3Lower")))


# non-linear continuous depth ####

mod_debris_pred_depth_s_scaled <- brm(Trash_count ~ s(Depth_m_scaled)+Complexity_3pt+Reefarea_pop100+Mismanaged_plastic_pop100Km_2010_Tons+Dist_Market+Dist_MPA+Pop10Km+(1|Location/Site)+offset(log(Area_m2)), 
                                      family = 'zero_inflated_negbinomial', 
                                      data =  dat_mesophotic_plastics_colab_scaled_total,
                                      prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
                                                prior(beta(2, 6), class = zi),
                                                prior(normal(0, 1.5), class = b),
                                                prior("cauchy(0, 2)", class = "sds")),
                                      iter = 10e3,
                                      warmup = 2e3,# reduce 'chains', 'iter', and 'warmup' args to suit processor power
                                      chains = 4,
                                      thin = 5,
                                      control = list(adapt_delta = 0.98),
                                      save_all_pars = TRUE,
                                      cores = 4
                                      # backend = "cmdstanr",# These last two lines allow multi-thread parallel modelling
                                      # threads = threading(16)# remove these last two lines if not running parallel threads
)

mod_debris_pred_depth_s_scaled <- add_criterion(mod_debris_pred_depth_s_scaled, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

mod_debris_pred_fishing_depth_s_scaled <- update(mod_debris_pred_depth_s_scaled, 
                                                 newdata =  dat_mesophotic_plastics_colab_scaled_fishing, 
                                                 cores = 4)
mod_debris_pred_fishing_depth_s_scaled <- add_criterion(mod_debris_pred_fishing_depth_s_scaled, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)

mod_debris_pred_plastics_depth_s_scaled <- update(mod_debris_pred_depth_s_scaled, 
                                                  newdata =  dat_mesophotic_plastics_colab_scaled_consumer_plastics, 
                                                  cores = 4)
mod_debris_pred_plastics_depth_s_scaled <- add_criterion(mod_debris_pred_plastics_depth_s_scaled, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)


mod_debris_pred_non_plastics_depth_s_scaled <- update(mod_debris_pred_depth_s_scaled, 
                                                      newdata =  dat_mesophotic_plastics_colab_scaled_non_plastics, 
                                                      cores = 4)
mod_debris_pred_non_plastics_depth_s_scaled <- add_criterion(mod_debris_pred_non_plastics_depth_s_scaled, "loo", moment_match = TRUE, save_psis = TRUE, reloo = T)


# LOO ELPD model comparisons ####

loo_compare(mod_debris_pred, mod_debris_pred__depth_s_scaled)
loo_compare(mod_debris_pred_fishing, mod_debris_pred_fishing_depth_s_scaled)
loo_compare(mod_debris_pred_plastics, mod_debris_pred_plastics_depth_s_scaled)
loo_compare(mod_debris_pred_non_plastics, mod_debris_pred_non_plastics_depth_s_scaled)


############################
# check and plot models ####
###########################
# Set model of interest #### Select (or add above models) as needed ####


mod_debris_ID <- mod_debris_pred; Debris_ID <- "all debris"     
# mod_debris_ID <- mod_debris_pred_depth_s_scaled; Debris_ID <- "all debris"  #1
# mod_debris_ID <-  mod_debris_pred_fishing; Debris_ID <- "fishing debris"  
# mod_debris_ID <- mod_debris_pred_fishing_depth_s_scaled; Debris_ID <- "fishing debris"  #2
# mod_debris_ID <- mod_debris_pred_consumer_plastics; Debris_ID <- "consumer plastics debris" 
# mod_debris_ID <- mod_debris_pred_plastics_depth_s_scaled; Debris_ID <- "consumer plastics debris" #3
# mod_debris_ID <- mod_debris_pred_non_plastics; Debris_ID <- "non-plastic debris"  
# mod_debris_ID <- mod_debris_pred_non_plastics_depth_s_scaled; Debris_ID <- "non-plastic debris" #4


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
  pars = c("b_Meso_zone3Upper", "b_Meso_zone3Lower", "b_Complexity_3ptintermediate", "b_Complexity_3pthigh","b_Mismanaged_plastic_pop100Km_2010_Tons", "b_Reefarea_pop100","b_Pop10Km","b_Dist_Market","b_Dist_MPA"),
  # transformations = mod_debris_ID$family$linkinv,
  prob = 0.8, # 80% intervals
  prob_outer = .95, # 95%
  area_method = "scaled height",
  point_est = "median"
)+
  labs(
    title = paste("Posterior distributions", Debris_ID),
    subtitle = "with medians, 80% and 95% intervals"
  )

# Plot predictor effect posteriors  - Depth_S ####
mcmc_areas(
  mod_debris_ID, 
  pars = c("sds_sDepth_m_scaled_1", "b_Complexity_3ptintermediate", "b_Complexity_3pthigh","b_Mismanaged_plastic_pop100Km_2010_Tons", "b_Reefarea_pop100","b_Pop10Km","b_Dist_Market","b_Dist_MPA"),
  # transformations = mod_debris_ID$family$linkinv,
  prob = 0.80, # 80% intervals
  prob_outer = .95, # 95%
  area_method = "scaled height",
  point_est = "median"
)+
  labs(
    title = paste("Posterior distributions", Debris_ID),
    subtitle = "with medians, 80% and 95% intervals"
  )

dat_name <- mod_debris_ID %>% summary
dat_name <- dat_name$data_name
yrepnzb <- posterior_predict(mod_debris_ID)
ppc_stat(y=dat_mesophotic_plastics_colab_scaled_total_null$Trash_count, yrepnzb, stat=function(y) mean(y==0))
ppc_stat(y=dat_mesophotic_plastics_colab_scaled_total_null$Trash_count, yrepnzb, stat="max")


# Effect probabilities
hypothesis(mod_debris_ID, c("Intercept < (Intercept + Meso_zone3Upper)", 
                           "Intercept < (Intercept + Meso_zone3Lower)",
                           "Intercept < (Intercept + Complexity_3ptintermediate)", 
                           "Intercept < (Intercept + Complexity_3pthigh)",
                           "Dist_MPA < 0", 
                           "Dist_Market < 0", 
                           "Reefarea_pop100< 0", 
                           "Mismanaged_plastic_pop100Km_2010_Tons< 0", 
                           "Pop10Km > 0")) $ hypothesis %>% 
  as.data.frame 

# Plot R2 posterior ####
bayes_R2(mod_debris_ID, summary = F) %>% 
   mcmc_areas(prob = .90) +
   labs(
      title = paste("R2 distributions", Debris_ID),
      subtitle = "90% intervals"
   )



#Plot smoothing terms ####
plot(conditional_smooths(mod_debris_pred_depth_s_scaled, spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(mod_debris_pred_fishing_depth_s_scaled, spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(mod_debris_pred_plastics_depth_s_scaled, spaghetti = T, ndraws = 500), rug = T)
plot(conditional_smooths(mod_debris_pred_non_plastics_depth_s_scaled, spaghetti = T, ndraws = 500), rug = T)


####Global grand mean predictions ####

dat <- dat_mesophotic_plastics_colab_scaled_fishing
n_dat_MPA <- 
  tibble(Dist_MPA = seq_range(dat$Dist_MPA, n = 600),
         Reefarea_pop100 = rep(mean(dat$Reefarea_pop100), 600),
         Mismanaged_plastic_pop100Km_2010_Tons = rep(mean(dat$Mismanaged_plastic_pop100Km_2010_Tons), 600),
         Dist_Market = rep(mean(dat$Dist_Market), 600),
         Pop10Km = rep(mean(dat$Pop10Km), 600),
         Area_m2 = rep(mean(dat$Area_m2), 600),
         Complexity_3pt = rep("intermediate", 600),
         Meso_zone3 = rep("Upper", 600))%>%
        add_epred_draws(mod_debris_ID, ndraws = 100, re_formula = NA) 


p_epred_Dist_MPA <- n_dat_MPA%>% 
  ggplot(aes((x = Dist_MPA))) +
  geom_point(data = dat, aes(y = Trash_count/Area_m2), alpha = 0.6) +
  stat_lineribbon(aes(y = .epred),  .width = 0.9, alpha = 0.1, fill = 'grey') +
  stat_lineribbon(aes(y = .epred),  .width = 0.8, alpha = 0.2, fill = 'orange') +
  stat_lineribbon(aes(y = .epred),  .width = 0.7, alpha = .1, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.6, alpha = 0.1, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.5, alpha = .2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.4, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.3, alpha = .2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.2, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.1, alpha = .2, fill = 'darkblue') +
  labs(x = "Standaridised distance from nearest MPA",
       y = "Total debris density .1m^2") +
  theme_classic()
  
n_dat_Pop10Km <- 
  tibble(Dist_MPA = rep(mean(dat$Dist_MPA), n = 600),
         Reefarea_pop100 = rep(mean(dat$Reefarea_pop100), 600),
         Mismanaged_plastic_pop100Km_2010_Tons = rep(mean(dat$Mismanaged_plastic_pop100Km_2010_Tons), 600),
         Dist_Market = rep(mean(dat$Dist_Market), 600),
         Pop10Km = seq_range(dat$Pop10Km, 600),
         Area_m2 = rep(mean(dat$Area_m2), 600),
         Complexity_3pt = rep("intermediate", 600),
         Meso_zone3 = rep("Upper", 600))%>%
  add_epred_draws(mod_debris_ID, ndraws = 1000, re_formula = NA) 

p_epred_Pop10Km <- n_dat_Pop10Km%>% 
  ggplot(aes((x = Pop10Km))) +
  geom_point(data = dat, aes(y = Trash_count/Area_m2), alpha = 0.6, position =  position_dodge(width = 2)) +
  # stat_lineribbon(aes(y = .epred),  .width = 0.9, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.8, alpha = 0.3, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.65, alpha = .3, fill = 'darkblue') +
  coord_cartesian(ylim = c(0,1)) +
  labs(x = "Standardized population within 10Km",
       y = "Total debris density .1m^2") +
  theme_classic()


n_dat_Dist_Market <- 
  tibble(Dist_MPA = rep(mean(dat$Dist_MPA), n = 600),
         Reefarea_pop100 = rep(mean(dat$Reefarea_pop100), 600),
         Mismanaged_plastic_pop100Km_2010_Tons = rep(mean(dat$Mismanaged_plastic_pop100Km_2010_Tons), 600),
         Pop10Km = rep(mean(dat$Pop10Km), 600),
         Dist_Market = seq_range(dat$Dist_Market, 600),
         Area_m2 = rep(mean(dat$Area_m2), 600),
         Complexity_3pt = rep("intermediate", 600),
         Meso_zone3 = rep("Upper", 600))%>%
     add_epred_draws(mod_debris_ID, ndraws = 1000, re_formula = NA) 


p_epred_Dist_Market <- n_dat_Dist_Market%>% 
  ggplot(aes((x = Dist_Market))) +
  geom_point(data = dat, aes(y = Trash_count/Area_m2), alpha = 0.6) +
  stat_lineribbon(aes(y = .epred),  .width = 0.9, alpha = 0.1, fill = 'grey') +
  stat_lineribbon(aes(y = .epred),  .width = 0.8, alpha = 0.2, fill = 'orange') +
  stat_lineribbon(aes(y = .epred),  .width = 0.7, alpha = .1, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.6, alpha = 0.1, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.5, alpha = .2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.4, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.3, alpha = .2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.2, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.1, alpha = .2, fill = 'darkblue') +
  labs(x = "Scaled distance to nearest fish market",
       y = "Total debris density .1m^2") +
  theme_classic()



n_dat_reef_area <- 
  tibble(Dist_MPA = rep(mean(dat$Dist_MPA), n = 600),
         Reefarea_pop100 = seq_range(dat$Reefarea_pop100, 600),
         Mismanaged_plastic_pop100Km_2010_Tons = rep(mean(dat$Mismanaged_plastic_pop100Km_2010_Tons), 600),
         Pop10Km = rep(mean(dat$Pop10Km), 600),
         Dist_Market = rep(mean(dat$Dist_Market), 600),
         Area_m2 = rep(mean(dat$Area_m2), 600),
         Complexity_3pt = rep("intermediate", 600),
         Meso_zone3 = rep("Upper", 600))%>%
   add_epred_draws(mod_debris_ID, ndraws = 1000, re_formula = NA) 


p_epred_reef_area <- n_dat_reef_area %>% 
  ggplot(aes(x = Reefarea_pop100)) +
  geom_point(data = dat, aes(y = Trash_count/Area_m2), alpha = 0.6) +
  stat_lineribbon(aes(y = .epred),  .width = 0.9, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.8, alpha = 0.3, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.65, alpha = .3, fill = 'darkblue') +
   labs(x = "Scaled reef area per population (100km radius)",
       y = "Total debris density .1m^2") +
  theme_classic()



n_dat_mismanaged_waste <- 
  tibble(Dist_MPA = rep(mean(dat$Dist_MPA), n = 600),
         Reefarea_pop100 = rep(mean(dat$Reefarea_pop100), 600),
         Mismanaged_plastic_pop100Km_2010_Tons = seq_range(dat$Mismanaged_plastic_pop100Km_2010_Tons, 600),
         Pop10Km = rep(mean(dat$Pop10Km), 600),
         Dist_Market = rep(mean(dat$Dist_Market), 600),
         Area_m2 = rep(mean(dat$Area_m2), 600),
         Complexity_3pt = rep("intermediate", 600),
         Meso_zone3 = rep("Upper", 600))%>%
   add_epred_draws(mod_debris_ID, ndraws = 1000, re_formula = NA) 


p_epred_mismanaged_waste <- n_dat_mismanaged_waste %>% 
  ggplot(aes((x = Mismanaged_plastic_pop100Km_2010_Tons))) +
  geom_point(data = dat, aes(y = Trash_count/Area_m2), alpha = 0.6) +
  stat_lineribbon(aes(y = .epred),  .width = 0.9, alpha = 0.2, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.8, alpha = 0.3, fill = 'darkblue') +
  stat_lineribbon(aes(y = .epred),  .width = 0.65, alpha = .3, fill = 'darkblue') +
  coord_cartesian(ylim = c(0,1)) +
   labs(x = "Scaled mismanaged plastics per pop in 2010 (100km radius)",
       y = "Total debris density .1m^2") +
  theme_classic()

#plot
p_epred_Dist_MPA 
p_epred_Dist_Market 
p_epred_reef_area 
p_epred_Pop10Km 
p_epred_mismanaged_waste


########################################
#### Debris Composition - multivariate ####
###########################################

#Read in anthropogenic debris data with size classes
sizes_debris_dat <- read_csv("~/Cal Academy/Trash/Sizes_trash_dat.csv")[,-1]
sizes_debris_dat$Complexity_3pt
#create data matrix from dataframe
dat_total_debris_mat <- sizes_debris_dat %>% 
   rename(Meso_zone3 = Mesozone3) %>% 
   mutate(SiteZoneCensus = paste(Site, Meso_zone3, Local_census))%>% 
   dplyr::select(SiteZoneCensus, everything()) %>% 
   ungroup()

#retain only sites with debris recorded
dat_total_debris_useable <- dat_total_debris_mat%>% 
   mutate(Total= rowSums(dat_total_debris_mat[,-c(1:8)])) %>% 
   filter(Total> 0) %>% 
   dplyr::select(-Total) %>% 
   glimpse()

#Remove two lines of data that is a large outlyer in multi dimentional space
#These two lines of data comprise of two sites each with only 1 large piece of non-plastic debris - they effectively compresses multidimensional space into this site and all other sites clustered on each other.
dat_total_debris_useable <- dat_total_debris_useable[-c(20,86),]

#Split data in observatoins and site/environment information
dat_total_debris_obs <- dat_total_debris_useable[,-c(1:8)] 
dat_total_debris_env <- dat_total_debris_useable[,c(1:8)] %>% 
   mutate(Meso_zone3 = factor(Meso_zone3, levels = c("Shallow", "Upper", "Lower")),
          Complexity_3pt = factor(Complexity_3pt, levels = c("low", "intermediate", "high")))

#Create distance matrix with bray curtis dissimialities
dis <- vegdist(dat_total_debris_obs,"bray")

anova(betadisper(dis, dat_total_debris_env$Meso_zone3))
anova(betadisper(dis, dat_total_debris_env$Complexity_3pt))
anova(betadisper(dis, dat_total_debris_env$Depth_m))

permutest(betadisper(dis, dat_total_debris_env$Meso_zone3), pairwise = TRUE)
permutest(betadisper(dis, dat_total_debris_env$Complexity_3pt), pairwise = TRUE)
permutest(betadisper(dis, dat_total_debris_env$Depth_m))

#Compare among-group composition by comparing within group distance to centroids to across distance to centroids
adon <- adonis2(dat_total_debris_obs~Meso_zone3*Complexity_3pt,data=dat_total_debris_env,method='bray',permutations = 9999)

#Compare among-group similarities in composition
#by complexity levels
anosim(dis, dat_total_debris_env$Complexity_3pt) 
#by depth zones
anosim(dis, dat_total_debris_env$Meso_zone3) 

#Plot nMDS
extract.xyz <- function(obj) {
   xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
   xyz <- cbind(xy, c(obj$grid$z))
   names(xyz) <- c("x", "y", "z")
   return(xyz)
}


col_vec = c("blue","lightblue",  "purple")
col_vec2 = c("goldenrod", "darkorange", "darkgoldenrod4")

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

mds1 <- ggplot()+
   scale_colour_manual(values = col_vec) + 
   scale_fill_manual(values = col_vec) + 
   coord_fixed() + # need aspect ratio of 1
   geom_mark_hull(data = data.scores,aes(x = sites.NMDS1, y = sites.NMDS2,fill = Zones, colour = Zones), concavity = 10, expand = unit(1, "mm"))+
   geom_point(data = data.scores, aes(x = sites.NMDS1, y = sites.NMDS2, colour = Zones, shape = Complexity)) +
   geom_segment(data = spp.scrs,
                aes(x = 0, xend = NMDS1*2.4, y = 0, yend = NMDS2*2.4),
                arrow = arrow(length = unit(0.25, "cm")), colour = "white") +
   geom_text_repel(data = spp.scrs, aes(x = NMDS1*2.7, y = NMDS2*2.7, label = Species),
             size = 3, colour = "black")+
   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
         legend.key = element_blank(), 
         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         legend.text = element_text(size = 9, colour = "grey30")) +
   labs(colour = "Zones", shape = "Complexity")


mds2 <- ggplot()+
   scale_colour_manual(values = col_vec2) + 
   scale_fill_manual(values = col_vec2) + 
   coord_fixed() + # need aspect ratio of 1
   geom_mark_hull(data = data.scores,aes(x = sites.NMDS1, y = sites.NMDS2,fill = Complexity, colour = Complexity), concavity = 10, expand = unit(1, "mm"))+
   geom_point(data = data.scores, aes(x = sites.NMDS1, y = sites.NMDS2, colour = Complexity, shape = Zones)) +
   geom_segment(data = spp.scrs,
                aes(x = 0, xend = NMDS1*2.4, y = 0, yend = NMDS2*2.4),
                arrow = arrow(length = unit(0.25, "cm")), colour = "white") +
   geom_text_repel(data = spp.scrs, aes(x = NMDS1*2.7, y = NMDS2*2.7, label = Species),
                   size = 3,check_overlap = F, colour = "black")+
   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
         legend.key = element_blank(), 
         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
         legend.text = element_text(size = 9, colour = "grey30")) +
   labs(colour = "Complexity", shape = "Zones")

mds1/mds2


