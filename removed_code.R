### Removed code while tidying for publication
##### Removed from data_preparations.R ####
#### Plotting data! To see how much survey info
# dev.off()
# pdf("Output/Neighbour_size.pdf")
# ggplot(surveydata, aes(x = Neighbour_sp_ID, y = log(Sum_nbh_DBH)))+
#   geom_boxplot()+
#   geom_jitter(alpha=0.1, colour = "forestgreen", width = 0.05, height = 0.05)+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90))
# dev.off()

# dev.off()
# pdf("Output/Neighbour_size_distance.pdf")
# ggplot(surveydata, aes(x = Neighbour_distance_m, y = log(Sum_nbh_DBH)))+
#   geom_point(aes(col = Neighbour_sp_ID), alpha=0.1)+
#   theme_classic()+
#   theme(legend.position = "bottom", legend.key.size = unit(0.2, "cm"), legend.title = NULL)
# dev.off()
# legend.key.width=unit(0.01,"cm")
####

#Canham 2004 - NCiintra = sum
#basal area/distance^2
#square - non-linear response based on distance
#linear decay if just distance
#Uriarte
#used to have: NCI_nbh = Neighbour_DBH_cm/(DBH_cm*Neighbour_distance_m)
# but we will instead include DBH_cm*NCI in the model 

#examples, 40 cm DBH focal, 20 cm DBH neighbour, 1 m away: 0.005
#or 9 m away: 0.00056

# ggplot(soildata, aes(x = `pH_Level_(CaCl2)`, y = `pH_Level_(H2O)`))+
#   geom_point(alpha=0.3)+
#  theme_classic()
# ggplot(soildata, aes(x = nitrate, y = ammonium))+
#   geom_point()+
#   theme_classic()

#Separate sample_ID into Site and Plot
#Each sample_ID is effectively a plot at the moment

# abioticpcadata <- abioticpcadata %>% unite("plotid", Site:Plot, remove = "false")
# abioticpcatrim <- abioticpcadata %>% select(-c(Site, Plot))