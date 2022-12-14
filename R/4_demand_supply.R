
# bind to map demand and supply


# load packages
require(dplyr)
require("geobr")
require(gridExtra)
require(here)


# load supply
load(here ("output", "table_supply_state.RData"))

# load demand
load(here ("output", "consumption_nutrients.RData"))

# bind demand and supply data
binded_data <- bind_cols (table_supply_state, 
           consumption_nutrients %>%
             group_by(state_adj) %>%
             summarise(across (ends_with("kg"), ~sum(.x,na.rm=T)),
                       mean_N_pop=sum(mean_N_pop,na.rm=T))
            )

# per capita landing
binded_data <- binded_data %>%

  mutate_each (funs(./mean_N_pop), ends_with("kg_1")) # landing of nutrients, per capita


# organize data to plot
#colnames(binded_data) <- gsub ("FERRO", "Iron", colnames(binded_data))
#colnames(binded_data) <- gsub ("AGPOLI", "Omega_3", colnames(binded_data))
colnames(binded_data) <- gsub ("QTD_kg", "Catch_QTD_kg", colnames(binded_data))
colnames(binded_data) <- gsub ("Omega3_kg", "Omega-3_kg", colnames(binded_data))

# analyze the difference
df_nut_data <- binded_data %>%
  mutate (supply_higher_demand_catch = CatchAmount_kg_1> Catch_QTD_kg,
          supply_higher_demand_zinc = Zinc_mu_kg_1> Zinc_kg,
          supply_higher_demand_iron = Iron_mu_kg_1 > Iron_kg,
          supply_higher_demand_calcium = Calcium_mu_kg_1 > Calcium_kg,
          supply_higher_demand_vitaA = Vitamin_A_mu_kg_1 > `Vitamin-A_kg`,
          supply_higher_demand_omega3 = Omega_3_mu_kg_1 > `Omega-3_kg`) %>%
  mutate_if(is.logical, as.character) %>%
  mutate(supply_higher_demand_catch = recode(supply_higher_demand_catch, "TRUE" = "S>D",
                                       "FALSE" = "S<D"),
           
         supply_higher_demand_zinc = recode(supply_higher_demand_zinc, "TRUE" = "S>D",
                          "FALSE" = "S<D"),
         supply_higher_demand_iron = recode(supply_higher_demand_iron, "TRUE" = "S>D",
                                              "FALSE" = "S<D"),
         supply_higher_demand_calcium = recode(supply_higher_demand_calcium, "TRUE" = "S>D",
                                              "FALSE" = "S<D"),
         supply_higher_demand_vitaA = recode(supply_higher_demand_vitaA, "TRUE" = "S>D",
                                              "FALSE" = "S<D"),
         supply_higher_demand_omega3 = recode(supply_higher_demand_omega3, "TRUE" = "S>D",
                                              "FALSE" = "S<D")
         
  )
          


# plot settings
def_max.overlaps<-30
my_theme<- theme(legend.position = "none",
                axis.title.x = element_blank(),
                axis.text.x = element_text(size=8, 
                                           angle=0))

# all catch and consumption


plot_all <- df_nut_data %>% 
  select("state_adj",contains("Catch"))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_catch")) %>%
  ggplot (aes (x= variable, 
               y=log(value),
               group= state_adj,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2,
             col="#00BCC0")+
  geom_line(size =1,
            col="#00BCC0")+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  col="#00BCC0",
                  max.overlaps = def_max.overlaps)#+
  #scale_colour_viridis_d(begin=0.8,end=0.8)
  #scale_fill_distiller(palette = "Spectral",direction=1)



# plot zinc

plot_zinc<-df_nut_data %>% 
  select("state_adj",contains("Zinc"))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_zinc")) %>%
  ggplot (aes (x= variable, 
                   y=(value),
                   group= state_adj,
                   col=supply_higher_demand_zinc,
                   label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral",
                       direction =1)


# plot calcium

plot_calcium<-df_nut_data %>% 
  select("state_adj",contains("calci"))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_calcium")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=supply_higher_demand_calcium,
               
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")



# plot iron

plot_iron<-df_nut_data %>% 
  select("state_adj",contains("iron"))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_iron")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=supply_higher_demand_iron,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")




# plot omega

plot_omega3<-df_nut_data %>% 
  select("state_adj",contains(c("omega", "poly")))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_omega3")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=supply_higher_demand_omega3,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")



# plot vitA

plot_vitA<-df_nut_data %>% 
  select("state_adj",contains("vita"))  %>%
  reshape2::melt (id.vars = c("state_adj", "supply_higher_demand_vitaA")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=supply_higher_demand_vitaA,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  theme(legend.position = c(0.15,0.87),
        legend.background = element_rect(fill="gray80"),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=8, 
                                   angle=0)
        )+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")


# arrange


pdf (here ("output", "demand_supply.pdf"),width=10,height=6)

grid.arrange(plot_all+ylab("Per capita kg/year"),
             plot_zinc+theme (axis.title = element_blank()),
             plot_calcium+theme (axis.title = element_blank()),
             plot_iron+theme (axis.title = element_blank()),
             plot_omega3+theme (axis.title = element_blank()),
             plot_vitA+theme (axis.title = element_blank()),
             layout_matrix = rbind (c (1,2,3),
                                    c(4,5,6)))

dev.off()



# have BR map
# https://ipeagit.github.io/geobr/
# help here : https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html

# load states
#BR_states <- read_state()
#
## join the databases
#states_consumption <- dplyr::left_join(BR_states, 
#                                       binded_data, 
#                                       by = c("name_state" = "state_adj"))
#
## map
#plot_consumption <- states_consumption %>% 
#  filter (is.na(OtherArea)!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=ZINCO_kg),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Zinc consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="Consumption\n(mg/kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$ZINCO_kg),
#                                  max(states_consumption$ZINCO_kg))) +
#  theme(legend.position = "top", #c(0.85,0.2),
#        legend.direction = "horizontal",
#        legend.text = element_text(size=8,angle=340),
#        panel.background = element_rect(fill = "white",
#                                        colour = "gray",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "gray"), 
#        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                        colour = "gray"))
#
#
#
#
## map
#plot_supply <- states_consumption %>% 
#  filter (is.na(OtherArea)!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=Zinc_mu_kg_1),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Zinc supply, Brazilian States, 2000-2015", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="Zinc landings \n(kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$Zinc_mu_kg_1),
#                                  max(states_consumption$Zinc_mu_kg_1))) +
#  theme(legend.position = "top", #c(0.85,0.2),
#        legend.direction = "horizontal",
#        legend.text = element_text(size=8,angle=340),
#        panel.background = element_rect(fill = "white",
#                                        colour = "gray",
#                                        size = 0.5, linetype = "solid"),
#        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                        colour = "gray"), 
#        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                        colour = "gray"))
#
#
#
## arrange
#
#grid.arrange (plot_supply,
#              plot_consumption,
#              ncol=2)
#
#
## plot the relationship between demand and supply
#
#ggplot (states_consumption, aes (prot_landed_kg,mean_year_cons)) +
#  geom_point()+
#  geom_smooth(method = "loess")
#