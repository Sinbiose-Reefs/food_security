

# ----------------------------------------

# nutrient demand and supply: matching all data


# ----------------------------------------

# load packages
require(dplyr);require("geobr");require(gridExtra);require(here);require(ggplot2);require(ggrepel)


# load supply
load(here ("output", "table_supply_state.RData"))

# load demand
load(here ("output", "consumption_nutrients.RData"))

# correct order
consumption_nutrients_state <- consumption_nutrients[match (table_supply_state$OtherArea,
                                                            consumption_nutrients$state_adj),]




# bind demand and supply data
binded_data <- bind_cols (table_supply_state, 
                          consumption_nutrients_state
            )

# per capita landing
binded_data <- binded_data %>%
  
  mutate_each (funs(./npop), ends_with("kg_1"))# %>% # landing of nutrients, per capita

  #mutate_each (funs(.*npop), ends_with("kg")) # state consumption



# organize data to plot
#colnames(binded_data) <- gsub ("FERRO", "Iron", colnames(binded_data))
#colnames(binded_data) <- gsub ("AGPOLI", "Omega_3", colnames(binded_data))
colnames(binded_data) <- gsub ("sum_seafood_kg", "Catch_QTD_kg", colnames(binded_data))
colnames(binded_data) <- gsub ("Omega3_kg", "Omega-3_kg", colnames(binded_data))

# analyze the difference
df_nut_data <- binded_data %>%
  mutate (supply_higher_demand_catch = CatchAmount_kg_1> Catch_QTD_kg,
          supply_higher_demand_zinc = Zinc_mu_kg_1> sum_zinc_kg,
          supply_higher_demand_iron = Iron_mu_kg_1 > sum_iron_kg,
          supply_higher_demand_calcium = Calcium_mu_kg_1 > sum_calcium_kg,
          supply_higher_demand_vitaA = Vitamin_A_mu_kg_1 > sum_vita_kg,
          supply_higher_demand_omega3 = Omega_3_mu_kg_1 > sum_omega3_kg) %>%
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
               label = state_adj,
               col=supply_higher_demand_catch)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral",
                       direction =1)



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

grid.arrange(plot_all+ylab("Per capita kg/year (log scale)"),
             plot_zinc+theme (axis.title = element_blank()),
             plot_calcium+theme (axis.title = element_blank()),
             plot_iron+theme (axis.title = element_blank()),
             plot_omega3+theme (axis.title = element_blank()),
             plot_vitA+theme (axis.title = element_blank()),
             layout_matrix = rbind (c (1,2,3),
                                    c(4,5,6)))

dev.off()


