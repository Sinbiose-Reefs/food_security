

# ----------------------------------------

# nutrient demand and supply: matching all data

# ----------------------------------------

# load packages
require(dplyr);require("geobr");require(gridExtra);require(here);require(ggplot2);require(ggrepel)


# load supply
load(here ("processed_data", "table_supply_state.RData"))

# load demand
load(here ("processed_data", "consumption_nutrients_SeaFood.RData"))
load(here ("processed_data", "consumption_nutrients_Other.RData"))


# load data - FAO's recommendations
FAO_threshold <- openxlsx::read.xlsx (here ("data_fisheries_nutrients", "Threshold_FAO.xlsx"))

# convert to kg and year
FAO_threshold$yearly_needs <-  (FAO_threshold$threshold_g*365)/1000

# correct order
consumption_nutrients_state <- consumption_nutrients_SeaFood[match (table_supply_state$OtherArea,
                                                                    consumption_nutrients_SeaFood$state_adj),]




# bind demand and supply data
binded_data <- bind_cols (table_supply_state, 
                          consumption_nutrients_state
            )

# per capita landing
binded_data <- binded_data %>%
  
  mutate_each (funs(./npop), ends_with("mu_1"))# %>% # landing of nutrients, per capita

  #mutate_each (funs(.*npop), ends_with("kg")) # state consumption


# organize data to plot
colnames(binded_data) <- gsub ("sum_seafood_kg", "Catch_QTD_kg", colnames(binded_data))
colnames(binded_data) <- gsub ("Omega3_kg", "Omega-3_kg", colnames(binded_data))


binded_data <- binded_data [,-which(colnames(binded_data) == "Year")]

# analyze the difference
df_nut_data <- binded_data %>%
 
  mutate (supply_higher_demand_catch = CatchAmount_kg_mu_1> Catch_QTD_kg,
          supply_higher_demand_protein = Protein_mu_1 > sum_protein_kg,
          supply_higher_demand_zinc = Zinc_mu_1> sum_zinc_kg,
          supply_higher_demand_iron = Iron_mu_1 > sum_iron_kg,
          supply_higher_demand_calcium = Calcium_mu_1 > sum_calcium_kg,
          supply_higher_demand_vitaA = Vitamin_A_mu_1 > sum_vita_kg,
          supply_higher_demand_omega3 = Omega_3_mu_1 > sum_omega3_kg) %>%
  
  mutate_if(is.logical, as.character) %>%
  
  mutate(supply_higher_demand_catch = (ifelse(supply_higher_demand_catch == "TRUE", "S>D",
                                       "S<D")),
         
         supply_higher_demand_protein = (ifelse(supply_higher_demand_protein == "TRUE", "S>D",
                                              "S<D")),
         
         supply_higher_demand_zinc = (ifelse(supply_higher_demand_zinc == "TRUE", "S>D",
                                             "S<D")),
         supply_higher_demand_iron = (ifelse(supply_higher_demand_iron == "TRUE", "S>D",
                                             "S<D")),
         supply_higher_demand_calcium = (ifelse(supply_higher_demand_calcium == "TRUE", "S>D",
                                                "S<D")),
         supply_higher_demand_vitaA = (ifelse(supply_higher_demand_vitaA == "TRUE", "S>D",
                                              "S<D")),
         supply_higher_demand_omega3 = (ifelse(supply_higher_demand_omega3 == "TRUE", "S>D",
                                               "S<D"))
         
  )
          

# managing the data of all sources
# correct state order
consumption_nutrients_state_all <- consumption_nutrients_Other[match (table_supply_state$OtherArea,
                                                                      consumption_nutrients_Other$state_adj),]


# bind demand and supply data
binded_data_all <- bind_cols (table_supply_state, 
                              consumption_nutrients_state_all
)

binded_data_all <- binded_data_all [,-which(colnames(binded_data_all) == "Year")]

# per capita landing
binded_data_all <- binded_data_all %>%
  
  mutate_each (funs(./npop), ends_with("mu_1"))# %>% # landing of nutrients, per capita

#mutate_each (funs(.*npop), ends_with("kg")) # state consumption



# organize data to plot
colnames(binded_data_all) <- gsub ("sum_other_kg", "Catch_QTD_kg", colnames(binded_data_all))
colnames(binded_data_all) <- gsub ("Omega3_kg", "Omega-3_kg", colnames(binded_data_all))


# calculate the difference of nutrient consumption through all sources relative to FAOs recommendations
binded_data_all <- binded_data_all %>% 
  mutate (sum_protein_kg_diff = FAO_threshold$yearly_needs[grep ("Protein",FAO_threshold$label)] - sum_other_protein_kg ,
          sum_calcium_kg_diff = FAO_threshold$yearly_needs[grep ("Calcium",FAO_threshold$label)] - sum_other_calcium_kg,
          sum_iron_kg_diff =  FAO_threshold$yearly_needs[grep ("Iron",FAO_threshold$label)] - sum_other_iron_kg,
          sum_zinc_kg_diff =  FAO_threshold$yearly_needs[grep ("Zinc",FAO_threshold$label)] - sum_other_zinc_kg,
          sum_vita_kg_diff =  FAO_threshold$yearly_needs[grep ("Vitamin",FAO_threshold$label)] - sum_other_vita_kg,
          sum_omega3_kg_diff =  FAO_threshold$yearly_needs[grep ("Omega",FAO_threshold$label)] - sum_other_omega3_kg,
          sum_magn_kg_diff =  FAO_threshold$yearly_needs[grep ("Magnesium",FAO_threshold$Nutrient)] - sum_other_magn_kg
          
          ) %>%
  
  mutate (sum_protein_kg_achievedFAO = ifelse (sum_protein_kg_diff <= 0, "Achieved", "Deficit"),
          sum_calcium_kg_achievedFAO =ifelse (sum_calcium_kg_diff <= 0, "Achieved", "Deficit"),
          sum_iron_kg_achievedFAO = ifelse (sum_iron_kg_diff <= 0, "Achieved", "Deficit"),
          sum_zinc_kg_achievedFAO = ifelse (sum_zinc_kg_diff <= 0, "Achieved", "Deficit"),
          sum_vita_kg_achievedFAO = ifelse (sum_vita_kg_diff <= 0, "Achieved", "Deficit"),
          sum_omega3_kg_achievedFAO = ifelse (sum_omega3_kg_diff <= 0, "Achieved", "Deficit"),
          sum_magn_kg_achievedFAO = ifelse (sum_magn_kg_diff <= 0, "Achieved", "Deficit")
          )
  
  

# plot settings
def_max.overlaps<-30
my_theme<- theme(legend.position = c(0.2,0.8),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size=8, 
                                           angle=0))

# all catch and consumption

bind_rows(df_nut_data, 
df_nut_data %>% map(~mean(.,na.rm=T)) %>% rbind.data.frame)


plot_all <- df_nut_data %>% 
  select("state_adj",contains("Catch"))  %>%
  reshape2::melt (id.vars = c("state_adj",  "supply_higher_demand_catch")) %>%
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
                  max.overlaps = def_max.overlaps) +
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral",
                       direction =1)  
  


# plot protein
require(forcats)

# bind seafood and other sources
plot_protein <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("Protein", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("protein", "poly")),-"Protein_mu_1")
  
)  %>%
  
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_protein_kg_diff = ifelse (sum_protein_kg_diff <=0,0,sum_protein_kg_diff),
          
          deficits = sum_protein_kg_diff - sum_protein_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # melt
  reshape2::melt (id.vars = c("state_adj", "sum_protein_kg_achievedFAO","supply_higher_demand_protein","sum_protein_kg_diff","sum_other_protein_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_protein_kg", "Protein_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_protein,sum_protein_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")

# bind seafood and other sources
plot_calcium <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("calcium", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("calcium", "poly")), -"Calcium_mu_1")
  
) %>%
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_calcium_kg_diff = ifelse (sum_calcium_kg_diff <=0,0,sum_calcium_kg_diff),
          
          deficits = sum_calcium_kg_diff - sum_calcium_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # organize the data
  reshape2::melt (id.vars = c("state_adj", "sum_calcium_kg_achievedFAO","supply_higher_demand_calcium","sum_calcium_kg_diff","sum_other_calcium_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_calcium_kg", "Calcium_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_calcium,sum_calcium_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")

# zinc
plot_zinc <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("zinc", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("Zinc", "poly")),-"Zinc_mu_1")
  
) %>%
  
  
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_zinc_kg_diff = ifelse (sum_zinc_kg_diff <=0,0,sum_zinc_kg_diff),
          
          deficits = sum_zinc_kg_diff - sum_zinc_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # melt the data
  reshape2::melt (id.vars = c("state_adj", "sum_zinc_kg_achievedFAO","supply_higher_demand_zinc","sum_zinc_kg_diff","sum_other_zinc_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_zinc_kg", "Zinc_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_zinc,sum_zinc_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")


# iron
plot_iron <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("iron", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("iron", "poly")), -"Iron_mu_1")
  
) %>%
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_iron_kg_diff = ifelse (sum_iron_kg_diff <=0,0,sum_iron_kg_diff),
          
          deficits = sum_iron_kg_diff - sum_iron_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # melt the data
  reshape2::melt (id.vars = c("state_adj", "sum_iron_kg_achievedFAO","supply_higher_demand_iron","sum_iron_kg_diff","sum_other_iron_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_iron_kg", "Iron_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_iron,sum_iron_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=abs(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")


# omega3
plot_omega3 <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("omega", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("omega", "poly")), -"Omega_3_mu_1")
  
) %>%
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_omega3_kg_diff = ifelse (sum_omega3_kg_diff <=0,0,sum_omega3_kg_diff),
          
          deficits = sum_omega3_kg_diff - sum_omega3_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # melt the data
  
  reshape2::melt (id.vars = c("state_adj", "sum_omega3_kg_achievedFAO","supply_higher_demand_omega3","sum_omega3_kg_diff","sum_other_omega3_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_omega3_kg", "Omega_3_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_omega3,sum_omega3_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=abs(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")


# omega3
plot_vitA <- cbind (
  df_nut_data %>%
    select("state_adj",contains(c("Vit", "poly"))),
  
  binded_data_all %>% 
    select(contains(c("Vit", "poly")),-"Vitamin_A_mu_1")
  
) %>%
  # calculate the suplementation (deficits - seafood)
  
  mutate (sum_vita_kg_diff = ifelse (sum_vita_kg_diff <=0,0,sum_vita_kg_diff),
          
          deficits = sum_vita_kg_diff - sum_vita_kg,
          
          deficits = ifelse (deficits <=0,0,deficits)) %>%
  
  # melt the data
  
  reshape2::melt (id.vars = c("state_adj", "sum_vita_kg_achievedFAO","supply_higher_demand_vitaA","sum_vita_kg_diff","sum_other_vita_kg")) %>%
  mutate (variable = fct_relevel(variable, "sum_vita_kg", "Vitamin_A_mu_1", "deficits")) %>%
  mutate (inter_fact = paste (supply_higher_demand_vitaA,sum_vita_kg_achievedFAO,sep=".")) %>%
  ggplot (aes (x= variable, 
               y=abs(value),
               group= state_adj,
               col=inter_fact,
               label = state_adj)) +
  geom_point(shape=1,size=3,stroke=2)+
  geom_line(size =1)+
  theme_bw()+
  my_theme+
  geom_text_repel(size=2,
                  max.overlaps = def_max.overlaps)+
  #scale_colour_viridis_d(begin=0.2,end=0.8)
  scale_fill_distiller(palette = "Spectral")



# arrange


pdf (here ("output", "demand_supply_deficit.pdf"),width=11,height=8)

grid.arrange(plot_all+ylab("Per capita kg/year"),
             plot_protein+theme (axis.title = element_blank()),
             plot_zinc+theme (axis.title = element_blank()),
             plot_calcium+theme (axis.title = element_blank()),
             plot_iron+theme (axis.title = element_blank()),
             plot_omega3+theme (axis.title = element_blank()),
             plot_vitA+theme (axis.title = element_blank()),
             layout_matrix = rbind (c (1,2,3,4),
                                    c(1,5,6,7)))

dev.off()





# old plots
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


