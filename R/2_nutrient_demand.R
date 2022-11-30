

require("here")
require(dplyr)
require(ggplot2)
require(reshape)
require(reshape2)
require(tidyr)
library(sf)
library(tidyverse)
library(ggrepel)
library(scatterpie)
library (gridExtra)

# load functions
source ("R/functions.R")


# ----------------



# load data for analysis
load (here ("output",
            "fishConsumption_Income.RData"))


# 1 - proportion

foodType_ind <- dcast (data = CONSUMO_ALIMENTAR, 
       formula = unit_analysis+income_cat+region~general_type,
       value.var= "QTD",
       fun.aggregate = sum,
       na.rm=T,
       margins = "general_type",
       drop=F)



# proportion (NAs are produced because each individual only belongs to one social class)
sel_cols <- colnames(foodType_ind)[seq (which(colnames(foodType_ind) == "Beef") , 
                            which(colnames(foodType_ind) == "Seafood"),
                            1)]


# proportion
foodType_ind[,sel_cols]<-foodType_ind[,sel_cols]/foodType_ind[,"(all)"]
# 0/0 = NaN
foodType_ind[is.na(foodType_ind)] <- 0


pdf (here ("output", "barplot_consumption"),width=4,height=7)

# aggregate
foodType_ind %>%
  select (-`(all)`) %>%
  filter (is.na(Beef) !=T) %>%
  group_by(income_cat,region) %>%
  summarise(across (Beef:Seafood,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion", -income_cat,-region) %>%
  mutate(food_type = factor(food_type, levels = c("Beef", 
                                                 "Beef&other",
                                                 "Game",
                                                 "Goat",
                                                 "Pork",
                                                 "Poultry",
                                                 "Freshwater fish",
                                                 "Imported fish",
                                                  "Seafood")))%>%
  ggplot (aes (fill=food_type, 
               x=(income_cat),
               y=proportion))+
    geom_bar (position="fill", stat="identity") +
  coord_polar(theta = "y")+
  #scale_fill_viridis_d()+
  scale_fill_brewer(palette = "Spectral",direction=1)+
  facet_wrap(~region,nrow=2,ncol=2)+
  theme(axis.text.x = element_text(angle=90),
        panel.background = element_rect(fill = "white",
                                        colour = "gray",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.caption =  element_text(size=8))+
  xlab ("Income class")+
  ylab ("Average proportion in the daily diet")+
  labs (caption = "Source of data: Brazilian Institute of Geography and Statistics (POF, 2018)") +
  guides(fill=guide_legend(title="Protein type"))



dev.off()


# ----------------------------------------------------




# 2 - daily consumption
# filter the food
filter_interesting_food <- CONSUMO_ALIMENTAR %>% 
  filter (sea_food == 1  )  # seafood



# function to transform quantitites into  kg / year
fun_kg_year <- function (x) {(x/1000)*365}



# filter the days
consumption_nutrients <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  filter (position == "sea") %>% # coastal states
  group_by(state,income_cat,unit_analysis) %>% # group by interviewer
  select (state,income_cat,unit_analysis,DIA_SEMANA,N_pop_class, 
          QTD, 
          Calcium, 
          Iron, 
          Zinc, 
          `Vitamin-A`, 
          `Polyunsatured fat`,
          Magnesium) %>% # select  variables (nutrients) to test
  #mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  group_by(state,income_cat,unit_analysis) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(unit_analysis)) %>% #, # N interviewers
            #Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  #group_by(state,income_cat,unit_analysis) %>%
  #summarise( sum_nut = mean( sum_nut,na.rm=T)) %>% #, # the average across the N days
             #Ndays=sum(Ndays,na.rm=T)) %>%
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) %>% # N interviewers
  group_by(state,income_cat) %>% # further group by state and class (summarize individual consumption)
  summarise(across (ends_with("_kg"), ~mean(.x,na.rm=T)) , # per capita consumption (mean across interviewees)
            mean_N_pop = mean(mean_N_pop,na.rm=T),
            Ninterv = mean (Ninterv,na.rm=T)
            ) %>%
  complete(income_cat) %>% # keep all levels
  mutate_at(c("Calcium_kg",
              "Iron_kg",
              "Zinc_kg",
              "Vitamin-A_kg",
              "Polyunsatured fat_kg",
              "Magnesium_kg"), ~replace_na(.,0)) %>%
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo"),
         sum_consumpt = Calcium_kg +Iron_kg+Zinc_kg+ `Vitamin-A_kg`+ `Polyunsatured fat_kg`
         ) #%>%
  #mutate (kg_consumed = mean_year_cons_kg*mean_N_pop) %>%
  #mutate_each (funs(.*mean_N_pop), ends_with("kg"))  %>% # extrapolate to all people
  #mutate_each (funs(./Ninterv), ends_with("kg"))  # per capita consumption
  
  
  

# save
save (consumption_nutrients, file = here ("output", "consumption_nutrients.RData"))


# have BR map
require("geobr")
# https://ipeagit.github.io/geobr/
# help here : https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html


# load states
BR_states <- read_state()
  

# join the databases
states_consumption <- dplyr::left_join(BR_states, 
                                       consumption_nutrients, 
                                       by = c("name_state" = "state_adj"))




# map
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_text(size=6,angle=45),
                 axis.ticks=element_blank(),
                 strip.text.x = element_text(size = 8),
                 legend.position = "top", #c(0.85,0.2),
                 legend.direction = "horizontal",
                 legend.title = element_text(size=8),
                 legend.text = element_text(size=6,angle=0),
                 legend.key.size = unit(0.5, "cm"),
                 plot.subtitle = element_text(size=9,angle=0),
                 panel.background = element_rect(fill = "white",
                                                 colour = "gray",
                                                 size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                 colour = "gray80"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "gray80"))


# total consumption

total_consumption_data <- states_consumption %>% 
  # bind cols with state centroids
  mutate(lon = map_dbl(geom, ~st_centroid_within_poly(.x)[[1]]),
         lat = map_dbl(geom, ~st_centroid_within_poly(.x)[[2]])) %>%
  # geom in the last col
  relocate (geom, .after = lat)  %>% 
  # proportion
  mutate (QTD = QTD_kg) %>%
  mutate_each (funs(./sum_consumpt), ends_with("kg")) %>%
  filter (is.na(state )!=T) #%>%  # not coastal state
  #filter (income_cat == "Class E") # class E for a while
  

# plot
p1 <- ggplot(data = total_consumption_data) +
  geom_sf(aes(fill=QTD),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Total seafood consumption, Brazilian coastal states, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "Spectral", 
                       name="(kg/year)", 
                       na.value = "gray80",
                       direction=-1,
                       limits = c(min(states_consumption$QTD,na.rm=T),
                                  max(states_consumption$QTD,na.rm=T)),
                       breaks = round (seq(min(states_consumption$QTD,na.rm=T),
                                    max(states_consumption$QTD,na.rm=T),
                                    26),1)) +
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5) + 
  theme(legend.position = "top",
        legend.direction = "horizontal")  
  
p1

# data to the pie chart
data_pie <- data.frame (
    total_consumption_data %>%
      st_drop_geometry()%>% 
      select (income_cat,
              name_state,
              sum_consumpt,
              Calcium_kg,
              Iron_kg,
              Zinc_kg,
              `Vitamin-A_kg`,
              `Polyunsatured fat_kg`,
              Magnesium_kg,
              lat,
              lon))%>%
  complete(income_cat) %>%
  mutate_at(c("Calcium_kg",
              "Iron_kg",
              "Zinc_kg",
              "Vitamin.A_kg",
              "Polyunsatured.fat_kg",
              "Magnesium_kg"), ~replace_na(.,0))

# plot


p2<-ggplot() + geom_scatterpie(aes(x=lon, y=lat, 
                                   group=name_state,
                                   r = 1), 
                               data=data_pie,
                           cols=c("Calcium_kg",
                                  "Iron_kg",
                                  "Zinc_kg",
                                  "Vitamin.A_kg",
                                  "Polyunsatured.fat_kg",
                                  "Magnesium_kg"),
                           color=NA) + 
  coord_equal()+
  scale_fill_viridis_d(option="viridis") +
  facet_wrap(~income_cat,ncol=5) +
  

#geom_label_repel(data = data_pie,
                  # aes(x=lon,y=lat,label=name_state),
                   #size=2)+
  no_axis + 
  theme(panel.background =element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank())
 

p2

pdf (here ("output", "Map_nutrients"),width=10,height=9)

# arrange maps
grid.arrange(p1+theme(plot.subtitle =  element_blank()),p2,
             ncol=1,nrow=2)


dev.off()


# plots showing consumption per class
# aggregate data by personID
# obtain a boxplot per type of protein, and region
#CONSUMO_ALIMENTAR %>% 
#  filter (general_type %in% c("DD", "beef&other", "wildmeat","goat") !=T) %>%
#  filter(region != "Central") %>%
#  filter (position == "sea") %>%
#  group_by (income_cat,general_type,position,region,unit_analysis) %>%  
#  summarize (QTD_mu = mean(QTD,na.rm=T)
#             #,
#             #Calcium_mu = mean(CALCIO,na.rm=T),
#             #Iron_mu = mean(FERRO,na.rm=T),
#             #Zinc_mu = mean (ZINCO,na.rm=T),
#             #Vitamin_A_mu = mean(VITA_RAE,na.rm=T)
#             ) %>%
#  #melt(id.vars = c("income_cat", "general_type","position","region")) %>% # ,"unit_analysis"
#  ggplot (aes(x=general_type, 
#              y=log(QTD_mu),
#              fill=general_type))+
#  geom_boxplot()+
#  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
#  facet_wrap(~region+income_cat, nrow=5,ncol=5)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle=45,size=8),
#        legend.position = "none", #c(0.85,0.15))
#        legend.direction = "vertical")
#  
#
## trends per class
#
#CONSUMO_ALIMENTAR %>% 
#  filter (general_type %in% c("DD") !=T) %>%
#  group_by (general_type,position,region,unit_analysis,income_cat) %>% 
#  summarize (Zinc = (sum(ZINCO)),
#             Quantidade = sum(QTD),
#             PC_RENDA_MONET=mean(PC_RENDA_MONET)) %>%
#  
#  ggplot (aes(x=log10(PC_RENDA_MONET), 
#              y=log10(Zinc),
#              #group = income_cat,
#              fill = income_cat,
#              colour=income_cat))+
#  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
#  geom_point (aes(fill=income_cat,
#                  colour=income_cat),
#              alpha = 0.025)+
#  geom_smooth(method= "lm")+
#  facet_wrap(~general_type, ncol=5)+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle=0),
#        legend.position = c(0.92,0.15))
#
#
## trends of protein consumption with income, per region
#CONSUMO_ALIMENTAR %>% 
#  filter (general_type %in% c("DD") !=T) %>%
#  group_by (general_type,position,region,unit_analysis) %>% 
#  
#  summarize (Zinc = (sum(ZINCO)),
#             Quantidade = sum(QTD),
#             PC_RENDA_MONET=mean(PC_RENDA_MONET))%>%
#  
#  ggplot (aes(x=log10(PC_RENDA_MONET), 
#              y=log10(Zinc),
#              group=region,
#              colour=region,
#              fill=region))+
#  geom_point(aes(colour=region,fill=region),alpha=0.01)+
#  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
#  geom_smooth(method= "lm")+
#  facet_wrap(~general_type, ncol=5)+
#  theme_classic()+
#  theme(axis.text.x = element_text(angle=0))
#
#
#
#
## trend
#
#CONSUMO_ALIMENTAR %>% 
#  filter (general_type %in% c("seafood")) %>%
#  group_by (food_type,general_type,position,region,unit_analysis) %>% 
#  summarize (Zinc = (sum(ZINCO)),
#             Quantidade = sum(QTD),
#             PC_RENDA_MONET=mean(PC_RENDA_MONET))%>%
#  
#  ggplot (aes(x=log10(PC_RENDA_MONET), 
#              y=log10(Zinc),
#              group=region,
#              colour=region,
#              fill=region))+
#  geom_point(aes(colour=region,fill=region),alpha=0.05)+
#  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
#  geom_smooth(method= "glm")+
#  theme_classic()+
#  theme(axis.text.x = element_text(angle=0))
#
#
#
#
## plots per state and nutrient
#
#
## map
#
#zinc <- states_consumption  %>% 
#  filter (is.na(state )!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=ZINCO_kg),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Zinc consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="(mg/kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$ZINCO_kg),
#                                  max(states_consumption$ZINCO_kg))) +
#  
#  no_axis+
#  facet_wrap(~income_cat,scales="fixed",ncol=5)
#
#
## Calcium
#
#calcium <- states_consumption  %>% 
#  filter (is.na(state )!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=CALCIO_kg),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Calcium consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="(mg/kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$CALCIO_kg),
#                                  max(states_consumption$CALCIO_kg))) +
#  
#  no_axis+
#  facet_wrap(~income_cat,scales="fixed",ncol=5)
#
#
## iron
#
#iron <- states_consumption  %>% 
#  filter (is.na(state )!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=FERRO_kg),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Iron consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="(mg/kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$FERRO_kg),
#                                  max(states_consumption$FERRO_kg))) +
#  
#  no_axis+
#  facet_wrap(~income_cat,scales="fixed",ncol=5)
#
#
## vita
#
#vitA <- states_consumption  %>% 
#  filter (is.na(state )!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=VITA_RAE_kg ),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Vitamin-A consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name=expression(paste("(", mu, "g/kg/year)",sep="")), 
#                       direction=-1,
#                       limits = c(min(states_consumption$VITA_RAE_kg ),
#                                  max(states_consumption$VITA_RAE_kg ))) +
#  
#  no_axis+
#  facet_wrap(~income_cat,scales="fixed",ncol=5)
#
## ag poli
#
#agpoli <- states_consumption  %>% 
#  filter (is.na(state )!=T) %>% 
#  ggplot() +
#  geom_sf(aes(fill=AGPOLI_kg),
#          colour="black",#NA 
#          size=.15) +
#  labs(subtitle="Polyunsaturated fatty acids consumption, Brazilian States, 2017-2018", 
#       size=8) +
#  scale_fill_distiller(palette = "RdGy", 
#                       name="(g/kg/year)", 
#                       direction=-1,
#                       limits = c(min(states_consumption$AGPOLI_kg),
#                                  max(states_consumption$AGPOLI_kg))) +
#  no_axis+
#  facet_wrap(~income_cat,scales="fixed",ncol=5)
#
#
#
## arrange
#ncols <- 8
#grid.arrange(zinc,
#             calcium,
#             iron,
#             vitA,
#             agpoli,
#             ncol=ncols,
#             nrow=5,
#             layout_matrix = rbind (rep(1,ncols),
#                                    rep(2,ncols),
#                                    rep(3,ncols),
#                                    rep(4,ncols),
#                                    rep(5,ncols)))
#

rm(list=ls())
