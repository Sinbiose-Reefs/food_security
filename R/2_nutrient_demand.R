

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

foodType_ind <- dcast (data = CONSUMO_ALIMENTAR_MEAT, 
       formula = COD_INFOR+income_cat+region~general_type,
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
#foodType_ind[is.na(foodType_ind)] <- 0


pdf (here ("output", "barplot_consumption.pdf"),width=4,height=7)

# aggregate
foodType_ind %>%
  select (-`(all)`) %>%
  filter (is.na(Beef) !=T) %>%
  group_by(income_cat,region) %>%
  summarise(across (Beef:Seafood,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion", -income_cat,-region) %>%
  mutate(food_type = factor(food_type, levels = c("Beef", 
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


# circular plot
# https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# https://r-graph-gallery.com/299-circular-stacked-barplot.html


# aggregate
dat_circular <- foodType_ind %>%
  select (-`(all)`) %>%
  filter (is.na(Beef) !=T) %>%
  group_by(income_cat,region) %>% # mean across states and people of a region
  summarise(across (Beef:Seafood,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion", -income_cat,-region) %>%
  mutate(food_type = factor(food_type, levels = c("Beef", 
                                                  "Game",
                                                  "Goat",
                                                  "Pork",
                                                  "Poultry",
                                                  "Freshwater fish",
                                                  "Imported fish",
                                                  "Seafood")),
         proportion = proportion*100)


# library
library(tidyverse)
library(viridis)



# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 1
nObsType <- nlevels(as.factor(dat_circular$food_type))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(as.factor(dat_circular$region))*nObsType, ncol(dat_circular)) )
colnames(to_add) <- colnames(dat_circular)
to_add$region <- rep(levels(as.factor(dat_circular$region)), each=empty_bar*nObsType )
dat_circular <- rbind(dat_circular, to_add)
dat_circular <- dat_circular %>% arrange(region, income_cat)
dat_circular$id <- rep( seq(1, nrow(dat_circular)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- dat_circular %>% group_by(id, income_cat) %>% 
  summarize(tot=sum(proportion,na.rm=T))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substracted 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- dat_circular %>% 
  group_by(region) %>% 
  summarize(start=min(id,na.rm=T), end=max(id,na.rm=T) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(dat_circular) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=proportion, fill=food_type), 
           stat="identity", alpha=0.7) +
  scale_fill_brewer(palette = "Spectral",direction=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(dat_circular$id),6), 
                    y = c(0, 20, 40, 60, 80,100), 
                    label = c("0", "20", "40", "60", "80","100") , 
                    color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.size = unit(0.3, 'cm'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot-80, 
                                 label=income_cat, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2, 
            angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
 geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5,
                                  colour = region), 
               alpha=0.8, size=1.5 , inherit.aes = FALSE )  +
  scale_colour_brewer(palette = "Spectral",direction=1)+
  geom_text(data=base_data, aes(x = title, y = -18, label=region), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)



p




# ------------------------------------------------------

# only blue food

foodType_ind <- dcast (data = CONSUMO_ALIMENTAR_MEAT %>% 
                         
                         filter (bluefood == "Bluefood" &
                                   
                                 protein_type %in% c("SWfish/crustacean",
                                                     "SWfish/cephalopod/crustacean/mollusk",
                                                     "cephalopod/crustacean/mollusk") ==F  
                                   ), 
                       formula = COD_INFOR+income_cat+region~protein_type,
                       value.var= "QTD",
                       fun.aggregate = sum,
                       na.rm=T,
                       fill=0,
                       margins = "protein_type",
                       drop=F)



# proportion (NAs are produced because each individual only belongs to one social class)
sel_cols <- colnames(foodType_ind)[seq (which(colnames(foodType_ind) == "cephalopod") , 
                                        which(colnames(foodType_ind) == "SWfish"),
                                        1)]


# proportion
foodType_ind[,sel_cols]<-foodType_ind[,sel_cols]/foodType_ind[,"(all)"]
# 0/0 = NaN
#foodType_ind[is.na(foodType_ind)] <- 0



# aggregate
agg_foodType_ind <- foodType_ind %>%
  select (-`(all)`) %>%
  #filter (is.na(Beef) !=T) %>%
  group_by() %>%#
  summarise(across (cephalopod:SWfish,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion")

# save plot
pdf (here ("output", "agg_foodType_ind.pdf"),width=5,height=6)  
ggplot (data = agg_foodType_ind,
          aes (fill=food_type, 
               x=1,#x=(income_cat),
               y=proportion[order(proportion)]))+
  geom_bar (position="stack", stat="identity") +
  #coord_polar(theta = "y")+
  #scale_fill_viridis_d()+
  scale_fill_brewer(palette = "Spectral",direction=-1)+
  #facet_wrap(~region,nrow=2,ncol=2)+
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
filter_interesting_food <- CONSUMO_ALIMENTAR_MEAT %>% 
  filter (sea_food == 1  & single_PTN == 1) # seafood & only raw protein
   



# function to transform quantitites into  kg / year
fun_kg_year <- function (x) {(x/1000)*365}


(filter_interesting_food [which (filter_interesting_food$COD_INFOR == "21_2121_2_210079610_11_1_1"),])
(consumption_nutrients[which(consumption_nutrients$COD_INFOR == "21_2121_2_210079610_11_1_1"), ] )



teste <- (filter_interesting_food [which (filter_interesting_food$COD_INFOR == "21_2121_2_210079610_11_1_1"),])

fun_kg_year (sum(teste$QTD)/mean(teste$Ndays))
consumption_nutrients %>%
  filter (COD_INFOR == "21_2121_2_210079610_11_1_1")

teste2 <- (consumption_nutrients [which (consumption_nutrients$state == "Maranhão" &
                                           consumption_nutrients$income_cat == "Class E"),])
mean(teste2$QTD)


# filter the days
consumption_nutrients <-  filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_class, 
          Ndays,
          sea_food,
          QTD, 
          ENERGIA_KCAL,
          PTN,
          Calcium, 
          Iron, 
          Zinc, 
          `Vitamin-A`, 
          Omega3,
          Magnesium) %>% # select  variables (nutrients) to test
  # mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  group_by(state,income_cat,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% #, # N interview days
            # Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  # quantity proportional to the number of days
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays)) %>% 
  # transform into kg
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) %>% # N interviewers
  
  group_by(state,income_cat) %>% # further group by state and class (summarize individual consumption)
  summarise(across (ends_with("_kg"), ~mean(.x,na.rm=T)) , # per capita consumption in kg (mean across interviewees)
            mean_N_pop = mean(mean_N_pop,na.rm=T),
            Ninterv = mean (Ninterv,na.rm=T)
            ) %>%
  complete(income_cat) %>% # keep all levels
  mutate_at(c("Calcium_kg",
              "Iron_kg",
              "Zinc_kg",
              "Vitamin-A_kg",
              "Omega3_kg",
              "Magnesium_kg"), ~replace_na(.,NA)) %>% # replace_na(.,0)
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo"),
         sum_consumpt = Calcium_kg +Iron_kg+Zinc_kg+ `Vitamin-A_kg`+ Omega3_kg
         ) #%>%
  #mutate (kg_consumed = mean_year_cons_kg*mean_N_pop) %>%
  #mutate_each (funs(.*mean_N_pop), ends_with("kg"))  %>% # extrapolate to all people
  #mutate_each (funs(./Ninterv), ends_with("kg"))  # per capita consumption
  
  

# statistics per state

consumption_nutrients %>%
  group_by(state)  %>% 
  summarise(mean(QTD_kg,na.rm=T), 
            sd (QTD_kg,na.rm=T))


# save
save (consumption_nutrients, file = here ("output", "consumption_nutrients.RData"))



# -------------------------------------------------------------------------


# summaries per state


# filter the days
consumption_nutrients <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  group_by(region,state,income_cat,COD_INFOR) %>% # group by interviewer
  select (region,
          state,
          income_cat,
          COD_INFOR,
          DIA_SEMANA,
          N_pop_class, 
          Ndays,
          QTD, 
          PTN,
          Calcium, 
          Iron, 
          Zinc, 
          `Vitamin-A`, 
          Omega3,
          Magnesium) %>% # select  variables (nutrients) to test
  # mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  group_by(region,state,income_cat,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% #, # N interview days
  # Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays)) %>% 
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) %>% # N interviewers
  group_by(region,state) %>% # further group by state and class (summarize individual consumption)
  summarise(across (ends_with("_kg"), ~mean(.x,na.rm=T)),
            # per capita consumption in kg (mean across interviewees)
            mean_N_pop = mean(mean_N_pop,na.rm=T),
            Ninterv = mean (Ninterv,na.rm=T)
  ) %>%
  #complete(income_cat) %>% # keep all levels
  mutate_at(c("Calcium_kg",
              "Iron_kg",
              "Zinc_kg",
              "Vitamin-A_kg",
              "Omega3_kg",
              "Magnesium_kg"), ~replace_na(.,NA)) %>% # replace_na(.,0)
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo"),
         sum_consumpt = Calcium_kg +Iron_kg+Zinc_kg+ `Vitamin-A_kg`+ Omega3_kg
  ) %>%
  arrange (QTD_kg)
#mutate (kg_consumed = mean_year_cons_kg*mean_N_pop) %>%
#mutate_each (funs(.*mean_N_pop), ends_with("kg"))  %>% # extrapolate to all people
#mutate_each (funs(./Ninterv), ends_with("kg"))  # per capita consumption



# statistics per state

consumption_nutrients

# have BR map
require("geobr")
# https://ipeagit.github.io/geobr/
# help here : https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html


# load states
BR_states <- read_state()
  

# join the databases
states_consumption <- dplyr::left_join(BR_states %>% 
                                         mutate(name_region = recode(name_region, "Norte" = "North",
                                                                   "Sul" = "South",
                                                                   "Sudeste" = "Southeast",
                                                                   "Nordeste" ="Northeast")), 
                                       consumption_nutrients, 
                                       by = c("name_state" = "state_adj"))

# br map
map_BR <- ggplot(data = states_consumption %>%
                   group_by(name_region,state)  %>% 
                   summarise(QTD_kg=mean(QTD_kg,na.rm=T))%>%
                   filter (is.na(QTD_kg) != T)) +
  geom_sf(aes(fill=QTD_kg,
          colour=name_region),
          size=0.5) + 
  theme_classic() +
  theme (legend.position = "right",
         legend.direction = "vertical",
         legend.key.size = unit(1,"cm"),
         legend.text = element_text(size=8),
         legend.title = element_text(size=10),
         axis.text = element_blank(),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing panel outline
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing plot outline
         legend.background = element_blank(),# element_rect(fill = "transparent"),
         #legend.box.background = element_rect(fill = "transparent"),
         #legend.key = element_rect(fill = "transparent")
         )+
  scale_fill_gradient(low = "#BCCEF8", high = "#181D31") + 
  scale_colour_brewer(palette = "Spectral",direction=1)+
  guides(fill=guide_legend(title=("Seafood\nconsumption\n(kg)")))
  

map_BR

# arrange map and circular plot

map_ccplot <- grid.arrange(p,
             map_BR,
             ncol=5,nrow=8,
             layout_matrix = rbind (c(1,1,1,1,1),
                                    c(1,1,1,1,1),
                                    c(1,2,2,2,1),
                                    c(1,2,2,2,1),
                                    c(1,2,2,2,1),
                                    c(1,1,1,1,1),
                                    c(1,1,1,1,1),
                                    c(1,1,1,1,1)))

#save
ggsave(map_BR, file=here ('output',"map_consumption.pdf"), 
       width=10, height=10,bg="white")
ggsave(p, file=here ('output',"circular_plot.pdf"), 
       width=10, height=10,bg="white")


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
                 panel.grid.major = element_blank(),#element_line(size = 0.5, linetype = 'solid',
                                                 #colour = "gray80"), 
                 panel.grid.minor = element_blank() #element_line(size = 0.25, linetype = 'solid',
                                                 #colour = "gray80")
)


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
  scale_fill_distiller(palette = "Greys", 
                       name="(kg/year)", 
                       na.value = "red",
                       direction=-1,
                       limits = c(min(states_consumption$QTD,na.rm=T),
                                  max(states_consumption$QTD,na.rm=T)),
                       breaks = round (seq(min(states_consumption$QTD,na.rm=T),
                                    max(states_consumption$QTD,na.rm=T),
                                    3),1)) +
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)   
  
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
              Omega3_kg,
              Magnesium_kg,
              lat,
              lon))%>%
  complete(income_cat) %>%
  mutate_at(c("Calcium_kg",
              "Iron_kg",
              "Zinc_kg",
              "Vitamin.A_kg",
              "Omega3_kg",
              "Magnesium_kg"), ~replace_na(.,0))

# plot


p2<-ggplot() + geom_scatterpie(aes(x=lon, y=lat, 
                                   group=name_state,
                                   r = 1.5), 
                               data=data_pie,
                           cols=c("Calcium_kg",
                                  "Iron_kg",
                                  "Zinc_kg",
                                  "Vitamin.A_kg",
                                  "Omega3_kg",
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


# save
pdf (here ("output", "Map_nutrients"),width=10,height=9)

# arrange maps
grid.arrange(p1+theme(plot.subtitle =  element_blank()),p2,
             ncol=1,nrow=2)


dev.off()

# end


rm(list=ls())
