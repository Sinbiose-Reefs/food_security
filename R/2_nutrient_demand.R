

# ----------------------------------------------------------- # 

#       Explore the patterns of consumption across states
#       (quantities shown in the Results)
#       Codes used to produce the components of Fig. 1,  Figure S2.1, Figure S2.3.
#       Transform quantities (g) into  kg / year

# ------------------------------------------------------------ # 

rm(list=ls())

# load packages
source("R/packages.R")

# load functions
source ("R/functions.R")

# create directory to host the results
dir.create("output")

# ----------------

# load data for analysis
load (here ("processed_data",
            "fishConsumption_Income_meat.RData"))

# select only data from coastal state
CONSUMO_ALIMENTAR_MEAT <- CONSUMO_ALIMENTAR_MEAT %>% 
  filter (position == "sea") 
  
# average consumption across interviewing days  
CONSUMO_ALIMENTAR_MEAT$QTD_N <- CONSUMO_ALIMENTAR_MEAT$QTD/CONSUMO_ALIMENTAR_MEAT$Ndays

# 1 - proportion
foodType_ind <- dcast (data = CONSUMO_ALIMENTAR_MEAT, 
       formula = COD_INFOR+income_cat+region~general_type,
       value.var= "QTD_N",
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
 
# save fig
pdf (here ("output", "barplot_consumption.pdf"),width=4,height=7)

# aggregate
foodType_ind %>%
  select (-`(all)`) %>%
  melt() %>%
  group_by(income_cat,region,variable) %>%
  summarise(proportion = mean(value, na.rm = TRUE)) %>% 
  mutate(food_type= variable,
         food_type = factor(food_type, levels = c("Beef", 
                                                  "Game",
                                                  "Goat",
                                                  "Pork",
                                                  "Poultry",
                                                  "Freshwater fish",
                                                  "Imported fish",
                                                  "Seafood"))
         ) %>%

  ggplot (aes (fill=food_type, 
               x=(income_cat),
               y=proportion))+
    geom_bar (position="stack", stat="identity") +
  #coord_polar(theta = "y")+
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


# circular plot - Figure 1 and S2.1.
# https://r-graph-gallery.com/297-circular-barplot-with-groups.html
# https://r-graph-gallery.com/299-circular-stacked-barplot.html


# aggregate
dat_circular <- foodType_ind %>%
  select (-`(all)`) %>%
  melt() %>%
  group_by(income_cat,region,variable) %>%
  summarise(proportion = mean(value, na.rm = TRUE)) %>%
  mutate(food_type= variable,
         food_type = factor(food_type, levels = c("Beef", 
                                                  "Game",
                                                  "Goat",
                                                  "Pork",
                                                  "Poultry",
                                                  "Freshwater fish",
                                                  "Imported fish",
                                                  "Seafood")),
         proportion = proportion*100
         )



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
  geom_text(data=base_data, aes(x = title, y = -18, label=region), 
            hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)



p # Figure 1 and S2.1.


# ------------------------------------------------------

# only blue food

foodType_ind <- dcast (data = CONSUMO_ALIMENTAR_MEAT %>% 
                         
                         filter (bluefood == "Bluefood" &
                                  # filter (position == "sea")
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

# aggregate
agg_foodType_ind <- foodType_ind %>%
  select (-`(all)`) %>%
  #filter (is.na(Beef) !=T) %>%
  group_by() %>%#
  summarise(across (cephalopod:SWfish,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion")

# save plot -  histogram from Figure 1.
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

# QTD	Quantidade em gramas consumida (g)
# PTN	Quantidade de proteínas em gramas consumida (g)
# AGPOLI	Quantidade de ácidos graxos poli-insaturados em gramas (g)
# CALCIO	Quantidade de cálcio em miligramas (mg)
# FERRO	Quantidade de ferro em miligramas (mg)
# ZINCO	Quantidade de zinco em miligramas (mg)
# VITA_RAE	Quantidade de vitamina A, atividade retinoica, em microgramas (mcg)


# 2 - daily consumption
# filter the food
filter_interesting_food <- CONSUMO_ALIMENTAR_MEAT 

# transform nutrients into grams
filter_interesting_food <- filter_interesting_food %>% 
  
  mutate (`Vitamin-A` = `Vitamin-A`/1e+6,
          Calcium = Calcium/1000,
          Zinc = Zinc/1000,
          Iron = Iron/1000,
          Magnesium = Magnesium/1000) # into grams for different measurement scales


# function to transform quantitites into  kg / year
fun_kg_year <- function (x) {(x/1000)*365}

# extract and transform the data
consumption_all <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  select (region,
          state,
          #income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_state, 
          Ndays,
          bluefood,
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
  group_by(state,#income_cat,
           COD_INFOR,bluefood, sea_food) %>%  # bluefood #  summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_state,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% #, # N interview days
  # Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  # quantity proportional to the number of days
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays)) %>% 
 
   # transform into kg
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) 


# averages across all interviewees, regarding kg of bluefoods and seafood
sum(consumption_all [which(consumption_all$bluefood ==  "Bluefood"), "QTD_kg"])/length(unique(consumption_all$COD_INFOR))
sum(consumption_all [which(consumption_all$sea_food ==  "1"), "QTD_kg"])/length(unique(consumption_all$COD_INFOR))
# nrow(unique(test [which(test$state == "Alagoas"),"COD_INFOR"]))


# quantities of seafood in average across all interviewees -- values shown in the RESULTS
mean(consumption_all$QTD[which(consumption_all$sea_food==1)],na.rm=T)
sd(consumption_all$QTD[which(consumption_all$sea_food==1)],na.rm=T)
range(consumption_all$QTD[which(consumption_all$sea_food==1)],na.rm=T)
length(unique(consumption_all$COD_INFOR[which(consumption_all$sea_food==1)])) # interviewees that ate seafood

# quantities of other items in average across all interviewees -- values shown in the RESULTS
mean(consumption_all$QTD[which(consumption_all$bluefood !="Bluefood")],na.rm=T)
sd(consumption_all$QTD[which(consumption_all$bluefood !="Bluefood")],na.rm=T)
range(consumption_all$QTD[which(consumption_all$bluefood !="Bluefood")],na.rm=T)
length(unique(consumption_all$COD_INFOR[which(consumption_all$bluefood !="Bluefood")]))
length(unique(consumption_all$COD_INFOR))

# calculate the averages across states (to map)
states <- unique(consumption_all$state)
df_states_kg <- lapply (states, function (i) {
  
    d1 <- consumption_all [which(consumption_all$state == i),] 
    df_test <- data.frame (nint = length (unique(d1$COD_INFOR)),
                sum_kg_seafood = sum(d1$QTD_kg[which(d1$sea_food == "1")]),
                sum_kg_other = sum(d1$QTD_kg[is.na(d1$sea_food)]),
                sum_kg_bluefood = sum(d1$QTD_kg[which(d1$bluefood == "Bluefood")]),
                sum_kg_total = sum(d1$QTD_kg))
    df_res <- data.frame (seaf_cons = df_test$sum_kg_seafood/df_test$nint,
                          other_cons =   df_test$sum_kg_other/df_test$nint,     
                          bluef_cons = df_test$sum_kg_bluefood/df_test$nint,
                          total_cons = df_test$sum_kg_total/df_test$nint)
    df_res
    
})
names(df_states_kg) <- states
df_states_kg <- do.call(rbind,df_states_kg)
df_states_kg$state <- rownames(df_states_kg)
# adjust names
df_states_kg <- df_states_kg %>% 
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo",
  ))
  

# demand only involves seafood
# extract and transform the data
consumption_nutrients <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  select (region,
          state,
          #income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_state, 
          Ndays,
          bluefood,
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
  group_by(state,#income_cat,
           COD_INFOR,bluefood, sea_food) %>%  # bluefood #  summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_state,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% #, # N interview days
  # Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  # quantity proportional to the number of days
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays)) %>% 
  # transform into kg
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) 

# summarize the data to calculate total demand
# calculate the averages across states to have per capita consumption of seafood and nutrients
states <- unique(consumption_nutrients$state)
df_states_kg_nut_SeaFood  <- lapply (states, function (i) { # and income class
                        
                          # subset the data
                          d1 <- consumption_nutrients [which(consumption_nutrients$state == i),] 
                          # calculate the sum
                          df_test <- data.frame (state = i,
                                                 npop=unique(d1$mean_N_pop),
                                                 nint = length (unique(d1$COD_INFOR)),
                                                 sum_seafood_kg = sum(d1$QTD_kg[which(d1$sea_food == "1")]),
                                                 sum_protein_kg = sum(d1$PTN_kg[which(d1$sea_food == "1")]),
                                                 sum_calcium_kg = sum(d1$Calcium_kg[which(d1$sea_food == "1")]),
                                                 sum_iron_kg = sum(d1$Iron_kg[which(d1$sea_food == "1")]),
                                                 sum_zinc_kg = sum(d1$Zinc_kg[which(d1$sea_food == "1")]),
                                                 sum_vita_kg = sum(d1$`Vitamin-A_kg`[which(d1$sea_food == "1")]),
                                                 sum_omega3_kg = sum(d1$Omega3_kg[which(d1$sea_food == "1")]),
                                                 sum_magn_kg = sum(d1$Magnesium_kg[which(d1$sea_food == "1")]))
                                                 
                                                   
                          df_test
                          
})

# melt the list
consumption_nutrients_SeaFood <- do.call(rbind , df_states_kg_nut_SeaFood)

# adjust names
consumption_nutrients_SeaFood <- consumption_nutrients_SeaFood %>% 
  mutate(state_adj = recode(state, 
                            "Rio Grande Do Sul" = "Rio Grande do Sul"
  )) %>%
  
  mutate_at(vars (sum_seafood_kg:sum_magn_kg), funs(. / nint))   # per capita amounts


# average values reported in the section of Results!

mean(consumption_nutrients_SeaFood$sum_seafood_kg)# average seafood consumption 
sd(consumption_nutrients_SeaFood$sum_seafood_kg)# sd
range(consumption_nutrients_SeaFood$sum_seafood_kg)

# state quantities
consumption_nutrients_SeaFood %>%
  arrange(sum_seafood_kg)

# save
save (consumption_nutrients_SeaFood, 
      file = here ("processed_data", "consumption_nutrients_SeaFood.RData"))


# demand for other sources beyond bluefoods
df_states_kg_nut_Other  <- lapply (states, function (i) { # and income class
  
  # subset the data
  d1 <- consumption_nutrients [which(consumption_nutrients$state == i),] 
  # calculate the sum
  df_test <- data.frame (state = i,
                         npop=unique(d1$mean_N_pop),
                         nint = length (unique(d1$COD_INFOR)),
                         sum_other_kg = sum(d1$QTD_kg[is.na(d1$sea_food)]),
                         sum_other_protein_kg = sum(d1$PTN_kg[is.na(d1$sea_food)]),
                         sum_other_calcium_kg = sum(d1$Calcium_kg[is.na(d1$sea_food)]),
                         sum_other_iron_kg = sum(d1$Iron_kg[is.na(d1$sea_food)]),
                         sum_other_zinc_kg = sum(d1$Zinc_kg[is.na(d1$sea_food)]),
                         sum_other_vita_kg = sum(d1$`Vitamin-A_kg`[is.na(d1$sea_food)]),
                         sum_other_omega3_kg = sum(d1$Omega3_kg[is.na(d1$sea_food)]),
                         sum_other_magn_kg = sum(d1$Magnesium_kg[is.na(d1$sea_food)]))
  
  
  df_test
  
})

# melt the list
consumption_nutrients_Other <- do.call(rbind , df_states_kg_nut_Other)

# adjust names
consumption_nutrients_Other <- consumption_nutrients_Other %>% 
  mutate(state_adj = recode(state, 
                            "Rio Grande Do Sul" = "Rio Grande do Sul"
  )) %>%
  
  mutate_at(vars (sum_other_kg:sum_other_magn_kg), funs(. / nint))   # per capita amounts


# averages - values reported in the RESULTS
mean(consumption_nutrients_Other$sum_other_kg)
sd(consumption_nutrients_Other$sum_other_kg)
consumption_nutrients_Other %>%
  arrange(sum_other_kg)

# save
save (consumption_nutrients_Other, file = here ("processed_data", "consumption_nutrients_Other.RData"))


# -------------------------------------------------------------------------

# statistics per state
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
                                                                   "Nordeste" ="Northeast",
                                                                   "Centro Oeste"="Central")), 
                                       df_states_kg, 
                                       by = c("name_state" = "state_adj"))

# br map
map_BR_seafood <- ggplot(data = states_consumption) +
  geom_sf(aes(fill=seaf_cons,
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
  scale_fill_gradient(low = "#B7DAF9", high = "#032442")+ 
  scale_colour_brewer(palette = "Spectral",direction=1)+
  guides(colour=guide_legend(title=("Region")))
  

map_BR_seafood # component of Fig. 1

# bluefood

# br map
map_BR_bluefood <- ggplot(data = states_consumption) +
  geom_sf(aes(fill= bluef_cons,
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
  scale_fill_gradient(low = "#B7DAF9", high = "#032442")+ 
  scale_colour_brewer(palette = "Spectral",direction=1)+
  guides(colour=guide_legend(title=("Region")))


map_BR_bluefood # fig 1 and S1.2

# arrange map and circular plot

map_ccplot <- grid.arrange(p,
             map_BR_seafood,
             ncol=5,nrow=8,
             layout_matrix = rbind (c(1,1,1,1,1),
                                    c(1,1,1,1,1),
                                    c(1,2,2,2,1),
                                    c(1,2,2,2,1),
                                    c(1,2,2,2,1),
                                    c(1,1,1,1,1),
                                    c(1,1,1,1,1),
                                    c(1,1,1,1,1)))


map_ccplot_bluefood <- grid.arrange(p,
                           map_BR_bluefood,
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
ggsave(map_BR_seafood, file=here ('output',"map_consumption_seafood.pdf"), 
       width=10, height=10,bg="white")
ggsave(map_BR_bluefood, file=here ('output',"map_consumption_bluefood.pdf"), 
       width=10, height=10,bg="white")
ggsave(p, file=here ('output',"circular_plot.pdf"), 
       width=10, height=10,bg="white")


# ------------------------------------

#         Create Figure S2.3.


# extract and transform the data
consumption_all_class <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_state, 
          Ndays,
          bluefood,
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
  group_by(state,income_cat,COD_INFOR,bluefood, sea_food) %>%  # bluefood #  summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_state,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% #, # N interview days
  # Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  # quantity proportional to the number of days
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays)) %>% 
 
   # transform into kg
  mutate (across (QTD:Magnesium,list(kg = fun_kg_year)), # yearly consumption of nutrients, in KG/year
          mean_N_pop = mean(mean_N_pop,na.rm=T), # N per pop class
          Ninterv = sum (Ninterv,na.rm=T)) 


# calculate the averages across states (to map)
states <- unique(consumption_all_class$state)
classes <- unique(consumption_all_class$income_cat)

df_states_kg <- lapply (states, function (i) 
  do.call(rbind,lapply (classes, function (k) {
  
    d1 <- consumption_all_class [which(consumption_all_class$state == i & consumption_all_class$income_cat ==  k),] 
    df_test <- data.frame (nint = length (unique(d1$COD_INFOR)),
                sum_kg_seafood = sum(d1$QTD_kg[which(d1$sea_food == "1")]),
                sum_kg_other = sum(d1$QTD_kg[is.na(d1$sea_food)]),
                sum_kg_bluefood = sum(d1$QTD_kg[which(d1$bluefood == "Bluefood")]),
                sum_kg_total = sum(d1$QTD_kg))
    df_res <- data.frame (seaf_cons = df_test$sum_kg_seafood/df_test$nint,
                          other_cons =   df_test$sum_kg_other/df_test$nint,     
                          bluef_cons = df_test$sum_kg_bluefood/df_test$nint,
                          total_cons = df_test$sum_kg_total/df_test$nint,
                          class=k,
                          state=i)
    df_res
    
})))
names(df_states_kg) <- states
df_states_kg <- do.call(rbind,df_states_kg)
# adjust names
df_states_kg <- df_states_kg %>% 
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo",
  ))
  

# join the databases
states_class_consumption <- dplyr::left_join(BR_states %>% 
                                         mutate(name_region = recode(name_region, "Norte" = "North",
                                                                   "Sul" = "South",
                                                                   "Sudeste" = "Southeast",
                                                                   "Nordeste" ="Northeast",
                                                                   "Centro Oeste"="Central")), 
                                       df_states_kg, 
                                       by = c("name_state" = "state_adj"))
# map - Create Fig. S2.3
map_state_class <- ggplot(data = states_class_consumption %>% 
                            filter (!is.na(state))
                            ) +
  geom_sf(aes(fill=seaf_cons,
          colour=name_region),
          size=0.5) + 
  facet_wrap(~class,ncol=5)+
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
  scale_fill_gradient(low = "#B7DAF9", high = "#032442")+ 
  scale_colour_brewer(palette = "Spectral",direction=1)+
  guides(colour=guide_legend(title=("Region")))
  

map_state_class # component of Fig. S3.2
ggsave(map_state_class, file=here ('output',"Map_classes.pdf"), 
       width=10, height=10,bg="white")


# end
rm(list=ls())
