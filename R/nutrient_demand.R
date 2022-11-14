

require("here")
require(dplyr)
require(ggplot2)
require(reshape)
require(reshape2)
require(tidyr)


# load data for analysis
load (here ("output",
            "fishConsumption_Income.RData"))

# change food type levels



# 1 - proportion

foodType_ind <- dcast (data = CONSUMO_ALIMENTAR %>% 
                         filter (position == "sea" & general_type != "DD"), 
       formula = unit_analysis+income_cat+region~general_type,
       value.var= "QTD",
       fun.aggregate = sum,
       na.rm=T,
       margins = "general_type",
       drop=F)



# proportion (NAs are produced because each individual only belongs to one social class)
sel_cols <- colnames(foodType_ind)[seq (which(colnames(foodType_ind) == "beef") , 
                            which(colnames(foodType_ind) == "wildmeat"),
                            1)]


# proportion
foodType_ind[,sel_cols]<-foodType_ind[,sel_cols]/foodType_ind[,"(all)"]
# 0/0 = NaN
foodType_ind[is.na(foodType_ind)] <- 0

# aggregate
foodType_ind %>%
  select (-`(all)`) %>%
  filter (is.na(beef) !=T) %>%
  group_by(income_cat,region) %>%
  summarise(across (beef:wildmeat,~ mean(.x, na.rm = TRUE))) %>% 
  gather ("food_type", "proportion", -income_cat,-region) %>%
  ggplot (aes (fill=food_type, 
               x=(income_cat),
               y=proportion))+
    geom_bar (position="fill", stat="identity") +
  #scale_fill_viridis_d()+
  scale_fill_brewer(palette = "Spectral")+
  facet_wrap(~region,nrow=5,ncol=1)+
  theme(axis.text.x = element_text(angle=90),
        panel.background = element_rect(fill = "white",
                                        colour = "gray",
                                        size = 0.5, 
                                        linetype = "solid")) +
  xlab ("Income class")+
  ylab ("Average proportion in the daily diet")+
  labs (caption = "Source of data: Brazilian Institute of Geography and Statistics")






# ----------------------------------------------------




# 2 - daily comsumption
# filter the food
filter_interesting_food <- CONSUMO_ALIMENTAR %>% 
  filter (sea_food == 1  ) 

# function transform in kg year
fun_kg_year <- function (x) {(x/1000)*365}

# filter the days
consumption_nutrients <- filter_interesting_food %>%
  arrange(state)%>% # order
  filter (position == "sea") %>% # coastal states
  group_by(state,income_cat,unit_analysis) %>% # group by interviewer
  select (state,income_cat,unit_analysis,DIA_SEMANA,N_pop_class, QTD, CALCIO, FERRO, ZINCO, VITA_RAE, AGPOLI) %>% # select  variables to test
  #mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interview days
  group_by(state,income_cat,unit_analysis) %>%  # solve duplicate data (measuremnet by people)
  summarise(across (QTD:AGPOLI, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class)) %>% #, # N per pop class
            #Ndays=sum(Ndays,na.rm=T)) %>% # finally group by interviewer
  #group_by(state,income_cat,unit_analysis) %>%
  #summarise( sum_nut = mean( sum_nut,na.rm=T)) %>% #, # the average across the N days
             #Ndays=sum(Ndays,na.rm=T)) %>%
  mutate (across (QTD:AGPOLI,list(kg = fun_kg_year)), # yearly consumption of nutrient, KG
          mean_N_pop = mean(mean_N_pop)) %>% 
  group_by(state,income_cat) %>% # further group by state and class (summarize individual consumption)
  summarise(across (ends_with("_kg"), ~mean(.x,na.rm=T)) , # mean or sum?
            mean_N_pop = mean(mean_N_pop)) %>%
  mutate(state_adj = recode(state, "Mato Grosso do Sul" = "Mato Grosso Do Sul",
                            "Rio de Janeiro" = "Rio De Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte",
                            "Espírito Santo" ="Espirito Santo")) %>%
  #mutate (kg_consumed = mean_year_cons_kg*mean_N_pop) %>%
  mutate_each(funs(.*mean_N_pop), ends_with("kg"))  # extrapolate to all people
  



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
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 strip.text.x = element_text(size = 8),
                 legend.position = "right", #c(0.85,0.2),
                 legend.direction = "vertical",
                 legend.title = element_text(size=8),
                 legend.text = element_text(size=6,angle=0),
                 legend.key.size = unit(0.25, "cm"),
                 plot.subtitle = element_text(size=9,angle=0),
                 panel.background = element_rect(fill = "white",
                                                 colour = "gray",
                                                 size = 0.5, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                 colour = "gray80"), 
                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                 colour = "gray80"))


# map

zinc <- states_consumption  %>% 
  filter (is.na(state )!=T) %>% 
  ggplot() +
  geom_sf(aes(fill=ZINCO_kg),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Zinc consumption, Brazilian States, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "RdGy", 
                       name="(mg/kg/year)", 
                       direction=-1,
                       limits = c(min(states_consumption$ZINCO_kg),
                                  max(states_consumption$ZINCO_kg))) +
  
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)


# Calcium

calcium <- states_consumption  %>% 
  filter (is.na(state )!=T) %>% 
  ggplot() +
  geom_sf(aes(fill=CALCIO_kg),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Calcium consumption, Brazilian States, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "RdGy", 
                       name="(mg/kg/year)", 
                       direction=-1,
                       limits = c(min(states_consumption$CALCIO_kg),
                                  max(states_consumption$CALCIO_kg))) +
  
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)


# iron

iron <- states_consumption  %>% 
  filter (is.na(state )!=T) %>% 
  ggplot() +
  geom_sf(aes(fill=FERRO_kg),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Iron consumption, Brazilian States, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "RdGy", 
                       name="(mg/kg/year)", 
                       direction=-1,
                       limits = c(min(states_consumption$FERRO_kg),
                                  max(states_consumption$FERRO_kg))) +
  
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)


# vita

vitA <- states_consumption  %>% 
  filter (is.na(state )!=T) %>% 
  ggplot() +
  geom_sf(aes(fill=VITA_RAE_kg ),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Vitamin-A consumption, Brazilian States, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "RdGy", 
                       name=expression(paste("(", mu, "g/kg/year)",sep="")), 
                       direction=-1,
                       limits = c(min(states_consumption$VITA_RAE_kg ),
                                  max(states_consumption$VITA_RAE_kg ))) +
  
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)

# ag poli

agpoli <- states_consumption  %>% 
  filter (is.na(state )!=T) %>% 
  ggplot() +
  geom_sf(aes(fill=AGPOLI_kg),
          colour="black",#NA 
          size=.15) +
  labs(subtitle="Polyunsaturated fatty acids consumption, Brazilian States, 2017-2018", 
       size=8) +
  scale_fill_distiller(palette = "RdGy", 
                       name="(g/kg/year)", 
                       direction=-1,
                       limits = c(min(states_consumption$AGPOLI_kg),
                                  max(states_consumption$AGPOLI_kg))) +
  no_axis+
  facet_wrap(~income_cat,scales="fixed",ncol=5)



# arrange
ncols <- 8
grid.arrange(zinc,
             calcium,
             iron,
             vitA,
             agpoli,
             ncol=ncols,
             nrow=5,
             layout_matrix = rbind (rep(1,ncols),
                                    rep(2,ncols),
                                    rep(3,ncols),
                                    rep(4,ncols),
                                    rep(5,ncols)))




# plots showing consumption per class

# aggregate data by personID
# obtain a boxplot per type of protein, and region
CONSUMO_ALIMENTAR %>% 
  filter (general_type %in% c("DD", "beef&other", "wildmeat") !=T) %>%
  group_by (income_cat,general_type,position,region,unit_analysis) %>% 
  summarize (Zinc = (sum(ZINCO)),
             Quantidade = sum(QTD)) %>%
  #filter (region %in% c("North")) %>%
  ggplot (aes(x=general_type, 
              y=log(Zinc)))+
  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
  geom_boxplot(aes(fill=income_cat))+
  facet_wrap(~region, nrow=2,ncol=3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45),
        legend.position = c(0.85,0.15))
  

# trends per class

CONSUMO_ALIMENTAR %>% 
  filter (general_type %in% c("DD") !=T) %>%
  group_by (general_type,position,region,unit_analysis,income_cat) %>% 
  summarize (Zinc = (sum(ZINCO)),
             Quantidade = sum(QTD),
             PC_RENDA_MONET=mean(PC_RENDA_MONET)) %>%
  
  ggplot (aes(x=log10(PC_RENDA_MONET), 
              y=log10(Zinc),
              #group = income_cat,
              fill = income_cat,
              colour=income_cat))+
  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
  geom_point (aes(fill=income_cat,
                  colour=income_cat),
              alpha = 0.025)+
  geom_smooth(method= "lm")+
  facet_wrap(~general_type, ncol=5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=0),
        legend.position = c(0.92,0.15))


# trends of protein consumption with income, per region
CONSUMO_ALIMENTAR %>% 
  filter (general_type %in% c("DD") !=T) %>%
  group_by (general_type,position,region,unit_analysis) %>% 
  
  summarize (Zinc = (sum(ZINCO)),
             Quantidade = sum(QTD),
             PC_RENDA_MONET=mean(PC_RENDA_MONET))%>%
  
  ggplot (aes(x=log10(PC_RENDA_MONET), 
              y=log10(Zinc),
              group=region,
              colour=region,
              fill=region))+
  geom_point(aes(colour=region,fill=region),alpha=0.01)+
  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
  geom_smooth(method= "lm")+
  facet_wrap(~general_type, ncol=5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=0))




# trend

CONSUMO_ALIMENTAR %>% 
  filter (general_type %in% c("seafood")) %>%
  group_by (food_type,general_type,position,region,unit_analysis) %>% 
  summarize (Zinc = (sum(ZINCO)),
             Quantidade = sum(QTD),
             PC_RENDA_MONET=mean(PC_RENDA_MONET))%>%
  
  ggplot (aes(x=log10(PC_RENDA_MONET), 
              y=log10(Zinc),
              group=region,
              colour=region,
              fill=region))+
  geom_point(aes(colour=region,fill=region),alpha=0.05)+
  scale_fill_viridis_d(option = "viridis", begin =0.2,end=1)+
  geom_smooth(method= "glm")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=0))


