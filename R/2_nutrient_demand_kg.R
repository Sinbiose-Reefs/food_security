

# ------------------------------------------- # 


# Explore the patterns of consumption across states


# codes used to produce the components of Fig. 1


# basic data of consumption


# ------------------------------------------- # 


# load packages
source("R/packages.R")

# load functions
source ("R/functions.R")


# ----------------



# load data for analysis
load (here ("processed_data",
            "fishConsumption_Income_meat.RData"))



# select only data from coastal state

CONSUMO_ALIMENTAR_MEAT <- CONSUMO_ALIMENTAR_MEAT %>% 
  filter (position == "sea") 


CONSUMO_ALIMENTAR_MEAT$QTD_N <- CONSUMO_ALIMENTAR_MEAT$QTD/CONSUMO_ALIMENTAR_MEAT$Ndays

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
          Magnesium = Magnesium/1000) # into grams


# function to transform quantitites into  kg / year
fun_kg_year <- function (x) {(x/1000)*365}

# extract and transform the data
consumption_all <- filter_interesting_food %>%
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
  group_by(state,income_cat,
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


# averages across all interviewees
sum(consumption_all [which(consumption_all$bluefood ==  "Bluefood"), "QTD_kg"])/length(unique(consumption_all$COD_INFOR))
sum(consumption_all [which(consumption_all$sea_food ==  "1"), "QTD_kg"])/length(unique(consumption_all$COD_INFOR))
# nrow(unique(test [which(test$state == "Alagoas"),"COD_INFOR"]))


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
  group_by(state,income_cat,
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
income_cat <- unique(consumption_nutrients$income_cat)

df_states_kg_nut_SeaFood  <- lapply (states, function (i) 
  
  do.call(rbind, 
    lapply (income_cat, function (k)  { # states and income class
    
      
      tryCatch( {
        
              # subset the data
              d1 <- consumption_nutrients [which(consumption_nutrients$state == i &
                                                   consumption_nutrients$income_cat == k),] 
              # calculate the sum
              df_test <- data.frame (state = i,
                                     income_cat = k,
                                     npop=unique(d1$mean_N_pop),
                                     nint = length (unique(d1$COD_INFOR)),
                                     sum_seafood_kg = sum(d1$QTD_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_protein_kg = sum(d1$PTN_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_calcium_kg = sum(d1$Calcium_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_iron_kg = sum(d1$Iron_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_zinc_kg = sum(d1$Zinc_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_vita_kg = sum(d1$`Vitamin-A_kg`[which(d1$sea_food == "1")],na.rm=T),
                                     sum_omega3_kg = sum(d1$Omega3_kg[which(d1$sea_food == "1")],na.rm=T),
                                     sum_magn_kg = sum(d1$Magnesium_kg[which(d1$sea_food == "1")],na.rm=T))
              
              
              
                },
              
              error = function(e) return ("NULL"))
               
      
  })
  )
)

# melt the list
consumption_nutrients_SeaFood <- do.call(rbind , df_states_kg_nut_SeaFood)

# adjust names
consumption_nutrients_SeaFood <- consumption_nutrients_SeaFood %>% 
  filter (state != "NULL") %>%
  mutate(state_adj = recode(state, 
                            "Rio Grande Do Sul" = "Rio Grande do Sul",
                            "Espírito Santo" = "Espirito Santo",
                            "Rio De Janeiro" = "Rio de Janeiro",
                            "Rio Grande do Norte" = "Rio Grande Do Norte"
  )) %>%
  mutate_at(vars (npop:sum_magn_kg), as.numeric) %>%

  mutate_at(vars (sum_seafood_kg:sum_magn_kg), funs(. / nint))   # per capita amounts
  



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
                                       consumption_nutrients_SeaFood, 
                                       by = c("name_state" = "state_adj"))



states_consumption %>%
  filter (is.na(sum_seafood_kg) != T) %>%
  select(c("name_region", "income_cat", "sum_seafood_kg",-"geom")) %>%
  group_by (name_region,income_cat) %>%
  summarise (sum_seafood_kg_m = mean(sum_seafood_kg,na.rm=T),
             sum_seafood_kg_sd = sd(sum_seafood_kg,na.rm=T)) %>%


  ggplot (aes (x=income_cat, y=sum_seafood_kg_m,col= name_region))+
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=sum_seafood_kg_m, ymax=sum_seafood_kg_m+sum_seafood_kg_sd), width=.2,
                position=position_dodge(.2))+
  facet_wrap(~income_cat,scales = "free",nrow=1)+
  
  geom_segment(aes(x = 0.9, y = mean(sum_seafood_kg_m), 
                   xend = 1.1, yend = mean(sum_seafood_kg_m)), colour = "black",size=1.5) + 
  theme_bw()+
  theme(legend.position = "top") 

ggsave ("output/barplot_consumption_kg.pdf",width=9,height=6)

rm(list=ls())
