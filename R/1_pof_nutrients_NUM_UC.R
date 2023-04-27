# organize data to analyses

source("R/packages.R")

# load data
load ( "POF_processed_data.RData")

## create an identified to each unit
MORADOR$COD_INFOR  <- paste(MORADOR$UF,
                            MORADOR$ESTRATO_POF,
                            MORADOR$TIPO_SITUACAO_REG,
                            MORADOR$COD_UPA,
                            MORADOR$NUM_DOM,
                            MORADOR$NUM_UC,
                            MORADOR$COD_INFORMANTE,
                            sep = "_")


# average per capita income (family average) 
income_MORADOR <- MORADOR %>% 
  group_by (UF) %>%
  summarize (mean_income = mean(PC_RENDA_MONET,na.rm=T))


# interviewees
length(unique(MORADOR$COD_INFOR  ))


## create an identified to each unit
CONSUMO_ALIMENTAR$COD_INFOR  <- paste(CONSUMO_ALIMENTAR$UF,
                                         CONSUMO_ALIMENTAR$ESTRATO_POF,
                                         CONSUMO_ALIMENTAR$TIPO_SITUACAO_REG,
                                         CONSUMO_ALIMENTAR$COD_UPA,
                                         CONSUMO_ALIMENTAR$NUM_DOM,
                                         CONSUMO_ALIMENTAR$NUM_UC,
                                         CONSUMO_ALIMENTAR$COD_INFOR.MANTE,
                                         
                               sep = "_")

# Number of interviewees
length(unique(CONSUMO_ALIMENTAR$COD_INFOR  ))


## create an identified to each family
CONSUMO_ALIMENTAR$COD_FAMILY  <- paste(CONSUMO_ALIMENTAR$UF,
                                      CONSUMO_ALIMENTAR$ESTRATO_POF,
                                      CONSUMO_ALIMENTAR$TIPO_SITUACAO_REG,
                                      CONSUMO_ALIMENTAR$COD_UPA,
                                      CONSUMO_ALIMENTAR$NUM_DOM,
                                      CONSUMO_ALIMENTAR$NUM_UC,
                                      
                                      sep = "_")

length(unique(CONSUMO_ALIMENTAR$COD_FAMILY  ))


#View(CONSUMO_ALIMENTAR[which(CONSUMO_ALIMENTAR$COD_UPA == "110000016"),])
#table(CONSUMO_ALIMENTAR [which(CONSUMO_ALIMENTAR$COD_INFOR  == CONSUMO_ALIMENTAR$COD_INFOR [1]),"V9001"],
#      CONSUMO_ALIMENTAR [which(CONSUMO_ALIMENTAR$COD_INFOR  == CONSUMO_ALIMENTAR$COD_INFOR [1]),"QUADRO"])



# load omega3 data to match
omega3 <- read.csv (here ("POF_government", 
                                 "Omega3_QTD.csv"),
                     sep=",")


# bind omega 3 in the consumption table
CONSUMO_ALIMENTAR$Omega3 <- (omega3$O3 [match (CONSUMO_ALIMENTAR$COD_INFOR, omega3$COD_INFOR)])



# Loading documentation -- food codes
Documentacao <- read.xlsx (here ("POF_government", 
                                "Cadastro de Produtos do Consumo Alimentar.xlsx"),sheet=1)


# replace empty cells by NA
Documentacao$protein_type[nchar(Documentacao$protein_type)==0] <- NA 


# MATCHING FOOD TYPE WITH THE CODE
# table(unique(CONSUMO_ALIMENTAR$V9001) %in% unique(Documentacao$`CÓDIGO DO ALIMENTO`))

CONSUMO_ALIMENTAR$food_type <- Documentacao[match (CONSUMO_ALIMENTAR$V9001,
                                                    Documentacao$CÓDIGO.DO.ALIMENTO),"DESCRIÇÃO.DO.ALIMENTO"]
CONSUMO_ALIMENTAR$food_code <- Documentacao[match (CONSUMO_ALIMENTAR$V9001,
                                                   Documentacao$CÓDIGO.DO.ALIMENTO),"CÓDIGO.DO.ALIMENTO"]
CONSUMO_ALIMENTAR$protein_type <- Documentacao[match (CONSUMO_ALIMENTAR$V9001,
                                                   Documentacao$CÓDIGO.DO.ALIMENTO),"protein_type"]
CONSUMO_ALIMENTAR$sea_food <- Documentacao[match (CONSUMO_ALIMENTAR$V9001,
                                                      Documentacao$CÓDIGO.DO.ALIMENTO),"Seafood"]
CONSUMO_ALIMENTAR$single_PTN <- Documentacao[match (CONSUMO_ALIMENTAR$V9001,
                                                  Documentacao$CÓDIGO.DO.ALIMENTO),"single_PTN"]



# BIND INCOME
CONSUMO_ALIMENTAR <- cbind(CONSUMO_ALIMENTAR,
                                 MORADOR[match (CONSUMO_ALIMENTAR$COD_INFOR ,
                                                MORADOR$COD_INFOR ),
                                          c("PC_RENDA_DISP",
                                            "PC_RENDA_MONET", 
                                            "COMPOSICAO",
                                            "RENDA_TOTAL",
                                            "V0404", 
                                            "V0405")])


# change colnames
colnames(CONSUMO_ALIMENTAR)[which(colnames(CONSUMO_ALIMENTAR) %in% 
                                                  c("V0404", "V0405"))] <- c("Sex", "Race")


# recode sex
CONSUMO_ALIMENTAR$Sex <- ifelse (CONSUMO_ALIMENTAR$Sex == 1, "Man", "Woman")


# recode race
CONSUMO_ALIMENTAR$Race <- as.character (CONSUMO_ALIMENTAR$Race)
CONSUMO_ALIMENTAR$Race <- recode(CONSUMO_ALIMENTAR$Race,
                        "4" = "pardo",
                        "1" = "branco",
                        "2" = "preto",
                        "3" = "amarelo",
                        "5" = "indigena",
                        "9" = "nao_declarado")



# COLUMNS WE DON'T NEED
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR [,which (colnames(CONSUMO_ALIMENTAR) %in% 
                                   c(paste ("V90",seq (15,30),sep=""),
                                            "COD_UNIDADE_MEDIDA_FINAL","COD_PREPARACAO_FINAL") == F
)]






# BR states
states <- read_excel (here ("POF_government", 
                                  "Documentacao_20210423",
                                  "Estratos POF 2017-2018.xls"))




# df with state codes
df_states <- data.frame (states = states$...1,
                         codes = substr(states$ESTRATOS,1,2)
)



# code of states
CONSUMO_ALIMENTAR$state <- ((df_states [match (CONSUMO_ALIMENTAR$UF, 
                                                             df_states$codes),
                                                      "states"]))





# load PIB data to match PIB, region and position

PIB <- read.csv(here ("POF_state", 
                      'Pib_BR.csv'),
                sep=";")


# match to find 
CONSUMO_ALIMENTAR <- cbind(CONSUMO_ALIMENTAR,
                          PIB[match (CONSUMO_ALIMENTAR$UF, PIB$cod_uf),
                                                      c("position", "region")])





# discretisize income
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% 
  mutate (income_cat = cut(PC_RENDA_MONET,breaks = c(-1,
                                                     1891,
                                                     3782,
                                                     9455,
                                                     18910,
                                                     200000), # classes
                                         labels = c("Class E", 
                                                    "Class D", 
                                                    "Class C", 
                                                    "Class B", 
                                                    "Class A")))  %>%
  
                  filter (is.na(PC_RENDA_MONET) != T) # remove missing classes
  
# load population size data
library("readxl")
# 2017
state_population_2017  <-  read_excel(here ("POF_government",
                                            "POP2017_20220905.xls"), 
                                      sheet=1,skip=1) %>% 
  mutate(state_adj = recode(`BRASIL E UNIDADES DA FEDERAÇÃO`, 
                            "Rio Grande do Sul" = "Rio Grande Do Sul",
  ))
state_population_2017 <- (state_population_2017[which(state_population_2017$state_adj %in% 
                              unique(CONSUMO_ALIMENTAR$state)),])
state_population_2017$`POPULAÇÃO ESTIMADA` <- gsub ("(*)","",state_population_2017$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2017$`POPULAÇÃO ESTIMADA` <- gsub (" (*)","",state_population_2017$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2017$`POPULAÇÃO ESTIMADA` <- gsub (" (**)","",state_population_2017$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2017$`POPULAÇÃO ESTIMADA` <- gsub (".","",state_population_2017$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2017$`POPULAÇÃO ESTIMADA` <- as.numeric(state_population_2017$`POPULAÇÃO ESTIMADA`)

# 2018
state_population_2018  <-  read_excel(here ("POF_government",
                                            "POP2018_20220905.xls"), 
                                      sheet=1,skip=1) %>% 
  mutate(state_adj = recode(`BRASIL E UNIDADES DA FEDERAÇÃO`, 
                            "Rio Grande do Sul" = "Rio Grande Do Sul",
  ))
state_population_2018 <- (state_population_2018[which(state_population_2018$state_adj %in% 
                                  unique(CONSUMO_ALIMENTAR$state)),])
state_population_2018$`POPULAÇÃO ESTIMADA` <- gsub ("(*)","",state_population_2018$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2018$`POPULAÇÃO ESTIMADA` <- gsub (" (**)","",state_population_2018$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2018$`POPULAÇÃO ESTIMADA` <- gsub (" (***)","",state_population_2018$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2018$`POPULAÇÃO ESTIMADA` <- gsub (".","",state_population_2018$`POPULAÇÃO ESTIMADA`,fixed = TRUE)
state_population_2018$`POPULAÇÃO ESTIMADA` <- as.numeric(state_population_2018$`POPULAÇÃO ESTIMADA`)

# df population 
df_population <- data.frame (state = state_population_2018$state_adj,
                             N_pop = rowMeans(cbind (state_population_2018$`POPULAÇÃO ESTIMADA`, 
                                                    state_population_2017$`POPULAÇÃO ESTIMADA`)))


# match state and consumption
CONSUMO_ALIMENTAR$N_pop_state  <-  df_population$N_pop [match (CONSUMO_ALIMENTAR$state, df_population$state)]


# examples : inf - levels that did not exist in data 
which(CONSUMO_ALIMENTAR$state == "Alagoas" & CONSUMO_ALIMENTAR$income_cat == "Class A")


# general food type

CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% 
  mutate(general_type = recode(protein_type, 
                               "beef" = "Beef",
                               "chicken" = "Poultry",
                               "chicken " = "Poultry",
                               "turkey" = "Poultry",
                               "duck" = "Poultry",
                               "lamb" = "Goat",
                               "goat" = "Goat",
                               "beef/pork" = "Beef&other",
                               "beef/pork/chicken" = "Beef&other",
                               "pork/beef" = "Beef&other",
                               "SWfish" = "Seafood",
                               "SWfish/cephalopod/crustacean/mollusk" = "Seafood",
                               "crustacean" = "Seafood",
                               "mollusk" = "Seafood",
                               "cephalopod/crustacean/mollusk" = "Seafood",        
                               "SWfish/crustacean" = "Seafood" ,
                               "cephalopod" = "Seafood" ,
                               "Ifish" = "Imported fish",
                               "FWfish" = "Freshwater fish",
                               "wildmeat" = "Game",
                               "pork" = "Pork"
                               
  )) %>% 
  filter (DIA_ATIPICO == 2) %>% # remove atypical days (1=atypical)
  #filter (general_type != "Beef&other") %>%
  # remove beef&other
  dplyr::rename("Omega3"= Omega3 ,
                "Calcium" = CALCIO,
                "Iron" = FERRO,
                "Zinc" = ZINCO,
                "Vitamin-A" = VITA_RAE,
                "Magnesium" = MAGNESIO)

# factor bluefood

CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% 
  
  mutate(general_type, bluefood=ifelse (general_type %in% c("Seafood","Freshwater fish",
                                                            "Imported fish"),
                                        "Bluefood",
                                        "RedMeat/Other"))


# bind the number of interview days
Ndays <- lapply (unique(CONSUMO_ALIMENTAR$COD_INFOR), function (i)
  
               length (unique(CONSUMO_ALIMENTAR$DIA_SEMANA [which(CONSUMO_ALIMENTAR$COD_INFOR == i)]))
)
names(Ndays) <- unique(CONSUMO_ALIMENTAR$COD_INFOR)
 # melt
Ndays <- data.matrix (do.call(rbind,Ndays))
dimnames(Ndays)[2] <- "Ndays"

# match
CONSUMO_ALIMENTAR$Ndays <- Ndays[match (CONSUMO_ALIMENTAR$COD_INFOR,
                         rownames(Ndays)), "Ndays"]

unique(CONSUMO_ALIMENTAR [which (CONSUMO_ALIMENTAR$COD_INFOR == "21_2121_2_210079610_11_1_1"),"DIA_SEMANA"])
(CONSUMO_ALIMENTAR [which (CONSUMO_ALIMENTAR$COD_INFOR == "21_2121_2_210079610_11_1_1"),"Ndays"])


# useful data to report in the paper
# sex
colSums(table (CONSUMO_ALIMENTAR$COD_INFOR ,
             CONSUMO_ALIMENTAR$Sex)>0)/length(unique(CONSUMO_ALIMENTAR$COD_INFOR ))
# race
round (colSums(table (CONSUMO_ALIMENTAR$COD_INFOR ,
               CONSUMO_ALIMENTAR$Race)>0)/length(unique(CONSUMO_ALIMENTAR$COD_INFOR )),2)
# class
round (colSums(table (CONSUMO_ALIMENTAR$COD_INFOR ,
               CONSUMO_ALIMENTAR$income_cat)>0)/length(unique(CONSUMO_ALIMENTAR$COD_INFOR )),2)

# state
dat_state<- data.frame (int=(colSums(table (CONSUMO_ALIMENTAR$COD_INFOR ,
                                CONSUMO_ALIMENTAR$state)>0))[order((colSums(table (CONSUMO_ALIMENTAR$COD_INFOR ,
                                                                                   CONSUMO_ALIMENTAR$state)>0)),decreasing=T)])
dat_state$prop <- dat_state$int/length(unique(CONSUMO_ALIMENTAR$COD_INFOR ))
round(dat_state <- rbind (dat_state, 
       data.frame (int=sum(dat_state$int),
                    prop=sum(dat_state$prop))
),2)
write.xlsx(dat_state,file = here ("output", "state_interviewees.xlsx"), rowNames=T)

# families
length(unique(CONSUMO_ALIMENTAR$COD_INFOR))
length(unique(CONSUMO_ALIMENTAR$COD_FAMILY))

# number of food items
length(unique(CONSUMO_ALIMENTAR$food_type))
length(unique(CONSUMO_ALIMENTAR$food_type [which(CONSUMO_ALIMENTAR$bluefood == "RedMeat/Other")]))
length(unique(CONSUMO_ALIMENTAR$food_type [which(CONSUMO_ALIMENTAR$bluefood == "Bluefood")]))
length(unique(CONSUMO_ALIMENTAR$food_type [which(CONSUMO_ALIMENTAR$single_PTN == 1)]))
length(unique(CONSUMO_ALIMENTAR$food_type [which(CONSUMO_ALIMENTAR$single_PTN == 1 & 
                                                   CONSUMO_ALIMENTAR$sea_food == 1)]))

# save  
save (CONSUMO_ALIMENTAR, file = here ("output",
                                      "fishConsumption_Income_all_food.RData"))



# onlymeat data 
CONSUMO_ALIMENTAR_MEAT <- CONSUMO_ALIMENTAR %>% 
  
  # remove beef&other
  filter (general_type != "Beef&other") %>%
  
  # removed indefined
  filter (general_type != "DD")



# food consumption - meat
save (CONSUMO_ALIMENTAR_MEAT, file = here ("output",
                                      "fishConsumption_Income.RData"))


# end data organization
rm(list=ls())

