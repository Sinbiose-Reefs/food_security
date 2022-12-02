# perfil de alimentacao domiciliar
# POF

require(here)
library(dplyr)
require(readxl)
require(openxlsx)
require(reshape)


# REGISTRO - MORADOR
# PS: already saved as RData
#MORADOR <- 
#  read.fwf(here ("POF_government","Dados_20210304" ,"MORADOR.txt")
#           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
#                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
#                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
#                        2,1,2,14,14,10,1,1,20,20,20,20)
#           , na.strings=c(" ")
#           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", 
#                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
#                           "V0306", "V0401", "V04021", "V04022", "V04023",
#                           "V0403", "V0404", "V0405", "V0406", "V0407",
#                           "V0408", "V0409", "V0410", "V0411", "V0412",
#                           "V0413", "V0414", "V0415", "V0416", 
#                           "V041711", "V041712", "V041721", "V041722",
#                           "V041731", "V041732", "V041741", "V041742",
#                           "V0418", "V0419", "V0420", "V0421", "V0422",
#                           "V0423", "V0424", "V0425", "V0426", "V0427",
#                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
#                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
#                           "INSTRUCAO", "COMPOSICAO", "PC_RENDA_DISP",
#                           "PC_RENDA_MONET", "PC_RENDA_NAO_MONET",
#                           "PC_DEDUCAO")
#           , dec="."
#  )   
#


## create an identified to each unit
MORADOR$unit_analysis <- paste(MORADOR$UF,
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
length(unique(MORADOR$unit_analysis ))





# REGISTRO - CONSUMO ALIMENTAR
# PS: already saved as RData
#CONSUMO_ALIMENTAR <- 
#  read.fwf(here ("POF_government","Dados_20210304" ,"CONSUMO_ALIMENTAR.txt") 
#           , widths = c(2,4,1,9,2,1,2,2,2,4,2,7,3,
#                        2,1,1,1,1,1,1,1,1,1,1,1,1,
#                        1,1,2,2,7,9,6,14,14,14,14,
#                        14,14,14,14,14,14,14,14,
#                        14,14,14,14,14,14,14,14,
#                        14,14,14,14,14,14,14,14,
#                        14,14,15,10,15,1
#           )
#           , na.strings=c(" ")
#           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                           "COD_UPA", "NUM_DOM", "NUM_UC",
#                           "COD_INFOR,MANTE", "QUADRO", "SEQ",
#                           "V9005", "V9007", "V9001", "V9015",
#                           "V9016", "V9017", "V9018", "V9019",
#                           "V9020", "V9021", "V9022", "V9023",
#                           "V9024", "V9025", "V9026", "V9027",
#                           "V9028", "V9029", "V9030",
#                           "COD_UNIDADE_MEDIDA_FINAL",
#                           "COD_PREPARACAO_FINAL", "GRAMATURA1",
#                           "QTD", "COD_TBCA", "ENERGIA_KCAL",
#                           "ENERGIA_KJ", "PTN", "CHOTOT", "FIBRA",
#                           "LIP", "COLEST", "AGSAT", "AGMONO",
#                           "AGPOLI", "AGTRANS", "CALCIO", "FERRO",
#                           "SODIO", "MAGNESIO", "FOSFORO", "POTASSIO",
#                           "COBRE", "ZINCO", "VITA_RAE", "TIAMINA",
#                           "RIBOFLAVINA", "NIACINA", "PIRIDOXAMINA",
#                           "COBALAMINA", "VITD", "VITE", "VITC",
#                           "FOLATO", "PESO", "PESO_FINAL",
#                           "RENDA_TOTAL", "DIA_SEMANA", "DIA_ATIPICO")
#           , dec="."
#  )   
#
#

## create an identified to each unit

CONSUMO_ALIMENTAR$unit_analysis <- paste(CONSUMO_ALIMENTAR$UF,
                                         CONSUMO_ALIMENTAR$ESTRATO_POF,
                                         CONSUMO_ALIMENTAR$TIPO_SITUACAO_REG,
                                         CONSUMO_ALIMENTAR$COD_UPA,
                                         CONSUMO_ALIMENTAR$NUM_DOM,
                                         CONSUMO_ALIMENTAR$NUM_UC,
                                         CONSUMO_ALIMENTAR$COD_INFOR.MANTE,
                                         
                               sep = "_")

# Number of interviewees

length(unique(CONSUMO_ALIMENTAR$unit_analysis ))


#View(CONSUMO_ALIMENTAR[which(CONSUMO_ALIMENTAR$COD_UPA == "110000016"),])
#table(CONSUMO_ALIMENTAR [which(CONSUMO_ALIMENTAR$unit_analysis == CONSUMO_ALIMENTAR$unit_analysis[1]),"V9001"],
#      CONSUMO_ALIMENTAR [which(CONSUMO_ALIMENTAR$unit_analysis == CONSUMO_ALIMENTAR$unit_analysis[1]),"QUADRO"])






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
                                 MORADOR[match (CONSUMO_ALIMENTAR$unit_analysis,
                                                MORADOR$unit_analysis),
                                          c("PC_RENDA_DISP","PC_RENDA_MONET", "COMPOSICAO","RENDA_TOTAL", "V0404", "V0405")])


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
                      'Pib_BR.csv'),sep=";")


# match to find 

CONSUMO_ALIMENTAR <- cbind(CONSUMO_ALIMENTAR,
                          PIB[match (CONSUMO_ALIMENTAR$UF, PIB$cod_uf),
                                                      c("position", "region")])



# remove NAs
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR [is.na(CONSUMO_ALIMENTAR$protein_type)!= T,]

# discretisize income
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% 
  mutate (income_cat = cut(PC_RENDA_MONET,breaks = c(-1,1891,3782,9455,18910,200000), # classes
                         labels = c("Class E", "Class D", "Class C", "Class B", "Class A")))  %>%
  filter (is.na(PC_RENDA_MONET) != T)
  


  
# percentage of each class per State

class_state<- read.csv (here ("POF_government", "Classes_municipios.csv"),
                        encoding = "UTF-8") 
class_state <- (class_state [which(class_state$Unidade == "pessoas"),]) # selection N 
# income descriptors
class_state <- class_state [grep ("salário",class_state$Nome),]
# descriptor for total income
class_state<-(class_state [grep ("17.2.1", class_state$Posição),])
# the dimension we want == N income classes x N states
length(unique(class_state$Nome)) * length(unique(class_state$Localidade))
dim(class_state)

# recode according to the class and income
class_state$Nome<-  recode(class_state$Nome, 
       "Até 1/4 de salário mínimo"   = "Class E",
       "Mais de 1/4 a 1/2 salário mínimo"= "Class E",
       "Mais de 1/2 a 1 salário mínimo"  = "Class E",
       "Mais de 1 a 2 salários mínimos"  = "Class E",
       "Mais de 2 a 3 salários mínimos"   = "Class D",
       "Mais de 3 a 5 salários mínimos"  = "Class D",
       "Mais de 5 a 10 salários mínimos" = "Class C",
       "Mais de 10 a 15 salários mínimos"  = "Class B",
       "Mais de 15 a 20 salários mínimos" = "Class B",
       "Mais de 20 a 30 salários mínimos"  = "Class A",
       "Mais de 30 salários mínimos"  = "Class A"
)

# numeric
class_state$N_pop <- as.numeric(class_state$X2010)

# table o population per class and state
tab_N_class_state <- (cast (data = class_state, 
      formula = Localidade ~ Nome,
      value="N_pop",
      drop=F,
      fun.aggregate = sum))

# melt
tab_N_class_state_df <- melt(tab_N_class_state, id.vars = "Localidade")
tab_N_class_state_df$Localidade [which(tab_N_class_state_df$Localidade == "Rio Grande do Sul")] <- "Rio Grande Do Sul" # adjust name

# table proportion per class
tab_prop_class_state <- (cast (data = class_state, 
                            formula = Localidade ~ Nome,
                            value="N_pop",
                            fun.aggregate = sum))
tab_prop_class_state[,c("Class A","Class B","Class C","Class D","Class E")] <- tab_prop_class_state[,c("Class A","Class B","Class C","Class D","Class E")]/rowSums(tab_prop_class_state[,c("Class A","Class B","Class C","Class D","Class E")])


# melt
tab_prop_class_state_df <- melt(tab_prop_class_state, 
                                id.vars = "Localidade")
tab_prop_class_state_df$Localidade [which(tab_prop_class_state_df$Localidade == "Rio Grande do Sul")] <- "Rio Grande Do Sul"


# match with the complete dataset
tab_N_class_state_df$interact_state_class <- paste (tab_N_class_state_df$Localidade, tab_N_class_state_df$Nome,sep=".")
tab_prop_class_state_df$interact_state_class<- paste (tab_prop_class_state_df$Localidade, tab_prop_class_state_df$Nome,sep=".")
CONSUMO_ALIMENTAR$interact_state_class <- paste (CONSUMO_ALIMENTAR$state, CONSUMO_ALIMENTAR$income_cat,sep=".")

# POF data did not have all combinations of class and state
length(unique(tab_prop_class_state_df$interact_state_class))
length(unique(CONSUMO_ALIMENTAR$interact_state_class))

# match -- some zeros will appear in the final table
# total N
CONSUMO_ALIMENTAR$N_pop_class <- tab_N_class_state_df [match(CONSUMO_ALIMENTAR$interact_state_class,
                                                             tab_N_class_state_df$interact_state_class),
                                                       "value"]
# proportions
CONSUMO_ALIMENTAR$proportion_pop_class <- tab_prop_class_state_df [match(CONSUMO_ALIMENTAR$interact_state_class,
                                                                         tab_prop_class_state_df$interact_state_class),
                                                                   "value"]

 
# check
## NAs are due to missing samples in POF
cast (CONSUMO_ALIMENTAR, formula = state~income_cat,
      fun.aggregate = mean,
      na.rm=T,
      drop=F,
      value= "N_pop_class")





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
  dplyr::rename("Polyunsatured fat"= AGPOLI ,
                "Calcium" = CALCIO,
                "Iron" = FERRO,
                "Zinc" = ZINCO,
                "Vitamin-A" = VITA_RAE,
                "Magnesium" = MAGNESIO) %>% 
  filter (position == "sea" & 
            general_type != "DD")



# useful data to report in the paper
# sex
colSums(table (CONSUMO_ALIMENTAR$unit_analysis,
             CONSUMO_ALIMENTAR$Sex)>0)/length(unique(CONSUMO_ALIMENTAR$unit_analysis))
# race
round (colSums(table (CONSUMO_ALIMENTAR$unit_analysis,
               CONSUMO_ALIMENTAR$Race)>0)/length(unique(CONSUMO_ALIMENTAR$unit_analysis)),2)
# class
round (colSums(table (CONSUMO_ALIMENTAR$unit_analysis,
               CONSUMO_ALIMENTAR$income_cat)>0)/length(unique(CONSUMO_ALIMENTAR$unit_analysis)),2)

# state
dat_state<- data.frame (int=(colSums(table (CONSUMO_ALIMENTAR$unit_analysis,
                                CONSUMO_ALIMENTAR$state)>0))[order((colSums(table (CONSUMO_ALIMENTAR$unit_analysis,
                                                                                   CONSUMO_ALIMENTAR$state)>0)),decreasing=T)])
dat_state$prop <- dat_state$int/length(unique(CONSUMO_ALIMENTAR$unit_analysis))
(dat_state <- rbind (dat_state, 
       data.frame (int=sum(dat_state$int),
                    prop=sum(dat_state$prop))
))
write.xlsx(dat_state,file = here ("output", "state_interviewees.xlsx"), rowNames=T)

# save the dataset to data analysis
save (CONSUMO_ALIMENTAR, file = here ("output",
                                      "fishConsumption_Income.RData"))


# end data organization
rm(list=ls())




# garbage



# LIST OF FRESHWATER FISH
#FW_FISH <- c("PEIXE DE AGUA DOCE (EM POSTA, EM FILE, ETC)" ,
#             "PEIXE DE AGUA DOCE SALGADO (EM POSTA, EM FILE, ETC)",
#             "DOURADO", # marinho - ver o estado
#             "DOURADO NA FOLHA DE BANANEIRA", # marinho
#             "PITU",
#             "PACU",
#             "TAMBAQUI",
#             "TUCUNARE")

# LIST OF SEAWATER FISH
#SW_FISH <- c("PEIXE DO MAR (EM POSTA, EM FILE, ETC)",
#             "PEIXE SALGADO (EM POSTA, EM FILE, ETC)",
#             "BACALHAU",
#             "TARTARE DE SALMAO",
#             "SARDINHA EM CONSERVA",
#             "ATUM EM CONSERVA",
#             "SALMAO EM CONSERVA",
#             "ATUM EM CONSERVA LIGHT",
#             "SARDELA",
#             "BOLINHO DE BACALHAU",
#             "SANDUICHE DE ATUM",
#             "PAO COM ATUM",
#             "PAO COM SALMAO",
#             "SANDUICHE DE SALMAO",
#             "SANDUICHE DE SARDINHA",
#             "PAO COM SARDINHA",
#             "PIZZA DE PEIXE (ATUM, SALMAO, SARDINHA)",
#             "CUSCUZ DE ATUM",
#             "ESCONDIDINHO DE BACALHAU",
#             "SALADA DE ATUM",
#             "SALADA DE BACALHAU COM BATATAS",
#             "SALADA DE BATATA E ATUM",
#             "MOUSSE DE SALMAO",
#             "SUFLE DE BACALHAU",
#             "TORTA DE BACALHAU",
#             "TORTA DE PEIXE / ATUM / SARDINHA"
#)


# source of food 

#CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>%
#  mutate (env_food = ifelse (food_type  %in% SW_FISH,
#                             "SWFISH",
#                             ifelse (food_type %in% FW_FISH,
#                                     "FW_FISH",
#                                     NA)))
#
