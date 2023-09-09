# POF data ( too heavy; load --> process --> save)

source("R/packages.R")


# REGISTRO - MORADOR
# PS: already saved as RData
MORADOR <- 
  read.fwf(here ("POF_government","Dados_20210304" ,"MORADOR.txt")
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10,1,1,20,20,20,20)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", 
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V0306", "V0401", "V04021", "V04022", "V04023",
                           "V0403", "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411", "V0412",
                           "V0413", "V0414", "V0415", "V0416", 
                           "V041711", "V041712", "V041721", "V041722",
                           "V041731", "V041732", "V041741", "V041742",
                           "V0418", "V0419", "V0420", "V0421", "V0422",
                           "V0423", "V0424", "V0425", "V0426", "V0427",
                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
                           "INSTRUCAO", "COMPOSICAO", "PC_RENDA_DISP",
                           "PC_RENDA_MONET", "PC_RENDA_NAO_MONET",
                           "PC_DEDUCAO")
           , dec="."
  )   


# REGISTRO - CONSUMO ALIMENTAR
# PS: already saved as RData
CONSUMO_ALIMENTAR <- 
  read.fwf(here ("POF_government","Dados_20210304" ,"CONSUMO_ALIMENTAR.txt") 
           , widths = c(2,4,1,9,2,1,2,2,2,4,2,7,3,
                        2,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,2,2,7,9,6,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,14,14,14,14,14,14,
                        14,14,15,10,15,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFOR,MANTE", "QUADRO", "SEQ",
                           "V9005", "V9007", "V9001", "V9015",
                           "V9016", "V9017", "V9018", "V9019",
                           "V9020", "V9021", "V9022", "V9023",
                           "V9024", "V9025", "V9026", "V9027",
                           "V9028", "V9029", "V9030",
                           "COD_UNIDADE_MEDIDA_FINAL",
                           "COD_PREPARACAO_FINAL", "GRAMATURA1",
                           "QTD", "COD_TBCA", "ENERGIA_KCAL",
                           "ENERGIA_KJ", "PTN", "CHOTOT", "FIBRA",
                           "LIP", "COLEST", "AGSAT", "AGMONO",
                           "AGPOLI", "AGTRANS", "CALCIO", "FERRO",
                           "SODIO", "MAGNESIO", "FOSFORO", "POTASSIO",
                           "COBRE", "ZINCO", "VITA_RAE", "TIAMINA",
                           "RIBOFLAVINA", "NIACINA", "PIRIDOXAMINA",
                           "COBALAMINA", "VITD", "VITE", "VITC",
                           "FOLATO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL", "DIA_SEMANA", "DIA_ATIPICO")
           , dec="."
  )   

#save these heavy datasets
dir.create ("processed_data")
save (MORADOR,CONSUMO_ALIMENTAR,file= here ("processed_data", "POF_processed_data.RData"))
