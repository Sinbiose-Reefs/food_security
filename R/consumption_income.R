# load

# read.xlsx

require(openxlsx)
require(here)




# income per state
list_dat <- list.files (here ("POF_state", 'tabelas_unidades_da_federacao_xls_20191108'),
                        pattern = "xlsx")
dat_income <- lapply (list_dat, function (i) 
  
    read.xlsx(here ("POF_state", 'tabelas_unidades_da_federacao_xls_20191108', i),
              skipEmptyRows = TRUE,
                 startRow = 5, sheet = 5)
)

# total income

total_income<-unlist(lapply (dat_income, function (i) 
  i[which(i$X1 == "Rendimento total "),"Total"]
))
total_income<-as.numeric(total_income)
names_income <- c ("Rondônia", "Acre","Amazonas","Roraima","Pará","Amapá","Tocantins","Maranhão","Piauí","Ceará",                                          
 "Rio Grande do Norte", "Paraíba","Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro",                                 
  "São Paulo","Paraná","Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal")
names(total_income)<-names_income

# load POF data

dat <- read.xlsx(here ("POF_state", 
                       'POF_2018.xlsx'),
                 startRow = 5)

# fish
dat_fish <- dat [,c(1,seq(min (c(grep ("Pescados",colnames(dat)),grep("Peixe", colnames(dat)))),
                      max (c(grep ("Pescados",colnames(dat)),grep("Peixe", colnames(dat))))))]
# sel rows
dat_fish<-dat_fish[-grep("Fonte",dat_fish$X1),]

# load PIB data

PIB <- read.csv(here ("POF_state", 
                       'Pib_BR.csv'),sep=";")


# match & bind
dat_fish <- cbind (dat_fish, 
                   PIB [match (dat_fish$X1, PIB$state_name),
                        c("state_name", "pib_mean", "position")]
)

# also the income
dat_fish<-cbind (dat_fish, 
       income_RS = total_income [match (dat_fish$X1, names(total_income))]
)



# melt
require(reshape)
dat_fish_df <- melt (dat_fish, 
                     id.vars = c("position",
                            "X1",
                            "pib_mean",
                            "income_RS"))
dat_fish_df$value <- as.numeric(dat_fish_df$value)



# fish from the seawater
sea <- c(    "9.1.4.Cação.fresco",
            "9.1.5.Camarão.fresco",
            "9.1.1.Anchova.fresca",
            "9.1.6.Corvina.fresca" ,                   
            "9.1.7.Meluza.em.filé.congelado",           
            "9.1.8.Merluza.em.filé.fresco"  ,
            "9.1.16.Tainha.fresca",
            "9.2.18.Outros.pescados.salgados",
            "9.1.12.Pescada.fresca",                   
            "9.1.13.Pescadinha.fresca",
            "9.3.4.Peixe.salgado",
            "9.1.9.Parati.fresco",
            "9.1.10.Pescada.em.filé.congelado",
            "9.1.11.Pescada.em.filé.fresco")

# fish from freshwater
fresh <-  c("9.2.1.Acará.fresca",                      
            "9.2.2.Acari.fresco",
            "9.2.3.Anujá.fresco",
            "9.2.4.Curimatã.fresco",                   
            "9.2.5.Dourada.fresca",                    
            "9.2.6.Jaraqui.fresco",                    
            "9.2.7.Lambari.fresco",                    
            "9.2.8.Mapará.fresco",                     
            "9.2.9.Piau.fresco",                       
            "9.2.10.Surubim.fresco",                   
            "9.2.11.Tambaqui.fresco",                  
            "9.2.12.Tilápia.fresca",                   
            "9.2.13.Traíra.fresca",                    
            "9.2.14.Tucunaré.fresco")


# salgada
dat_fish_df$class <- ifelse (dat_fish_df$variable %in% sea,
                             "SeawaterFish",NA)

# doce
dat_fish_df$class <- ifelse (dat_fish_df$variable %in% fresh,
                             "FreshwaterFish",dat_fish_df$class)



# relate
require(dplyr)
require(ggplot2)
require(ggrepel)

# further aggregate per state and class

dat_fish_df_class <- dat_fish_df %>%
  group_by(X1,position,class) %>%
  summarise (sum_fish = sum (value,na.rm=T),
             PIB = mean(pib_mean,na.rm=T),
             income = mean(income_RS,na.rm=T))
dat_fish_df_class <- dat_fish_df_class[which(is.na(dat_fish_df_class$class )!=T) ,]
dat_fish_df_class <- dat_fish_df_class[which(is.na(dat_fish_df_class$position )!=T) ,]

# plot
ggplot(dat_fish_df_class,
       aes (x= log10(`income`), 
            y = log10(`sum_fish`),
            col=position,
            label = X1)) +
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~poly(x,2)) + 
  theme_classic()+
  scale_colour_viridis_d (begin=0.2,end=0.6)+
  geom_text_repel (size=3,max.overlaps = 1000) + 
  facet_wrap(~class,scales="free")
  

# plot
ggplot(dat_fish_df_class,
       aes (x= log10(`PIB`), 
            y = log10(`sum_fish`),
            col=position,
            label = X1)) +
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~poly(x,2)) + 
  theme_classic()+
  scale_colour_viridis_d (begin=0.2,end=0.6)+
  geom_text_repel (size=3,max.overlaps = 1000) + 
  facet_wrap(~class,scales="free")



# ------------------------------------------------------------

# nutrientes POF

nutrients <- read.csv(here ("POF_state", 
                            'Tabela_Nutrição.csv'),
                      encoding = "UTF-8")

nutrients$Est <- gsub ("_", " ", nutrients$Est)
nutrients$Est[which(nutrients$Est == "Rondônica")] <- "Rondônia"
nutrients$Est[which(nutrients$Est == "Goias")] <- "Goiás"
nutrients$position <- dat_fish$position [match (nutrients$Est, dat_fish$X1)]



# sel nutrients
sel_nutrients <- c("PTN",
                   "CALCIO",
                   "FERRO",
                   "AGPOLI",
                   "VITA_RAE",
                   "ZINCO",
                   "COLEST",
                   "SODIO",
                   # also select state and region
                   "Est",
                   "position")


nutrients<- nutrients[,which(colnames(nutrients) %in% sel_nutrients)]

# bind PIB

nutrients <- cbind (nutrients, 
                    PIB [match (nutrients$Est, PIB$state_name),
                        c("pib_mean","position")]
)

# also the income
nutrients <- cbind (nutrients, 
                    income_RS = total_income [match (nutrients$Est, names(total_income))]
)



# melt
nutrients_DF <- melt (nutrients, id.vars = c("Est", 
                                             "position" , 
                                             "pib_mean",
                                             "income_RS"))

# discreticize income
# average income in 2017-2018
# following http://www.fetapergs.org.br/index.php/2015-07-27-16-46-22/tabelas-salario-minimo
nutrients_DF$salary_cutoff <- mean(c(954.00, 937.00))*4 # four salaries as cutoff
nutrients_DF$discrete_income <- ifelse (nutrients_DF$income_RS >=nutrients_DF$salary_cutoff, 
                                        "middle,rich",
                                        "poor")

# plot
# PIB
ggplot(nutrients_DF,
       aes (x= log10(pib_mean), 
            y = log10(value),
            col=position,
            label = Est)) +
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~poly(x,2)) + 
  theme_classic()+
  scale_colour_viridis_d (begin=0.2,end=0.6)+
  geom_text_repel (size=3,max.overlaps = 1000) + 
  facet_wrap(~variable,scales="free")


# income
ggplot(nutrients_DF,
       aes (x= log10(income_RS), 
            y = log10(value),
            col=position,
            label = Est)) +
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~poly(x,2)) + 
  theme_classic()+
  scale_colour_viridis_d (begin=0.2,end=0.6)+
  geom_text_repel (size=3,max.overlaps = 100) + 
  facet_wrap(~variable,scales="free")


# discrete income


# income
jitter_pts <- position_jitter(width = 0.1, height = 0.1)
jitter_stat <- position_jitter(width = 0.1, height = 0.2)

ggplot(nutrients_DF[which(nutrients_DF$position == "sea"),],
       aes (x= log10(value), 
            y = variable,
            col=discrete_income,
            shape = discrete_income,
            label = Est)) +
  geom_point(position = jitter_pts, aes(col = discrete_income),
             alpha = 1) + 
  stat_summary(fun.data="mean_cl_normal", 
               geom="pointrange", 
               size=1.2,
               alpha=0.4,
               aes(shape = discrete_income, 
                   color = (discrete_income)),
               position = jitter_stat)+
  theme_classic()+
  scale_colour_viridis_d (begin=0.2,end=0.6)+
  theme (legend.position = c(0.85,0.1))
  



# --------------------------------------------------------------

# load diet profile


diet_profile <- read.xlsx(here ("POF_state", "POF_2018.xlsx"),
                          startRow = 5)

# chicken
diet_profile[,grep("Frango",colnames(diet_profile))]






# -----------------------------------

# nutrition value of food

nutrient_food <- read.xlsx(here ("POF_state", "tabelacompleta.xlsx"),
                          startRow = 4)

# chicken
chicken <- nutrient_food[grep ("FRANGO", nutrient_food$DESCRIÇÃO.DO.ALIMENTO),]

require(dplyr)

# aggregate
chicken %>% 
  group_by(DESCRIÇÃO.DO.ALIMENTO) %>%
  summarise()


