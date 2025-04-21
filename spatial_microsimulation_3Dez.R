#Directory
setwd("C:/Users/lucas/Downloads")

#Packages
library(dplyr)
library(readxl)
library(readr)
library(survey)
library(tidyr)
library(writexl)
library(sf)
library(ggplot2)

###Data###
households <- read_excel("banco_final_lucas_modelagem_pof_2017_26_08_24.xlsx")
View(households)

establishments <- read_excel("estabelecimentos-CAISAN-nov24.xlsx")
View(establishments)

urbanrural <- read_excel("Tipologia_municipal_rural_urbano.xlsx")
View(urbanrural)

#Histogram and boxplot of the density of establishments for buy healthy food per municipality
hist(establishments$dens_estab_saud_mil)
boxplot(establishments$dens_estab_saud_mil)

#Means of the number of establishments for buy healthy food per UF
establishments$sigla <- as.factor(establishments$sigla)
str(establishments$sigla)

mean_estab_UF <- establishments %>%
  group_by(sigla) %>%
  summarise(media = mean(n_estab_nat_RAIS, na.rm = TRUE))
print(mean_estab_UF, n = 27)

mean_estab_filter <- mean_estab_UF[mean_estab_UF$media != 1256,] #Except DF
print(mean_estab_filter)
plot(mean_estab_filter$media)

num_estab_UF <- establishments %>%
  group_by(sigla) %>%
  summarise(contagem = sum(n_estab_nat_RAIS, na.rm = TRUE))
print(num_estab_UF, n = 27)

summary(establishments$n_estab_nat_RAIS)

mean_estab_nat <- mean(num_estab_filter$contagem)

graf_number <- ggplot(num_estab_UF, aes(x = sigla, y = contagem)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_hline(aes(yintercept = mean_estab_nat, color = "National average"), 
             linetype = "dashed", size = 1) +  # Linha com legenda
  scale_color_manual(name = "", values = c("National average" = "red")) +
  labs(x = "States",
       y = "Number of establishments") +
  theme_minimal() +
  theme(legend.position = "top")  # Ou "right", "bottom", etc.

graf_number

###Merging the establishments and urban/rural data sets###
establ_merged <- merge(establishments, urbanrural, by = "cod_IBGE")
View(establ_merged)

###Classifying municipalities into urban and rural using the household and establishment databases###
establ_merged <- establ_merged %>%
  mutate(
    UrbanoRural = ifelse(TIPO %in% c("Urbano", "IntermediarioAdjacente"), 1, 0), 
    ifelse(TIPO %in% c("IntermediarioRemoto", "RuralAdjacente", "RuralRemoto"), 1, 0)
  )
View(establ_merged)

establ_merged <- establ_merged %>%
  mutate(
    UrbanRural = case_when(
      UrbanoRural <= 0 ~ "Rural",  
      UrbanoRural >= 1 ~ "Urban", 
    )
  )
View(establ_merged)

#Transforming and adding labels to the variable in the household database
households$tipo_situacao_reg <- as.factor(households$tipo_situacao_reg)
households$tipo_situacao_reg <- factor(households$tipo_situacao_reg,
                                       levels = c(1, 2),
                                       labels = c("Urban", "Rural"))
View(households)

#Transforming the uf and urban/rural variables into factors
establ_merged$uf <- as.factor(establ_merged$uf)
establ_merged$UrbanRural <- as.factor(establ_merged$UrbanRural)
households$uf <- as.factor(households$uf)
households$tipo_situacao_reg <- as.factor(households$tipo_situacao_reg)

###Changing the UF codes for the names in the establishment database###
establ_merged <- establ_merged %>%
  mutate(uf = case_when(
    uf == "11" ~ "Rondônia",
    uf == "12" ~ "Acre",
    uf == "13" ~ "Amazonas",
    uf == "14" ~ "Roraima",
    uf == "15" ~ "Pará",
    uf == "16" ~ "Amapá",
    uf == "17" ~ "Tocantins",
    uf == "21" ~ "Maranhão",
    uf == "22" ~ "Piauí",
    uf == "23" ~ "Ceará",
    uf == "24" ~ "Rio Grande do Norte",
    uf == "25" ~ "Paraíba",
    uf == "26" ~ "Pernambuco",
    uf == "27" ~ "Alagoas",
    uf == "28" ~ "Sergipe",
    uf == "29" ~ "Bahia",
    uf == "31" ~ "Minas Gerais",
    uf == "32" ~ "Espírito Santo",
    uf == "33" ~ "Rio de Janeiro",
    uf == "35" ~ "São Paulo",
    uf == "41" ~ "Paraná",
    uf == "42" ~ "Santa Catarina",
    uf == "43" ~ "Rio Grande do Sul",
    uf == "50" ~ "Mato Grosso do Sul",
    uf == "51" ~ "Mato Grosso",
    uf == "52" ~ "Goiás",
    uf == "53" ~ "Distrito Federal",
  ))

View(establ_merged)

#Changing the UF codes for the names in the household database
households <- households %>%
  mutate(uf = case_when(
    uf == "11" ~ "Rondônia",
    uf == "12" ~ "Acre",
    uf == "13" ~ "Amazonas",
    uf == "14" ~ "Roraima",
    uf == "15" ~ "Pará",
    uf == "16" ~ "Amapá",
    uf == "17" ~ "Tocantins",
    uf == "21" ~ "Maranhão",
    uf == "22" ~ "Piauí",
    uf == "23" ~ "Ceará",
    uf == "24" ~ "Rio Grande do Norte",
    uf == "25" ~ "Paraíba",
    uf == "26" ~ "Pernambuco",
    uf == "27" ~ "Alagoas",
    uf == "28" ~ "Sergipe",
    uf == "29" ~ "Bahia",
    uf == "31" ~ "Minas Gerais",
    uf == "32" ~ "Espírito Santo",
    uf == "33" ~ "Rio de Janeiro",
    uf == "35" ~ "São Paulo",
    uf == "41" ~ "Paraná",
    uf == "42" ~ "Santa Catarina",
    uf == "43" ~ "Rio Grande do Sul",
    uf == "50" ~ "Mato Grosso do Sul",
    uf == "51" ~ "Mato Grosso",
    uf == "52" ~ "Goiás",
    uf == "53" ~ "Distrito Federal",
  ))

View(households)

#Mean and median of density of establishments for buy healthy food per UF (CAISAN base)
mean_dens_UF <- establ_merged %>%
  group_by(uf, UrbanRural) %>%
  summarise(
    media = mean(dens_estab_saud_mil),
    mediana = median(dens_estab_saud_mil)
  )
print(mean_dens_UF, n = 53)
View(mean_dens_UF)
plot(mean_dens_UF$mediana)
hist(mean_dens_UF$mediana)

#write_xlsx(mean_dens_UF, "mean_dens_UF.xlsx")

#Creating clustered bar chart to illustrate the distribution of median density observed in the establishment database
ggplot(mean_dens_UF, aes(x = uf, y = mediana, fill = factor(UrbanRural, levels = c("Urban", "Rural")))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  labs(x = "State",
       y = "Observed median density of establishments for purchasing healthy foods",
       fill = "Urban / Rural") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclinar rótulos para melhor leitura

###Allocating the densities of establishments for the purchase of healthy food for each household###
#Calculating the probability of each municipality
establ_merged <- establ_merged %>%
  group_by(uf, `UrbanRural`) %>%
  mutate(
    `Pop_Tot_Urb-Rur` = sum(`n_hab`),
    Probabilidade = `n_hab` / `Pop_Tot_Urb-Rur`
  ) %>%
  ungroup()

#Calculating the weighted density
establ_merged <- establ_merged %>%
  mutate(
    `Densid_Pond` = Probabilidade * `dens_estab_saud_mil`
  )
View(establ_merged)
#Obtaining the expected average densities by UF and Urban/Rural
dens_med <- establ_merged %>%
  group_by(uf, `UrbanRural`) %>%
  summarise(
    `Densid_esper` = sum(`Densid_Pond`)
  ) %>%
  ungroup()
str(dens_med)
View(dens_med)

#Assigning expected densities to households
households <- households %>% rename(UrbanRural = tipo_situacao_reg)

households <- households %>%
  left_join(dens_med, by = c("uf", "UrbanRural"))

#Visualising the result
hist(households$Densid_esper)
boxplot(households$Densid_esper)
View(households)

#Mean and median of expected density of establishments for buy healthy food per UF (POF base)
mean_expecdens_UF <- households %>%
  group_by(uf, UrbanRural) %>%
  summarise(
    media = mean(Densid_esper),
    mediana = median(Densid_esper)
  )
print(mean_expecdens_UF, n = 54)
hist(mean_expecdens_UF$mediana)

#write_xlsx(mean_expecdens_UF, "mean_expecdens_UF.xlsx")

#Creating a grouped bar chart to illustrate the distribution of estimated density for the household database
ggplot(mean_expecdens_UF, aes(x = uf, y = mediana, fill = factor(UrbanRural, levels = c("Urban", "Rural")))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  labs(x = "State",
       y = "Estimated median density of establishments for purchasing healthy foods",
       fill = "Urban / Rural") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #Inclinar rótulos para melhor leitura

#Map of the spatial distribution of the expected density in the UF's
#mapa <- st_read("C:/Users/lucas/Downloads/BR_UF_2022/BR_UF_2022.shp")
#print(mapa, n = 27)

#mapa <- mapa %>% rename(uf = NM_UF)

#map_data <- mapa %>% left_join(households, by = "uf")
#map_data$UrbanRural <- as.factor(map_data$UrbanRural)

#urban_map <- map_data %>% filter(UrbanRural == "Urbano")
#rural_map <- map_data %>% filter(UrbanRural == "Rural")

#mean_dens_esper_rur <- rural_map %>%
  #group_by(uf) %>%
  #summarise(media = mean(Densid_esper, na.rm = TRUE))
#print(mean_dens_esper_rur, n = 27)

#st_write(urban_map, "C:/Users/lucas/Downloads/urban_map_POF.shp") #Exportando o mapa do cluster urbano
#st_write(rural_map, "C:/Users/lucas/Downloads/rural_map_POF.shp") #Exportando o mapa do cluster rural

#ggplot(data = rural_map) +
  #geom_sf(aes(fill = Densid_esper)) +
  #scale_fill_viridis_c(option = "plasma") +
  #theme_minimal() +
  #labs(title = "Expected density of establishments for buying healthy food (Rural)",
       #fill = "Expected density")

###People enrolled in Bolsa Familia (social program)####

#Carregando os bancos de dados originais da POF 2017/2018
#Outros rendimentos (exceto renda oriunda do trabalho), domicilios e rendimento oriundo do trabalho
rendimentos <- read_excel("C:/Users/lucas/Downloads/Dados_20230713/OUTROS_RENDIMENTOS.XLSX")
View(rendimentos)
domic <- read_excel("C:/Users/lucas/Downloads/Dados_20230713/DOMICILIO.xlsx")
View(domic)
trab <- read_excel("C:/Users/lucas/Downloads/Dados_20230713/RENDIMENTO_TRABALHO.xlsx")
View(trab)

rendimentos <- rendimentos %>%
  mutate(ID = paste(COD_UPA, NUM_DOM, sep = "_"))

domic <- domic %>%
  mutate(ID = paste(COD_UPA, NUM_DOM, sep = "_"))

trab <- trab %>%
  mutate(ID = paste(COD_UPA, NUM_DOM, sep = "_"))

#Criando variável binária para identificar domicílios que recebiam o Bolsa Família
bolsa_familia1 <- rendimentos %>%
  filter(V9001 %in% c(5400102, 5400101, 5400104, 5400105, 5400106, 5400107)) %>%  #Filtrar quem recebeu Bolsa Família
  select(ID) %>%
  distinct() %>%  #Remover duplicatas caso um domicílio tenha múltiplos registros
  mutate(recebe_bolsa = 1) #Criar variável binária (1 = recebe Bolsa Família)
View(bolsa_familia1)

#Criando variável com o valor total do Bolsa Família recebido no domicílio
bolsa_familia2 <- rendimentos %>%
  filter(V9001 %in% c(5400102, 5400101, 5400104, 5400105, 5400106, 5400107)) %>%  #Filtrar quem recebeu Bolsa Família
  select(ID, V8500) %>%
  group_by(ID) %>%
  distinct() %>%  #Remover duplicatas caso um domicílio tenha múltiplos registros
  summarise(renda_bolsa = sum(V8500, na.rm = TRUE))  #Somar valores do Bolsa Família por domicílio
View(bolsa_familia2)

#Criando variável de proporção do valor do Bolsa Família em relação à renda total no banco de dados de rendimentos
rendimentos <- rendimentos %>%
  mutate(prop_BF = V8500*100/RENDA_TOTAL)
View(rendimentos)

#Criando variável de proporção do valor do Bolsa Família em relação à renda total do domicílio
bolsa_familia3 <- rendimentos %>%
  filter(V9001 %in% c(5400102, 5400101, 5400104, 5400105, 5400106, 5400107)) %>%  #Filtrar quem recebeu Bolsa Família
  select(ID, prop_BF) %>%
  group_by(ID) %>%
  distinct() %>%  #Remover duplicatas caso um domicílio tenha múltiplos registros
  summarise(prop_BF_tot = sum(prop_BF, na.rm = TRUE))  #Somar valores do Bolsa Família por domicílio
View(bolsa_familia3)

#Criando variável de renda total para os domicílios que recebem Bolsa Família
bolsa_familia4 <- rendimentos %>%
  filter(V9001 %in% c(5400102, 5400101, 5400104, 5400105, 5400106, 5400107)) %>%  # Filtrar quem recebeu Bolsa Família
  select(ID, RENDA_TOTAL) %>%
  group_by(ID) %>%
  distinct() %>%  # Remover duplicatas caso um domicílio tenha múltiplos registros
  summarise(RENDA_TOTAL_FINAL = sum(RENDA_TOTAL, na.rm = TRUE))  # Somar valores do Bolsa Família por domicílio
View(bolsa_familia4)

#Unindo a base de dados de domicílios com as variáveis do Bolsa Familia criadas
domic_BF <- domic %>%
  left_join(bolsa_familia1, by = "ID") %>% 
  mutate(recebe_bolsa = ifelse(is.na(recebe_bolsa), 0, recebe_bolsa))  # Substituir NA por 0 (não recebe Bolsa Família)
View(domic_BF)

domic_BF1 <-  domic_BF %>%
  left_join(bolsa_familia2 %>% select(ID, renda_bolsa), by = "ID")
View(domic_BF1)

domic_BF2 <- domic_BF1 %>%
  left_join(bolsa_familia3 %>% select(ID, prop_BF_tot), by = "ID")
View(domic_BF2)

domic_PBF_final <-  domic_BF2 %>%
  left_join(bolsa_familia4 %>% select(ID, RENDA_TOTAL_FINAL), by = "ID")
View(domic_PBF_final)

#Exportando o banco de dados com as variáveis do Bolsa Familia
#write_xlsx(domic_PBF_final,"domic_PBF_final.xlsx")

#Exibindo a quantidade de domicílios por categoria
table(domic_PBF_final$recebe_bolsa)

#Estimado: 9995 domicílios recebiam o BF na amostra. Ao expandir os resultados: 9685687 recebiam, o que representa 14.07% dos domicílios do país na POF.
#According to PNAD data, 14.6% of the country's households participate in the PBF (which represents 10.069.000 of the 68.862.295 households in the POF).

#Juntando os dados sobre o Bolsa Família com o banco de domicílios inicial
households <- households %>%
  mutate(ID = paste(cod_upa, num_dom, sep = "_"))

households <- households %>%
  left_join(domic_PBF_final %>% select(ID, RENDA_TOTAL_FINAL, recebe_bolsa, renda_bolsa, prop_BF_tot))
View(households)

#Counting households eligible for the Bolsa Familia program
eligibles_available_income <- sum(households$recebe_bolsa > 0)
print(eligibles_available_income)

#Checking the final distribution
households$recebe_bolsa <- as.factor(households$recebe_bolsa)
str(households$recebe_bolsa)

table(households$recebe_bolsa)

plot_PBF = data.frame(table(households$recebe_bolsa))
colnames(plot_PBF) = c("UrbanRural", "recebe_bolsa")
plot_PBF = ggplot(plot_PBF, aes(x = UrbanRural, y = recebe_bolsa))
plot_PBF+
  geom_bar(stat = "identity", fill = "darkorange")+ 
  xlab("Bolsa Família Program")+ylab("Number of Households")+
  theme_classic()+ geom_text(aes(label = recebe_bolsa), vjust = -0.2)+
  scale_y_continuous(breaks=seq(0, 10000, 1))

###Transforming the categorical variables into factors###
households$sexo_chefe_dom <-as.factor(households$sexo_chefe_dom)
levels(households$sexo_chefe_dom) #Reference category: woman
households$sexo_chefe_dom <- relevel(households$sexo_chefe_dom, ref = 2) #Replace reference category to man

households$cor_chefe_dom  <-as.factor(households$cor_chefe_dom)
levels(households$cor_chefe_dom) #Reference category: white OK

households <- households %>%
  mutate(cor_chefe_dom = recode(cor_chefe_dom,
                                "4" = "2",
                                "5" = "4")) #Recoding to unify black and "parda" categories

households$escolaridade  <-as.factor(households$escolaridade)
levels(households$escolaridade) #Reference category:≤ 8 years of study
households$escolaridade <- relevel(households$escolaridade, ref = 3) #Replace reference category to ≥ 12 years of study

households$agua_canal  <-as.factor(households$agua_canal)
levels(households$agua_canal) #Reference category: inadequate
households$agua_canal <- relevel(households$agua_canal, ref = 1) #Replace reference category to adequate
relevel(households$agua_canal, ref = "1")

households$energia_eletrica  <-as.factor(households$energia_eletrica)
levels(households$energia_eletrica)#Reference category: No
households$energia_eletrica <- relevel(households$energia_eletrica, ref = 1) #Replace reference category to Yes
relevel(households$energia_eletrica, ref = "1")

households$regiao  <-as.factor(households$regiao)
levels(households$regiao) #Reference category: North
households$regiao <- relevel(households$regiao, ref = 4) #Replace reference category to South

households$UrbanRural <-as.factor(households$UrbanRural)
levels(households$UrbanRural) #Reference category: Urban
households$UrbanRural <- relevel(households$UrbanRural, ref = "Rural") #Replace reference category to Rural

#Creating a categorical variable for children under 18 in the household
summary(households$menores_18_anos)
households <- households %>%
  mutate(
    menores_18_cat = case_when(
      menores_18_anos <= 0 ~ 1,
      menores_18_anos > 0 & menores_18_anos <= 2 ~ 2,
      menores_18_anos > 2 & menores_18_anos <= 5 ~ 3,
      menores_18_anos > 5 & menores_18_anos <= 8 ~ 4,
      menores_18_anos > 8 ~ 5))
View(households)

households$menores_18_cat <-as.factor(households$menores_18_cat)
levels(households$menores_18_cat) #Reference category: no children OK

#Changing Food Security variable levels to 0 and 1
households$seg_inseg <- factor(households$seg_inseg, 
                                levels = c(1, 2), 
                                labels = c(0, 1)) #0 = Food security; 1 = Food insecurity
str(households$seg_inseg)

write_xlsx(households,"households3.xlsx")

###Obtaining descriptive statistics
table(households$regiao)
table(households$seg_inseg)
table(households$UrbanRural)
table(households$sexo_chefe_dom)
table(households$cor_chefe_dom)
table(households$escolaridade)
table(households$agua_canal)
table(households$energia_eletrica)
table(households$menores_18_cat)

summary(households$idade_chefe_dom)
summary(households$pc_renda_disp)
summary(ifelse(is.na(households$pc_renda_disp), 0, households$pc_renda_disp))
q99 <- quantile(households$pc_renda_disp, 0.99, na.rm = TRUE) #Usando o percentil 99 como corte
summary(households$pc_renda_disp[households$pc_renda_disp <= q99])

summary(households$pc_renda_monet)
summary(ifelse(is.na(households$pc_renda_monet), 0, households$pc_renda_monet))
q99.1 <- quantile(households$pc_renda_monet, 0.99, na.rm = TRUE)
summary(households$pc_renda_monet[households$pc_renda_monet <= q99.1])

summary(households$renda_bolsa)
summary(ifelse(is.na(households$renda_bolsa), 0, households$renda_bolsa))
q99.2 <- quantile(households$renda_bolsa, 0.99, na.rm = TRUE)
summary(households$renda_bolsa[households$renda_bolsa <= q99.2])

summary(households$qtd_total_morador_domic)
summary(households$Densid_esper)

summary(households$HFCpc)
summary(ifelse(is.na(households$HFCpc), 0, households$HFCpc))
q99.3 <- quantile(households$HFCpc, 0.99, na.rm = TRUE)
summary(households$HFCpc[households$HFCpc <= q99.3])

###Obtaining descriptive statistics considering the complexity of the sample###
#Applying the survey method 
options(survey.lonely.psu = "adjust")
design <- svydesign(
  id = ~cod_upa,       #Households ID
  strata = ~estrato_pof,        #Layer
  weights = ~peso_final,          #Sample weights
  data = households,              #Database
  nest = TRUE
)

#Frequency of food secure and food insecure households
svytable(~seg_inseg, design)
#Frequency of households by Bolsa Familia participation status
svytable(~recebe_bolsa, design) #9685687 recebiam o Bolsa Família, o que representa 14.07% dos domicílios do país na POF.

#Total households: 68.862.295
#According to PNADC data, 10.069 million (14.6%) Brazilian households were in the programme in 2018.
#The PNADC estimated that there were 71.0 million households in Brazil in 2018 (which is only 3 per cent more than estimated by the POF), of which 31.0 million were in the Southeast;
#18.5 million in the Northeast; 10.7 million in the South; 5.5 million in the Centre-West; and 5.3 million in the North.

#Frequency of urban and rural households
svytable(~UrbanRural, design)
##Frequency of sex of head of household
svytable(~sexo_chefe_dom, design)
#Frequency of race of head of household
svytable(~cor_chefe_dom, design)
#Schooling of head of household
svytable(~escolaridade, design)
#Access to water
svytable(~agua_canal, design)
#Access to eletricity
svytable(~energia_eletrica, design)
#Region
svytable(~regiao, design)
#Household residents under 18
svytable(~menores_18_cat, design)
#Age of head of household
svymean(~idade_chefe_dom, design, na.rm = TRUE)
confint(svymean(~idade_chefe_dom, design, na.rm = TRUE))
svyquantile(~idade_chefe_dom, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Per capita available income
svymean(~pc_renda_disp, design, na.rm = TRUE)
confint(svymean(~pc_renda_disp, design, na.rm = TRUE))
svyquantile(~pc_renda_disp, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Monetary per capita income
svymean(~pc_renda_monet, design, na.rm = TRUE)
confint(svymean(~pc_renda_monet, design, na.rm = TRUE))
svyquantile(~pc_renda_monet, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Total income (entre aqueles que recebem Bolsa Familia)
svymean(~RENDA_TOTAL_FINAL, design, na.rm = TRUE)
confint(svymean(~RENDA_TOTAL_FINAL, design, na.rm = TRUE))
svyquantile(~RENDA_TOTAL_FINAL, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Renda oriunda do Bolsa Familia
svymean(~renda_bolsa, design, na.rm = TRUE)
confint(svymean(~renda_bolsa, design, na.rm = TRUE))
svyquantile(~renda_bolsa, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Total number of residents in household
svymean(~qtd_total_morador_domic, design, na.rm = TRUE)
confint(svymean(~qtd_total_morador_domic, design, na.rm = TRUE))
svyquantile(~qtd_total_morador_domic, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Household residents under 18
#svymean(~menores_18_anos, design, na.rm = TRUE)
#confint(svymean(~menores_18_anos, design, na.rm = TRUE))
#svyquantile(~menores_18_anos, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))
#Density of establishments to buy healthy food
svymean(~Densid_esper, design, na.rm = TRUE)
confint(svymean(~Densid_esper, design, na.rm = TRUE))
svyquantile(~Densid_esper, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))

resultado_qui <- svychisq(~seg_inseg + UrbanRural, design = design)
print(resultado_qui)

###Models for the base scenario###
#Food security situation predict model
FSL <- svyglm(seg_inseg ~ pc_renda_disp + sexo_chefe_dom + cor_chefe_dom + idade_chefe_dom + escolaridade + 
                qtd_total_morador_domic + menores_18_cat + agua_canal + energia_eletrica + recebe_bolsa + UrbanRural + regiao,
              design = design,
              family = quasibinomial())
print(FSL)

coeficientes <- coef(FSL)
print(coeficientes)
odds_ratios <- exp(coeficientes)
print(odds_ratios)
conf_int <- confint(FSL)
print(conf_int)
conf_int_or <- exp(conf_int)
print(conf_int_or)
p_valores <- summary(FSL)$coefficients[, "Pr(>|t|)"]
print(p_valores)

results <- data.frame(
  Variable = rownames(summary(FSL)$coefficients),
  Coef = coeficientes,
  Odds_Ratios = odds_ratios,
  CI_lower = conf_int_or[, 1],
  IC_upper = conf_int_or[, 2],
  p_value = p_valores
)

print(results)
write_xlsx(results, path = "results-base-model3.xlsx")

#Creating the healthy food consumption variable (HFC) from the sum of the food groups that make up the PHDI adequacy group
households <- households %>%
  mutate(HFC =nozes_amendoim_gr + leguminosas_gr + frutas_gr + outros_vegetais_gr + vegetais_verdes_gr + vegetais_vermelhos_gr 
         + cereais_integrais_gr)
View(households)
plot(households$HFC)
hist(households$HFC)

##Per capita
qtd_total_morador_domic
households <- households %>%
  mutate(HFCpc = HFC/qtd_total_morador_domic)
View(households)
plot(households$HFCpc)
hist(households$HFCpc)

##Descriptive
svymean(~HFCpc, design, na.rm = TRUE)
confint(svymean(~HFCpc, design, na.rm = TRUE))
svyquantile(~HFCpc, design, na.rm = TRUE, quantiles = c(0.25, 0.5, 0.75))

#Creating the price variable


#Creating the healthy food consumption prediction model
modelo_consumo <- lm(HFC ~ seg_inseg + Densid_esper + recebe_bolsa + regiao,
                     data = households, design = design) #Verificar o protocolo
print(modelo_consumo)

coeficientes2 <- coef(modelo_consumo)
print(coeficientes2)
odds_ratios2 <- exp(coeficientes2)
print(odds_ratios2)
conf_int2 <- confint(modelo_consumo)
print(conf_int2)
conf_int_or2 <- exp(conf_int2)
print(conf_int_or2)
p_valores2 <- summary(modelo_consumo)$coefficients[, "Pr(>|t|)"]
print(p_valores2)

results2 <- data.frame(
  Variable = rownames(summary(modelo_consumo)$coefficients),
  Coef = coeficientes2,
  CI_lower = conf_int2[, 1],
  IC_upper = conf_int2[, 2],
  p_value = p_valores2
)
print(results2)
write_xlsx(results2, path = "results-consumption-model-prev2.xlsx")

