#********************************************************
#***************  PROJETO APL's COM RAIS 2019   *********
#**********    MARCIO FREIRE      24/08/2021    *********
#********** EXPORTAÇÃO E GERAÇÃO DE DADOS      *********
#********   MICRODADOS RAIS 2019                *********
#********************************************************
#*
#*
setwd("E:\\MESTRADO\\#2.APL_RAIS")
getwd()
# install.packages("remotes")
# remotes::install_github("abjur/abjData")

library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(gridExtra)
library(abjData)
library(clipr)
#dados do IDH por municipio de SP

idh_muni = pnud_muni %>% filter(ufn == 'São Paulo', ano == 2010) %>% select(c(4,5, 6, 42,50,116,120,121,122,123))

#importação rais
md_rais_completo = readRDS(file = "./data/md_rais_estab.rds")
cad_municipios_data = readRDS("./Data/cad_municipios_data.rds")
#EXPORTANDO CNAE - FILTRO SOMENTE UF = SP
md_rais_SP = md_rais_completo %>% filter(SGL_UF == 'SP')
rm(md_rais_completo)
gc()
#EXPORTANDO CNAE - FILTROS
ATIVOS = c(0,1,9) # 0 = Sem atvidade e 1 = com Atividade
MinFunc = 0

#===== 001-Móveis de Madeira ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G01 = c('31012','33295','20622')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G01_analitico = if (length(CNAE_G01)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G01, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G01_analitico, file = "001_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "001_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G01_agrupado = G01_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G01_QTD_EMP = n(), G01_QTD_VINC = sum(Qtd_Vinc_Ativos))
write.csv(G01_agrupado, file = "001_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G01 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G01_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G01 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G01[is.na(mapa_agrup_G01)] = 0
write.csv(mapa_agrup_G01, file = "QGIS_mapa_agrup_001.csv")

#===== 001-Móveis de Madeira - V02 - 11/04/22 ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G01 = c('13529','16102','16218','16226','16234','16293','31012','31047')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G01_analitico = if (length(CNAE_G01)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G01, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
clipr::write_clip(mapa_agrup_G01)
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G01_analitico, file = "001_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "001_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G01_agrupado = G01_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(G01_QTD_EMP = n(), G01_QTD_VINC = sum(Qtd_Vinc_Ativos))
write.csv(G01_agrupado, file = "001_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G01 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G01_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G01 = mapa_agrup_G01 %>% select(9,10,14)
mapa_agrup_G01[is.na(mapa_agrup_G01)] = 0
write.csv(mapa_agrup_G01, file = "QGIS_mapa_agrup_001.csv")




#===== 002-Apicultura ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G02 = c('3314722','3314712','3314799','3314710','3314702','4661300','3314711','3314705','3314707',
             '3314718','3314703','0159802','3314721','3314713','2833000','3314706','3314719','3314717',
             '3314709','3314716','3314704','3314701','3314708','0159801','0159899','3314720','3314715',
             '3314714','0159804','0159803')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G02_analitico = if (length(CNAE_G02)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Subclasse` , 1, 7) %in% CNAE_G02, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G02_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G02_agrupado = G02_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G02_QTD_EMP = n(), G02_QTD_VINC = sum(Qtd_Vinc_Ativos))

write.csv(G02_agrupado, file = "002_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G02 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G02_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G02 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G02[is.na(mapa_agrup_G02)] = 0
write.csv(mapa_agrup_G02, file = "QGIS_mapa_agrup_002.csv")

#===== 003-Banana ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G03 = c('0133402')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G03_analitico = if (length(CNAE_G03)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Subclasse` , 1, 7) %in% CNAE_G03, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G03_analitico, file = "003_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G03_agrupado = G03_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G03_QTD_EMP = n(), G03_QTD_VINC = sum(Qtd_Vinc_Ativos))

write.csv(G03_agrupado, file = "003_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G03 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G03_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G03 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G03[is.na(mapa_agrup_G03)] = 0
write.csv(mapa_agrup_G03, file = "QGIS_mapa_agrup_003.csv")

#===== 004-Cafeicultura ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G04 = c('0134200', '1081301')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G04_analitico = if (length(CNAE_G04)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Subclasse` , 1, 7) %in% CNAE_G04, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G04_analitico, file = "004_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G04_agrupado = G04_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G04_QTD_EMP = n(), G04_QTD_VINC = sum(Qtd_Vinc_Ativos))

write.csv(G04_agrupado, file = "004_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G04 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G04_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G04 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G04[is.na(mapa_agrup_G04)] = 0
write.csv(mapa_agrup_G04, file = "QGIS_mapa_agrup_004.csv")

#===== 005-Calçados ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G05 = c('4782201','4773300','1531901','1540800','3321000','4616800','4643501','7723300','4689399',
             '4785799','2062200','1539400','9529101','9609299','4645102','1531902','1629301','1533500',
             '3250703','2864000','3314720','3292201')

#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G05_analitico = if (length(CNAE_G05)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Subclasse` , 1, 7) %in% CNAE_G05, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G05_analitico, file = "005_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G05_agrupado = G05_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G05_QTD_EMP = n(), G05_QTD_VINC = sum(Qtd_Vinc_Ativos))
write.csv(G05_agrupado, file = "005_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G05 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G05_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G05 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G05[is.na(mapa_agrup_G05)] = 0
write.csv(mapa_agrup_G05, file = "QGIS_mapa_agrup_005.csv")

#===== 006-Cervejas Artesanais ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G06 = c('10996')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G06_analitico = if (length(CNAE_G06)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G06, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G06_analitico, file = "006_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G06_agrupado = G06_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G06_QTD_EMP = n(), G06_QTD_VINC = sum(Qtd_Vinc_Ativos))
write.csv(G06_agrupado, file = "006_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G06 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G06_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G06 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G06[is.na(mapa_agrup_G06)] = 0
write.csv(mapa_agrup_G06, file = "QGIS_mapa_agrup_006.csv")

#===== 007-Ferramentaria ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G07 = c('25438')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G07_analitico = if (length(CNAE_G07)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G07, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP

G07_agrupado = G07_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G07_QTD_EMP = n(), G07_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G07_analitico, file = "007_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G07_agrupado = G07_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G07_agrupado, file = "007_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G07 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G07_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G07 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G07[is.na(mapa_agrup_G07)] = 0
write.csv(mapa_agrup_G07, file = "QGIS_mapa_agrup_007.csv")

#===== 008-Joalheiro ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G08 = c('3211602')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G08_analitico = if (length(CNAE_G08)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Subclasse` , 1, 7) %in% CNAE_G08, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
G08_agrupado = G08_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G08_QTD_EMP = n(), G08_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G08_analitico, file = "008_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G08_agrupado = G08_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G08_agrupado, file = "008_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G08 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G08_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G08 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G08[is.na(mapa_agrup_G08)] = 0
write.csv(mapa_agrup_G08, file = "QGIS_mapa_agrup_008.csv")

#===== 009-Leite e Derivados ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G09 = c('10520')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G09_analitico = if (length(CNAE_G09)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G09, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP

G09_agrupado = G09_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G09_QTD_EMP = n(), G09_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G09_analitico, file = "009_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G09_agrupado = G09_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G09_agrupado, file = "009_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G09 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G09_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G09 = mapa_agrup_G09 %>% select(9,10,14)
mapa_agrup_G09[is.na(mapa_agrup_G09)] = 0
write.csv(mapa_agrup_G09, file = "QGIS_mapa_agrup_009.csv")

#===== 010-Mobiliário ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G10 = c('32507','31047')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G10_analitico = if (length(CNAE_G10)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G10, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
G10_agrupado = G10_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G10_QTD_EMP = n(), G10_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G10_analitico, file = "010_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G10_agrupado = G10_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G10_agrupado, file = "010_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G10 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G10_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G10 = mapa_agrup_G10 %>% select(9,10,14)
mapa_agrup_G10[is.na(mapa_agrup_G10)] = 0
write.csv(mapa_agrup_G10, file = "QGIS_mapa_agrup_010.csv")

#===== 011-Cerâmica de Revestimento ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G11 = c('23427')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G11_analitico = if (length(CNAE_G11)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G11, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
G11_agrupado = G11_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G11_QTD_EMP = n(), G11_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G11_analitico, file = "011_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G11_agrupado = G11_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G11_agrupado, file = "011_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G11 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G11_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G11 = mapa_agrup_G11 %>% select(9,10,14)
mapa_agrup_G11[is.na(mapa_agrup_G11)] = 0
write.csv(mapa_agrup_G11, file = "QGIS_mapa_agrup_011.csv")


#===== 012-AeroEspacial ======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G12 = c('30920','30423','30121','30326','30911','30415','30997','30318','30113','30504')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G12_analitico = if (length(CNAE_G12)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G12, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP
G12_agrupado = G12_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G12_QTD_EMP = n(), G12_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G12_analitico, file = "012_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G12_agrupado = G12_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G12_agrupado, file = "012_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G12 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G12_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G12 = mapa_agrup_G12 %>% select(9,10,14)
mapa_agrup_G12[is.na(mapa_agrup_G12)] = 0
write.csv(mapa_agrup_G12, file = "QGIS_mapa_agrup_012.csv")


#===== 013-Limão Tahiti======
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G13 = c('46125')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G13_analitico = if (length(CNAE_G13)>=1)
  md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G13, 
                        md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                        md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                          md_rais_SP

G13_agrupado = G13_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G13_QTD_EMP = n(), G13_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G13_analitico, file = "013_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G13_agrupado = G13_analitico %>%
  select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
  summarise(QTD = n())
write.csv(G13_agrupado, file = "013_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G13 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
  left_join(G13_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G13 = mapa_agrup_G13 %>% select(9,10,14)
mapa_agrup_G13[is.na(mapa_agrup_G13)] = 0
write.csv(mapa_agrup_G13, file = "QGIS_mapa_agrup_013.csv")


#===== 014-Têxtil e Confecção =====
#COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
CNAE_G14 = c('13529','13596', '14118', '14126', '14134', '14142')
#COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
CIDADES = c()
#FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
G14_analitico = if (length(CNAE_G14)>=1)
                                md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G14, 
                                                             md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                                                             md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                                md_rais_SP
G14_agrupado = G14_analitico %>%
  select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(G14_QTD_EMP = n(), G14_QTD_VINC = sum(Qtd_Vinc_Ativos))

#GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
write.csv(G14_analitico, file = "014_full.csv")
#FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
# md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
#                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
#                                 md_rais_SP_CNAE_analitico
# write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
#FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
G14_agrupado = G14_analitico %>%
                           select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
                           group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
                           summarise(QTD = n())
write.csv(G14_agrupado, file = "014_agrup.csv")

#necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
mapa_agrup_G14 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
    left_join(G14_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
mapa_agrup_G14 = mapa_agrup_G14 %>% select(9,10,14)
mapa_agrup_G14[is.na(mapa_agrup_G14)] = 0
  write.csv(mapa_agrup_G14, file = "QGIS_mapa_agrup_014.csv")

  #===== 015-Extração Mineral =====
  #COLOQUE ABAIXO A LISTA DE CNAEs A EXPORTAR, SEPARADOS POR VIRGULA (,) E ENTRE ASPAS (SOMENTE NÚMEROS)
  CNAE_G15 = c('09904', '23991','08100')
  #COLOQUE ABAIXO A LISTA DE CIDADES A EXPORTAR, SEPARADOS POR VIRGULA (,) E SOMENTE NÚMEROS
  CIDADES = c()
  #FILTRO COM RESULTADO ANALITICO DOS CNAES SELECIONADOS
  G15_analitico = if (length(CNAE_G15)>=1)
    md_rais_SP %>% filter(substr(md_rais_SP$`CNAE 2.0 Classe` , 1, 5) %in% CNAE_G15, 
                          md_rais_SP$`Ind Atividade Ano` %in% ATIVOS,
                          md_rais_SP$Qtd_Vinc_Ativos >= MinFunc) else
                            md_rais_SP
  G15_analitico = G15_analitico %>% filter(G15_analitico$`CNAE 2.0 Subclasse` %in% c('0990401','0990402','0990403', '2399199','0810099'))
  G15_agrupado = G15_analitico %>%
    select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
    group_by(CO_MUNICIPIO) %>%
    summarise(G15_QTD_EMP = n(), G15_QTD_VINC = sum(Qtd_Vinc_Ativos))
  
  #GRAVA DADOS PARA IMPORTAÇÃO PLANILHA
  write.csv(G15_analitico, file = "015_full.csv")
  #FILTRO COM RESULTADO ANALITICO DOS CNAES E CIDADES SELECIONADOS
  # md_rais_SP_CNAE_analitico = if (length(CIDADES)>=1)
  #                                 md_rais_SP_CNAE_analitico %>% filter(md_rais_SP_CNAE_analitico$CO_MUNICIPIO %in% CIDADES) else
  #                                 md_rais_SP_CNAE_analitico
  # write.csv(md_rais_SP_CNAE_analitico, file = "002_full.csv")
  #FILTRO COM RESULTADO AGRUPADO DOS CNAES E CIDADES SELECIONADOS
  G15_agrupado = G15_analitico %>%
    select (CO_MUNICIPIO, NOME_MUNICIPIO) %>%
    group_by(CO_MUNICIPIO, NOME_MUNICIPIO) %>%
    summarise(QTD = n())
  write.csv(G15_agrupado, file = "015_agrup.csv")
  
  #necessário exportar todos os CNAEs encontrados, agrupado por municipio (0 sem dados) para QGIS
  mapa_agrup_G15 = cad_municipios_data %>% filter(SGL_UF == 'SP') %>% 
    left_join(G15_agrupado, QTD, by = c("CO_MUNICIPIO" = "CO_MUNICIPIO"))
  mapa_agrup_G15 = mapa_agrup_G15 %>% select(9,10,14)
  mapa_agrup_G15[is.na(mapa_agrup_G15)] = 0
  write.csv(mapa_agrup_G15, file = "QGIS_mapa_agrup_015.csv")
  
#FILTRO COM RESULTADO ANALITICO GERAL
  Geral_analitico = md_rais_SP
  Geral_agrupado = Geral_analitico %>%
    select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
    group_by(CO_MUNICIPIO) %>%
    summarise(Geral_QTD_EMP = n(), Geral_QTD_VINC = sum(Qtd_Vinc_Ativos))
  
#===== JUNÇÃO COM PNUD ========

Dados_PNUD_RAIS = idh_muni %>% 
    left_join(G01_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>% 
    left_join(G02_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G03_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G04_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G05_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G06_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G07_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G08_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G09_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G10_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G11_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G12_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G13_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>%
    left_join(G14_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>% 
    left_join(Geral_agrupado, by = c("codmun7" = "CO_MUNICIPIO"))
Dados_PNUD_RAIS[is.na(Dados_PNUD_RAIS)] = 0
write.csv(Dados_PNUD_RAIS, file = "Dados_PNUD_RAIS.csv")  
saveRDS(Dados_PNUD_RAIS, file = "./data/Dados_PNUD_RAIS.rds")  
  
Dados_PNUD_RAIS = idh_muni %>% 
  left_join(G15_agrupado, by = c("codmun7" = "CO_MUNICIPIO")) %>% 
  left_join(Geral_agrupado, by = c("codmun7" = "CO_MUNICIPIO"))
Dados_PNUD_RAIS[is.na(Dados_PNUD_RAIS)] = 0  

write_clip(Dados_PNUD_RAIS)
rm(G01_analitico)
rm(G01_agrupado)


