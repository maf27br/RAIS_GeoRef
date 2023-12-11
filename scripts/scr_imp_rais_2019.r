#********************************************************
#***************  PROJETO APL's COM RAIS 2019   *********
#**********    MARCIO FREIRE      24/08/2021    *********
#********** CARGA E TRATAMENTO DOS DADOS BRUTOS *********
#********   MICRODADOS RAIS 2019                *********
#********************************************************
setwd("E:\\MESTRADO\\#2.APL_RAIS")
getwd()
library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(gridExtra)

#LEITURA DA BASE PADRÃO - ESTABELECIMENTOS
md_rais_estab_full = read_delim("./raw-data/RAIS_ESTAB_PUB.txt",";",escape_double = FALSE,  
                              locale = locale(encoding = "ISO-8859-1"), 
                              trim_ws = TRUE)

#LEITURA DA BASE PADRÃO - NATUREZAS JURIDICAS
md_rais_estab_natjur = read_delim("./raw-data/Nat_juridicas.csv",",",escape_double = FALSE,  
                                locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)

#LEITURA DA BASE PADRÃO - Tipo Estabelecimento
md_rais_estab_tpestab = read_delim("./raw-data/TpEstab.csv",",",escape_double = FALSE,  
                                  locale = locale(encoding = "ISO-8859-1"), 
                                  trim_ws = TRUE)

#LEITURA DA BASE PADRÃO - Tipo Estabelecimento
md_rais_estab_ibge_subsetor = read_delim("./raw-data/Ibge_SubSetor.csv",";",escape_double = FALSE, 
                                   trim_ws = TRUE)



#SALVANDO E LIBERANDO VARIAVEIS
saveRDS(md_rais_estab_full, file = "./data/md_rais_estab_full.rds")
rm (md_rais_estab_full)
gc()
#LEITURA DA BASE PADRÃO - VINCULOS
md_rais_vinc_full = read_delim("./raw-data/RAIS_VINC_PUB_SP.txt",";",escape_double = FALSE,  
                                locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)
#SALVANDO E LIBERANDO VARIAVEIS
saveRDS(md_rais_vinc_full, file = "./data/md_rais_vinc_full.rds")
rm (md_rais_vinc_full)

#CARGA DE DADOS
md_rais_estab_full = readRDS(file = "./data/md_rais_estab_full.rds")
cad_municipios_data = readRDS(file = "./data/cad_municipios_data.rds")
#TRATAMENTO DADOS ESTABELECIMENTOS
md_rais_estab = readRDS(file = "./data/md_rais_estab_full.rds") %>% select(-c(1,2,3,5, 6, 17))
#adicionando os dados dos municipios
cad_municipios_data = cad_municipios_data %>% 
  mutate(COD_MUN_RAIS = as.numeric(str_sub(as.character(CO_MUNICIPIO), 1, 6)))

#TRANSFORMANDO CODIGOS EM DESCRITIVOS (TABELA VARIAVEIS) e ARRANJANDO OS DADOS
md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  mutate(Qtd_Vinc_CLT = as.numeric(md_rais_estab$`Qtd Vínculos CLT`)) %>% 
  mutate(Qtd_Vinc_Ativos = as.numeric(md_rais_estab$`Qtd Vínculos Ativos`)) %>% 
  mutate(Qtd_Vinc_Estat = as.numeric(md_rais_estab$`Qtd Vínculos Estatutários`)) %>% 
  mutate(Natureza_Juridica = as.numeric(md_rais_estab$`Natureza Jurídica`)) %>% 
  mutate(TpEstab = as.numeric(md_rais_estab$`Tipo Estab...20`)) %>% 
  mutate(IbgeSubSetor = as.numeric(md_rais_estab$`IBGE Subsetor`)) 

md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  left_join(select(cad_municipios_data, CO_MUNICIPIO, SGL_UF, DS_MESORREGIAO, DS_MICRORREGIAO, NOME_MUNICIPIO, LATITUDE, LONGITUDE, COD_MUN_RAIS), by = c("Município" = "COD_MUN_RAIS")) 

md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  left_join(md_rais_estab_natjur, by = c("Natureza_Juridica" = "cod_nat_jur")) %>% 
  left_join(md_rais_estab_tpestab, by = c("TpEstab" = "co_tpestab")) %>% 
  left_join(md_rais_estab_ibge_subsetor, by = c("IbgeSubSetor" = "cod_ibge_subsetor"))


#REMOVENDO COLUNAS DESNECESSÁRIAS
md_rais_estab = md_rais_estab %>% select(-c(2,3,4,11,14,15,17))


#SALVANDO E LIBERANDO VARIAVEIS
saveRDS(md_rais_estab, file = "./data/md_rais_estab.rds")
rm (md_rais_estab)
rm (cad_municipios_data)
rm (md_rais_estab_natjur)
rm (md_rais_estab_tpestab)
rm (md_rais_estab_ibge_subsetor)
gc()
