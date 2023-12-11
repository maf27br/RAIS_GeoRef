---
title: "PROJETO APL's COM RAIS 2019"
Código: "Marcio Freire"
Artigo: "Manoel Fernandes Silva Souza"
Orientador(es): "Marco Antonio Pinheiro da Silveira e Juciê de Sousa Almeida"

date: "16/12/2021"
---

#   Introdução
Código de suporte ao artigo/projeto de análise de APLs (arranjos produtivos locais) apoiadas pelo SDE no estado de SP, de acordo com a planilha enviada.
Este projeto pretendo importar os microdados da RAIS 2019, tratar os dados, filtrar os registros de empresas do estado de SP e estabelecer os grupos de CNAEs de acordo com os APLs, calculando os dados como IE (índice de especialização do município) e QL ().
Após a extração e tratamento, os dados foram exportados e utilizados no QGis/GeoDA para mapas temáticos.

#   RAIS - Importação e Tratamento 
Os microdados da RAIS 2019 podem ser obtidos diretamente em ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/.
O processo de importação e base do projeto foi obtido em: http://cemin.wikidot.com/raisr

##    Importação dos dados das empresas
**Carga de Bibliotecas utilizadas**

```{r message=FALSE, warning=FALSE}
library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(gridExtra)
```
Importação da base de estabelecimentos:
```{r message=FALSE, warning=FALSE}
md_rais_estab_full = read_delim("./raw-data/RAIS_ESTAB_PUB.txt",";",escape_double = FALSE,  
                              locale = locale(encoding = "ISO-8859-1"), 
                              trim_ws = TRUE)
```
A descodificação das variáveis Natureza Jurídica, Tipo de Estabelecimento e SubSetor do IBGE foram feitas com a exportação do dicionário de dados e posterior aplicação na base.
```{r message=FALSE, warning=FALSE}
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

```
Realizando a carga de municipios:
```{r}
cad_municipios_data = readRDS(file = "./data/cad_municipios_data.rds")
```
Excluindo colunas desnecessárias:
```{r}
md_rais_estab = md_rais_estab_full %>% select(-c(1,2,3,5, 6, 17))
dim(md_rais_estab)
```
Possibilitando a junção com o cadastro de municípios através de COD_MUN_RAIS:
```{r}
cad_municipios_data = cad_municipios_data %>% 
  mutate(COD_MUN_RAIS = as.numeric(str_sub(as.character(CO_MUNICIPIO), 1, 6)))
```
Tranformação de códigos em descritivos, arranjo dos dados e nomeação de colunas:
```{r}
md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  mutate(Qtd_Vinc_CLT = as.numeric(md_rais_estab$`Qtd Vínculos CLT`)) %>% 
  mutate(Qtd_Vinc_Ativos = as.numeric(md_rais_estab$`Qtd Vínculos Ativos`)) %>% 
  mutate(Qtd_Vinc_Estat = as.numeric(md_rais_estab$`Qtd Vínculos Estatutários`)) %>% 
  mutate(Natureza_Juridica = as.numeric(md_rais_estab$`Natureza Jurídica`)) %>% 
  mutate(TpEstab = as.numeric(md_rais_estab$`Tipo Estab...20` )) %>% 
  mutate(IbgeSubSetor = as.numeric(md_rais_estab$`IBGE Subsetor`)) 

md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  left_join(select(cad_municipios_data, CO_MUNICIPIO, SGL_UF, DS_MESORREGIAO, DS_MICRORREGIAO, NOME_MUNICIPIO, LATITUDE, LONGITUDE, COD_MUN_RAIS), by = c("Município" = "COD_MUN_RAIS")) 

md_rais_estab = md_rais_estab %>% select(everything()) %>% 
  left_join(md_rais_estab_natjur, by = c("Natureza_Juridica" = "cod_nat_jur")) %>% 
  left_join(md_rais_estab_tpestab, by = c("TpEstab" = "co_tpestab")) %>% 
  left_join(md_rais_estab_ibge_subsetor, by = c("IbgeSubSetor" = "cod_ibge_subsetor"))

dim(md_rais_estab)
```
Removendo as colunas não mais necessárias:
```{r}
md_rais_estab = md_rais_estab %>% select(-c(2,3,4,11,14,15,17))
dim(md_rais_estab)
```
Salvando a base para uso posterior e removendo as variáveis não mais utilizadas:
```{r}
saveRDS(md_rais_estab, file = "./data/md_rais_estab.rds")
rm (md_rais_estab)
rm (cad_municipios_data)
rm (md_rais_estab_natjur)
rm (md_rais_estab_tpestab)
rm (md_rais_estab_ibge_subsetor)
```


##    RAIS - Tratamento e Exportação
Esta etapa do código é responsável por tratar a base de dados bruta, filtrar e realizar a amostra e exportar os dados para uso no QGis e GeoDa.
Para auxílio no mapeamento, são incorporados os dados de IDH de todos os municípios.

**Carga de Bibliotecas utilizadas**

```{r message=FALSE, warning=FALSE}
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
```
Importação dos dados da PNUD-IDH
```{r}
idh_muni = pnud_muni %>% filter(ufn == 'São Paulo', ano == 2010) %>% select(c(4,5, 6, 76, 92, 230, 234, 235,236, 237))
dim (idh_muni)
```
Abrindo base de dados da rais importado anteriormente, já filtrando o estado de SP:
```{r}
md_rais_completo = readRDS(file = "./data/md_rais_estab.rds")
md_rais_SP = md_rais_completo %>% filter(SGL_UF == 'SP')
rm(md_rais_completo)
```
Configurando os filtros gerais de exportação:
```{r}
ATIVOS = c(0,1,9) # 0 = Sem atvidade e 1 = com Atividade
MinFunc = 0
```

###   Exportação de dados por grupos de CNAEs

####    001-Móveis de Madeira
```{r}
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

```

####    002-Apicultura
```{r}
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

```
####    003-Banana
```{r}
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

```
####    004-Cafeicultura
```{r}
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

```
####    005-Calçados
```{r}
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
```
####    006-Cervejas Artesanais
```{r}
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
```
####    007-Ferramentaria
```{r}
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

```

####    008-Joalheiro
```{r}
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

```

####    009-Leite e Derivados
```{r}
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

```

####    010-Mobiliário
```{r}
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

```

####    011-Cerâmica de Revestimento
```{r}
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

```

####    012-AeroEspacial
```{r}
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

```


####    013-Limão Tahiti
```{r}
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
```


####    014-Têxtil e Confecção
```{r}
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

```
#### Agrupamento totalizador da amostra (para IE/QL):
```{r}
#FILTRO COM RESULTADO ANALITICO GERAL
  Geral_analitico = md_rais_SP
  Geral_agrupado = Geral_analitico %>%
    select (CO_MUNICIPIO, Qtd_Vinc_Ativos) %>%
    group_by(CO_MUNICIPIO) %>%
    summarise(Geral_QTD_EMP = n(), Geral_QTD_VINC = sum(Qtd_Vinc_Ativos))

```

###   Junção e PNUD
Nesta etapa, fazemos a junção entre as bases dos grupos CNAEs coletados com a base PNUD por municípios.
```{r}
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

```
Salvando a base de dados:
```{r}
saveRDS(Dados_PNUD_RAIS, file = "./data/Dados_PNUD_RAIS.rds")  
```
Remoção de todas as variáveis utilizadas:
```{r}
rm(G01_agrupado)
rm(G01_analitico)
rm(G02_agrupado)
rm(G02_analitico)
rm(G03_agrupado)
rm(G03_analitico)
rm(G04_agrupado)
rm(G04_analitico)
rm(G05_agrupado)
rm(G05_analitico)
rm(G06_agrupado)
rm(G06_analitico)
rm(G07_agrupado)
rm(G07_analitico)
rm(G08_agrupado)
rm(G08_analitico)
rm(G09_agrupado)
rm(G09_analitico)
rm(G10_agrupado)
rm(G10_analitico)
rm(G11_agrupado)
rm(G11_analitico)
rm(G12_agrupado)
rm(G12_analitico)
rm(G13_agrupado)
rm(G13_analitico)
rm(G14_agrupado)
rm(G14_analitico)
rm (Geral_agrupado)
rm(Geral_analitico)
rm (Dados_PNUD_RAIS)
rm (md_rais_SP)
rm(md_rais_completo)
rm(md_rais_estab)
rm(md_rais_estab_ibge_subsetor)
rm(md_rais_estab_tpestab)
rm(md_rais_estab_natjur)
rm(md_rais_estab_full)
rm(cad_municipios_data)

```

##    Visualizações da amostra

