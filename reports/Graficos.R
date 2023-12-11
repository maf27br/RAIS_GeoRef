#**********    MARCIO FREIRE      24/08/2021    *********
#********** VISUALIZAÇÃO DE DADOS      *********
#********   MICRODADOS RAIS 2019                *********
#********************************************************
#*
#*
#*#==mapa de tamanho funcionarios, por estado ===========================================
#
#install.packages('geobr', dependencies = T)
#install.packages('esquisse')
library(esquisse)


library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(gridExtra)
library(geobr)]

# LOAD
md_rais_SP_CNAE30 = readRDS(file = "./data/md_rais_SP_CNAE30.rds")
md_rais_SP_CNAE31 = readRDS(file = "./data/md_rais_SP_CNAE31.rds")

geo_ufs = geobr::read_state(code_state = 'all', year = 2018)
metadata = geobr::download_metadata() # para ver codigos
head(metadata)
municipalidadesSP = geobr::read_municipality(code_muni=35, year=2020) 

ds_qtd_30 = md_rais_SP_CNAE30 %>%
  select (NOME_MUNICIPIO, CO_MUNICIPIO) %>%
  group_by(NOME_MUNICIPIO,CO_MUNICIPIO) %>%
  summarize(QTD = n())

ds_qtd_31 = md_rais_SP_CNAE31 %>%
  select (NOME_MUNICIPIO, CO_MUNICIPIO) %>%
  group_by(NOME_MUNICIPIO,CO_MUNICIPIO) %>%
  summarize(QTD = n())
# LOAD DIRETO
geo_munSP_qtd_30 = readRDS(file = "./data/geo_munSP_qtd_30.rds")
geo_munSP_qtd_31 = readRDS(file = "./data/geo_munSP_qtd_31.rds")

# geo_munSP_qtd_30 = left_join(municipalidadesSP, ds_qtd_30, by = c('code_muni' = 'CO_MUNICIPIO'))
# geo_munSP_qtd_31 = left_join(municipalidadesSP, ds_qtd_31, by = c('code_muni' = 'CO_MUNICIPIO'))
# saveRDS(geo_munSP_qtd_30, file = "./data/geo_munSP_qtd_30.rds")
# saveRDS(geo_munSP_qtd_31, file = "./data/geo_munSP_qtd_31.rds")
# geo_munSP_qtd_30 = geo_munSP_qtd_30 %>% select(-c(8))
# geo_munSP_qtd_30[is.na(geo_munSP_qtd_30)]=0
#por quantidade - 30

ggplot() +
  geom_sf(data=geo_munSP_qtd_30, aes(fill=QTD), color= NA, size=.15)+
  labs(title="QUANTIDADE DE CNAEs 30 POR MUNICIPIO",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Greens", limits=c(1, 300),
                       name="Code_muni")+
  theme_minimal()

#por quantidade - 31
ggplot() +
  geom_sf(data=geo_munSP_qtd_31, aes(fill=QTD), color= NA, size=.15)+
  labs(title="QUANTIDADE DE CNAEs 31 POR MUNICIPIO",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Reds", limits=c(1, 1200), name="code_muni")+
  theme_minimal()


# MODELO LEAFLET
ds_qtd_30_LF = md_rais_SP_CNAE30 %>%
  select (NOME_MUNICIPIO, CO_MUNICIPIO, LATITUDE, LONGITUDE) %>%
  group_by(NOME_MUNICIPIO,CO_MUNICIPIO, LATITUDE, LONGITUDE) %>%
  summarize(QTD = n())

ds_qtd_31_LF = md_rais_SP_CNAE31 %>%
  select (NOME_MUNICIPIO, CO_MUNICIPIO, LATITUDE, LONGITUDE) %>%
  group_by(NOME_MUNICIPIO,CO_MUNICIPIO, LATITUDE, LONGITUDE) %>%
  summarize(QTD = n())
# install.packages("leaflet")
library(leaflet)
library (sf)

#exemplos
# mapa = leaflet() %>% 
#   addTiles() %>%  # Adicionando um recorte de mapa do OpenStreetMap
#   addMarkers(lng=-43.940925, lat=-19.929799, popup="Oper")
# mapa  # Imprimindo o mapa
# mapa = brazilmaps::get_brmap("State")
# mapa = read_state(showProgress = FALSE)
# acesso_san = data.frame(code_state = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15, 
#                                         25, 41, 26, 22, 33, 24, 43, 11, 14, 42, 35, 28, 17), 
#                          com_rede = c(0.273, 0.412, 0.313, 0.177, 0.513, 0.696, 1.000, 0.974, 0.280, 0.065, 
#                                       0.191, 0.449, 0.916, 0.063, 0.731, 0.421, 0.881, 0.045, 0.924, 0.353, 
#                                       0.405, 0.096, 0.400, 0.352, 0.998, 0.347, 0.129))
# coord_pontos = mapa %>% 
#   left_join(acesso_san, by = "code_state") %>% 
#   mutate(com_rede = 100*com_rede) %>% 
#   st_centroid()
# 
# dados = data.frame(st_coordinates(coord_pontos), 
#                     com_rede = coord_pontos$com_rede, 
#                     UF = coord_pontos$name_state)
# leaflet(dados) %>% 
#   addTiles() %>%
#   addCircleMarkers(~ X, ~ Y,
#                    label = ~ as.character(paste0(UF, ": ", com_rede, "%")),
#                    labelOptions = labelOptions(textsize = "13px"),
#                    radius = ~ sqrt(com_rede),
#                    fillOpacity = 0.5)
#---MAPA CNAE 30
mapa_SP = brazilmaps::get_brmap("City", geo.filter = list(State = 35))

ds_qtd_30 = ds_qtd_30 %>% drop_na(QTD)

coord_pontos_30 = mapa_SP %>% 
  left_join(ds_qtd_30,  by = c("City" = "CO_MUNICIPIO")) %>% 
  st_centroid()
coord_pontos_30 = coord_pontos_30 %>% drop_na(QTD)
dados_30 = data.frame(st_coordinates(coord_pontos_30), 
                   QTD = coord_pontos_30$QTD, 
                   Cidade = coord_pontos_30$nome)

leaflet(dados_30) %>% 
  addTiles() %>%
  addCircleMarkers(~ X, ~ Y,
                   label = ~ as.character(paste0(Cidade, ": ", QTD)),
                   labelOptions = labelOptions(textsize = "13px"),
                   radius = ~ sqrt(QTD),
                   fillOpacity = 0.5)
#---MAPA CNAE 31
mapa_SP = brazilmaps::get_brmap("City", geo.filter = list(State = 35))
#mapa_SP = read_state(showProgress = FALSE)

ds_qtd_31 = ds_qtd_31 %>% filter (QTD > 3)
coord_pontos_31 = mapa_SP %>% 
  left_join(ds_qtd_31,  by = c("City" = "CO_MUNICIPIO")) %>% 
  st_centroid()

dados_31 = data.frame(st_coordinates(coord_pontos_31), 
                      QTD = coord_pontos_31$QTD, 
                      Cidade = coord_pontos_31$nome)
dados_31 = dados_31 %>% drop_na(QTD)
leaflet(dados_31) %>% 
  addTiles() %>%
  addCircleMarkers(~ X, ~ Y,
                   color = "#cd853f",
                   label = ~ as.character(paste0(Cidade, ": ", QTD)),
                   labelOptions = labelOptions(textsize = "13px"),
                   radius = ~ sqrt(QTD),
                   fillOpacity = 0.5)



#MAPA CALOR
# install.packages("brazilmaps")
# install.packages("sf", dependencies = T)
library(brazilmaps)
library(sf)
library(leaflet)
# pegando as geometrias das cidades de São Paulo (35)
shp = get_brmap("City", geo.filter = list(State = 35))
shp$City = as.character(shp$City)

shp31 = get_brmap("City", geo.filter = list(State = 35))
shp31$City = as.character(shp31$City)

ds_qtd_30_1 = ds_qtd_30
ds_qtd_30_1$CO_MUNICIPIO = as.character(ds_qtd_30_1$CO_MUNICIPIO)

ds_qtd_31_1 = ds_qtd_31
ds_qtd_31_1$CO_MUNICIPIO = as.character(ds_qtd_31_1$CO_MUNICIPIO)
# definindo que o dataframe contém dados geométricos
shp_sf = st_as_sf(shp)%>%  st_transform(4326)
shp_sf_31 = st_as_sf(shp31)%>%  st_transform(4326)
#unindo os dados de CNAE-30 com as geometrias das cidades.
shp_sf = shp_sf %>% filter (City %in% ds_qtd_30_1$CO_MUNICIPIO)
shp_sf = left_join(shp_sf,ds_qtd_30_1, by = c("City" = "CO_MUNICIPIO"))

shp_sf_31 = shp_sf_31 %>% filter (City %in% ds_qtd_31_1$CO_MUNICIPIO)
shp_sf_31 = left_join(shp_sf_31,ds_qtd_31_1, by = c("City" = "CO_MUNICIPIO"))
## define cores para cada conjunto numérico
pal = colorNumeric(palette = "Blues", domain = shp_sf$QTD)
pal = colorNumeric(palette = "Reds", domain = shp_sf_31$QTD)
# heatmap .
map_30 <- leaflet(shp_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = shp_sf,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              weight = 0.5,
              color = ~pal(QTD),
              opacity = 0.8,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0(sep = " ",
                              "<b>Cidade: </b>", nome, "<br>",
                              "<b>CNAEs: </b>", QTD, "<br>",
                              "<b>Vínculos: </b>", QTD),
              label = ~nome) %>% 
  addLegend("bottomright",
            title = "Quantidade Empresas - CNAE 30", 
            pal = pal, 
            values = ~QTD, 
            opacity = 0.8)

map_31 <- leaflet(shp_sf_31) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = shp_sf_31,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              weight = 0.5,
              color = ~pal(QTD),
              opacity = 0.8,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0(sep = " ",
                              "<b>Cidade: </b>", nome, "<br>",
                              "<b>CNAEs: </b>", QTD, "<br>",
                              "<b>Vínculos: </b>", QTD),
              label = ~nome) %>% 
  addLegend("bottomright",
            title = "Quantidade Empresas - CNAE 30", 
            pal = pal, 
            values = ~QTD, 
            opacity = 0.8)


#MAPA CALOR 2
# install.packages("brazilmaps")
# install.packages("sf", dependencies = T)
library(brazilmaps)
library(sf)
library(leaflet)
# pegando as geometrias das cidades de São Paulo (35)
heatmap_30 = get_brmap("City", geo.filter = list(State = 35))
heatmap_30$City = as.character(heatmap_30$City)

heatmap_31 = get_brmap("City", geo.filter = list(State = 35))
heatmap_31$City = as.character(heatmap_31$City)

dshm_qtd_30_1 = ds_qtd_30
dshm_qtd_30_1$CO_MUNICIPIO = as.character(dshm_qtd_30_1$CO_MUNICIPIO)

dshm_qtd_31_1 = ds_qtd_31
dshm_qtd_31_1$CO_MUNICIPIO = as.character(dshm_qtd_31_1$CO_MUNICIPIO)
# definindo que o dataframe contém dados geométricos
hm_sf_30 = st_as_sf(heatmap_30)%>%  st_transform(4326)
hm_sf_31 = st_as_sf(heatmap_31)%>%  st_transform(4326)
#unindo os dados de CNAE-30 com as geometrias das cidades.
hm_sf_30 = hm_sf_30 %>% filter (City %in% dshm_qtd_30_1$CO_MUNICIPIO)
hm_sf_30 = left_join(hm_sf_30,dshm_qtd_30_1, by = c("City" = "CO_MUNICIPIO"))

hm_sf_31 = hm_sf_31 %>% filter (City %in% dshm_qtd_31_1$CO_MUNICIPIO)
hm_sf_31 = left_join(hm_sf_31,dshm_qtd_31_1, by = c("City" = "CO_MUNICIPIO"))
## define cores para cada conjunto numérico
cores_30 = colorNumeric(palette = "Blues", domain = hm_sf_30$QTD)
cores_31 = colorNumeric(palette = "Reds", domain = hm_sf_31$QTD)
# heatmap .
hm_30 <- leaflet(hm_sf_30) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = hm_sf_30,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              weight = 0.5,
              color = ~cores_30(QTD),
              opacity = 0.8,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = T),
              popup = ~paste0(sep = " ",
                              "<b>Município: </b>", nome, "<br>",
                              "<b>CNAEs: </b>", QTD),
              label = ~nome)

hm_31 <- leaflet(hm_sf_31) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(data = hm_sf_31,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              weight = 0.5,
              color = ~cores_31(QTD),
              opacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0(sep = " ",
                              "<b>Município: </b>", nome, "<br>",
                              "<b>CNAEs: </b>", QTD),
              label = ~nome)

#LEAFLET points
## bibliotecas
library(leaflet)
df_agr = md_rais_SP_CNAE_all %>% 
          #filter(!(is.na(OC_latitude))) %>% 
          select(15, 19, 23,24, 25) %>% 
          group_by(`CEP Estab`,CO_MUNICIPIO, NOME_MUNICIPIO, LATITUDE, LONGITUDE) %>% 
          summarise(QTD = n())
colnames(df_agr)[1] = "cep"
colnames(df_agr)[2] = "COD_IBGE"
colnames(df_agr)[3] = "Cidade"
colnames(df_agr)[4] = "lat"
colnames(df_agr)[5] = "lng"
df_agr$lng = as.numeric(df_agr$lng)
df_agr$lat = as.numeric(df_agr$lat)
library(leaflet)
        
map_cities = leaflet(df_agr) %>% 
  addTiles() %>%
  addMarkers(lng = ~lng,
             lat = ~lat,
             popup = paste0("<b>CEP: </b>", df_agr$cep,"<br>",
                            "<b>Qtd: </b>",df_agr$QTD ),
             group = "addMarkers",
             clusterOptions = markerClusterOptions())
  


