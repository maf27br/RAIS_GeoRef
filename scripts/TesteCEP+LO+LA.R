#install.packages("cepR")
library(cepR)
library(dplyr)

a = busca_cep(cep = "12120000", token = "98c41d3b04acb40807c4eea26109b74e")

for (i in 1:1){
  a = busca_cep(cep = "12516110", token = "98c41d3b04acb40807c4eea26109b74e")
  Sys.sleep(3)
  b = busca_cep(cep = "09520310", token = "98c41d3b04acb40807c4eea26109b74e")
}

df = md_rais_SP_CNAE_all
df$OC_estado = ""
df$OC_cidade = ""
df$OC_bairro = ""
df$OC_cep = ""
df$OC_logradouro = ""
df$OC_latitude = 0
df$OC_longitude = 0
df$OC_altitude = 0
df$OC_ddd = 0
df$OC_cod_IBGE = ""

for (row in 1:nrow(df)) {
  a = busca_cep(cep = str_pad(df[row,15], 8, pad = "0"), token = "98c41d3b04acb40807c4eea26109b74e")
  Sys.sleep(3)
  df[row,]$OC_estado = a$estado
  df[row,]$OC_cidade = a$cidade
  df[row,]$OC_bairro = a$bairro
  df[row,]$OC_cep = a$cep
  df[row,]$OC_logradouro = a$logradouro
  df[row,]$OC_latitude = a$latitude
  df[row,]$OC_longitude = a$longitude
  df[row,]$OC_altitude = a$altitude
  df[row,]$OC_ddd = a$ddd
  df[row,]$OC_cod_IBGE = a$cod_IBGE
  rm(a)
}
