library(dplyr)
library(readxl)
t1=read_excel("Github/private.gob/Seguridad_Tablero/Datos/BASE DELITOS 15-24.xlsx")#Solo una hoja
t2=read_excel("Github/private.gob/Seguridad_Tablero/Datos/Delitos 15-24_total por año y por delito_POR CADA MIL.xlsx",sheet = "tot est delito año_e")
#Usar la primera para alimentar la gráfica de barras de tipos de delitos por año.
#Se cancela. Usar la segunda.

|>as_tibble()
as.data.frame(table(t2$Año))
for(i in 0:8){
  print(identical(t2$Tipo_delito[(40*i+1):(40*i+40)],t2$Tipo_delito[(40*i+1+40):(40*(i+2))]))
}
t3=read_excel("Github/private.gob/Seguridad_Tablero/Datos/Delitos a nivel nacional 2024.xlsx")##Dos hojas. La primera 
t4=read_excel("Github/private.gob/Seguridad_Tablero/Datos/Delitos_total por municipio 15-24.xlsx")##una hoja
tmp <- quux |>
  rowwise() |>
  mutate(fillers = list(c_across(matches("^fil[0-9]")))) |>
  ungroup() |>
  select(-matches("^fil[0-9]"))
split(tmp, tmp$ew_id) |>
  unname() |>
  jsonlite::toJSON(pretty = TRUE)







########## Datos para colorear mapas por año: Municipal 
t1_1=t1_1|>dplyr::mutate(valor_p_color=(`Suma de total`/población)*1000)
t1_1|>write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs/Hidalgo_colores_p_año.csv",row.names = F)


########## Datos para colorear mapas por año: Municipal 

t3_1=t3|>
  dplyr::select(-`pob por cada 1000`)|>
  dplyr::group_by(Estado)|>
  dplyr::summarise(`suma delitos`=sum(`suma delitos`),
                   pob_estatal=max(pob_estatal))
t3_1=t3_1|>
  dplyr::mutate(valor_p_color=1000*(`suma delitos`/pob_estatal))
t3_1|>write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs/Nacional_colores_solo_2024.csv",row.names = F)












##
datos_estatal_2025=readxl::read_excel("Github/private.gob/Seguridad_Tablero/Datos/nacional mensual 15-24.xlsx")
resumen_estatal_2025=datos_estatal_2025|>
  dplyr::select(año,`Tipo de delito`,tasa)|>
  dplyr::group_by(año,`Tipo de delito`)|>
  dplyr::summarise(tasa_media=mean(tasa))
library(readr)
datos_municipal_2025=read.csv("Github/private.gob/Seguridad_Tablero/Datos/IDM_NM_ene25.csv",check.names = T,fileEncoding = "latin1")
hidalgo_municipal_2025=datos_municipal_2025|>
  dplyr::filter(Clave_Ent==13)


hidalgo_municipal_2025$total=rowSums(hidalgo_municipal_2025|>dplyr::select(Enero:Diciembre))

##### nacional 2015
intercensal_nac_2015=readxl::read_excel("Github/private.gob/Seguridad_Tablero/Datos/intercensal_nacional_2015.xls")
intercensal_nac_2015_2=intercensal_nac_2015|>
  dplyr::select(`Entidad federativa`,`Grupos quinquenales de edad`,`Población total`,Estimador)|>
  dplyr::filter(!is.na(`Entidad federativa`) & `Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(`Entidad federativa`=substr(`Entidad federativa`,4,nchar(`Entidad federativa`)))|>
  dplyr::select(-Estimador,-`Grupos quinquenales de edad`)

##nacional 2020
nacional2020=readxl::read_excel("Github/private.gob/Seguridad_Tablero/Datos/Poblacion_nacional2020.xlsx")
nacional2020=nacional2020|>
  dplyr::select(`Entidad federativa`,`Grupo quinquenal de edad`,`2020...9`)|>
  dplyr::filter(`Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(`Grupo quinquenal de edad`=='Total')|>
  dplyr::select(-`Grupo quinquenal de edad`)


## municipal 2015
intercensal_mun_2015=readxl::read_excel("Github/private.gob/Seguridad_Tablero/Datos/intercensal_hidalgo_2015.xls")
intercensal_mun_2015_2=intercensal_mun_2015|>
  dplyr::select(Municipio,`Grupos quinquenales de edad`,Estimador,`Población total`)|>
  dplyr::filter(!is.na(`Municipio`) & Municipio!='Total')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(Municipio=substr(Municipio,5,nchar(Municipio)))|>
  dplyr::select(-Estimador)|>
  dplyr::select(-`Grupos quinquenales de edad`)



## municipal 2020
intercensal_nac_2020=readxl::read_excel("infografías/Datos/Banco de datos infografias _Eduardo.xlsx")
intercensal_nac_2020=intercensal_nac_2020|>
  dplyr::select(Municipio,`Población total`)|>
  dplyr::filter(!is.na(Municipio)& Municipio!='Estatal')

##Le pegamos la poblacion a cada bloque de 5 años. 
datos_estatal_2025$pobtot[datos_estatal_2025$año%in%c(2015:2019)]=
intercensal_nac_2015_2$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(5)
#el de 2020:2025 sí está bien

##Corregimos la tasa
datos_estatal_2025=datos_estatal_2025|>
  dplyr::mutate(tasa=1000*total/pobtot)



######################Lo mismo pero para la municipal
hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::select(Año,Municipio,Tipo.de.delito,Enero:Diciembre)

hidalgo_municipal_2025$total=rowSums(hidalgo_municipal_2025|>dplyr::select(Enero:Diciembre),na.rm = T)

##Le pegamos del 2015 a 2019 la poblacion
hidalgo_municipal_2025$pobtot=rep(0,nrow(hidalgo_municipal_2025))
hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::arrange(Año,Municipio,Tipo.de.delito)
hidalgo_municipal_2025_2=hidalgo_municipal_2025|>
  dplyr::group_by(Año,Municipio,Tipo.de.delito)|>
  dplyr::summarise_all(sum)
##Rellenamos 2015
hidalgo_municipal_2025_2$pobtot[hidalgo_municipal_2025_2$Año%in%c(2015:2019)]=
  intercensal_mun_2015_2$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(5)
##Rellenamos 2020
hidalgo_municipal_2025_2$pobtot[hidalgo_municipal_2025_2$Año%in%c(2020:2025)]=
  intercensal_nac_2020$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(6)

#Calculamos tasa 
hidalgo_municipal_2025_2=hidalgo_municipal_2025_2|>
  dplyr::mutate(pobtot=as.numeric(pobtot))|>
  dplyr::mutate(tasa=1000*total/pobtot)

##Guardamos preliminares. 
datos_estatal_2025|>write.csv("Github/private.gob/Seguridad_Tablero/Datos/Preliminares/datos_estatal_2025.csv",row.names = F,fileEncoding = "UTF-8")
hidalgo_municipal_2025_2|>write.csv("Github/private.gob/Seguridad_Tablero/Datos/Preliminares/datos_municipal_hgo_2025.csv",row.names = F,fileEncoding = "UTF-8")

##Ahora sí generamos los CSV consumibles por js.

datos_estatal_2025|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(año,`Tipo de delito`,total,pobtot,tasa)|>
  write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs_2/Hidalgo_Año_y_Tipo.csv",row.names = F,fileEncoding = "UTF-8")


datos_estatal_2025|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(-Entidad)|>
  tidyr::pivot_longer(cols = c(Enero:Diciembre),names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(año, `Tipo de delito`,Mes,Conteo)|>
  dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))|>
  write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs_2/delitos por mes_15-24_estatal.csv",row.names = F,fileEncoding = "UTF-8")


datos_estatal_2025|>
  dplyr::select(año,`Tipo de delito`,tasa,Entidad)|>
  dplyr::group_by(año,`Tipo de delito`)|>
  dplyr::summarise(tasa_media=mean(tasa),tasa_mediana=median(tasa))|>
  write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs_2/tasa_media_nacional.csv",fileEncoding = "UTF-8",row.names = F)


hidalgo_municipal_2025_2|>
  tidyr::pivot_longer(cols = Enero:Diciembre,names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(Año,Municipio,Tipo.de.delito,Mes,Conteo)|>
  write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs_2/delitos por mes_15-24.csv",fileEncoding = "UTF-8",row.names = F)


class(hidalgo_municipal_2025_2$pobtot)
|>write.csv("Github/private.gob/Seguridad_Tablero/Datos/CSVs/prueba.csv",row.names = F,fileEncoding = "UTF-8")
