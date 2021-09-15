library(shiny)            #App
library(shinydashboard)   #Para tableros
library(tidyverse)
library(ggplot2)          #Graficos
library(plotly)           #Interaccion graficos
library(haven)
library(srvyr)
library(survey)
library(DT)
library(rgdal)
library(maptools)
library(readxl)
gpclibPermit()
options(shiny.maxRequestSize = 300 * 1024^2)


shape <- readOGR(dsn = ".", layer = "c004Polygon", encoding = "UTF-8")
shape@data$id <- rownames(shape@data)
# create a data.frame from our spatial object
df <- fortify(shape, region = "id")
datosDF<- merge(df, shape@data, by = "id")
mapa_uru<-datosDF%>%
  dplyr::filter(popup != "LÍMITE CONTESTADO")
mapa_uru$popup<- recode(mapa_uru$popup,"ARTIGAS"="Artigas","CANELONES"="Canelones","CERRO LARGO"="Cerro Largo","COLONIA"="Colonia","DURAZNO"="Durazno","FLORES"="Flores","FLORIDA"="Florida","LAVALLEJA"="Lavalleja","MALDONADO"="Maldonado",
                        "MONTEVIDEO"="Montevideo","PAYSANDÚ"="Paysandú","RÍO NEGRO"="Río Negro","RIVERA"="Rivera","ROCHA"="Rocha","SALTO"="Salto","SAN JOSÉ"="San José","SORIANO"="Soriano","TACUAREMBÓ"="Tacuarembó","TREINTA Y TRES"="Treinta y Tres")

variables <- read_excel("variables.xlsx")

#####Funciones######


funcion1<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorA)
  
}

funcion2<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  return(Ind0)
  
}

funcion3<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  
  return(IndicadorA)
}

funcion4<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:7] -(1.96*Indicadorb[,2:7])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  sup[sup>1]<-1
  
  
  return(IndicadorA)
  
}

funcion5<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  sup[sup>1]<-1
  
  return(IndicadorA)
  
  
}

funcion6<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorA)
}

funcion7<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(ind)
  
}

funcion8<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total - (1.96*desv$Total_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(ind)
  
  
}

funcion9<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  inf[inf<0]<-0
  
  return(Ind0)
  
  
}


funcion1inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorInf)
  
}

funcion1sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  return(IndicadorSup)
  
}

funcion2inf<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  inf<- inf%>%
    select(auxe)
  
  sup<- sup%>%
    select(auxe)
  
  inf[inf<0]<-0
  
  
  return(inf)
  
}

funcion2sup<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             dplyr::filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      dplyr::filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  inf<- inf%>%
    select(auxe)
  
  sup<- sup%>%
    select(auxe)
  
  return(sup)
  
}

funcion3inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  
  
  
  return(inf)
}

funcion3sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  return(sup)
}

funcion4inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:7] -(1.96*Indicadorb[,2:7])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  
  
  return(inf)
  
}

funcion4sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(dpto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  return(sup)
  
}

funcion5inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  
  
  return(inf)
  
  
}

funcion5sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(dpto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  return(sup)
  
  
}

funcion6inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorInf)
}

funcion6sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorSup)
}

funcion7inf<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(inf)
  
}

funcion7sup<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(sup)
  
}

funcion8inf<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total - (1.96*desv$Total_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  inf[,2:ncol(inf)][inf[,2:ncol(inf)]<0] <- 0
  
  return(inf)
  
  
}

funcion8sup<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  sup[,2:ncol(sup)][sup[,2:ncol(sup)]>1] <- 1
  return(sup)
  
  
}

funcion9inf<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  inf[inf<0]<-0
  
  return(inf)
  
  
}

funcion9sup<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  return(sup)
  
  
}
