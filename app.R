#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(maptools)
library(mapproj)
library(shinythemes)
library(shinyWidgets)
library(fda)
library(fda.usc)
library(sf)
library(ggplot2)
library(tibble)
library(flexdashboard)
library(readxl)
###########################################################################
###########################################################################
###########################################################################
#           TRATAR DATOS 
###########################################################################
###########################################################################
###########################################################################

################################################
#####        ALMACENAR DATOS PDIA          #####
################################################

datos_PDIA <- readr::read_delim("https://dadesobertes.gva.es/dataset/ce195af2-39ec-4f44-bb77-b14235519b0d/resource/cb50e7d2-0c0e-46b8-a359-a0fa35998577/download/covid-19-serie-de-casos-con-pdia-positiva-en-la-comunitat-valenciana.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE) 
Poblacion <- read_excel("Poblacion.xlsx")

departamentos_mapa <- st_read("departamentos_salud_ogr.json")


################################################

ultima_pos = length(datos_PDIA$`Data diagnòstic laboratori/fecha diagnóstico laboratorio`)
ini = ultima_pos-15
Casos_Últimos_14_Dies = sum(datos_PDIA$C.Valenciana[ini:ultima_pos])
ini_prev = ini - 15 # PARA LA TASA DE CRECIMIENTO
data_actualitzacio <- datos_PDIA$`Data diagnòstic laboratori/fecha diagnóstico laboratorio`[ultima_pos]


################################################
#####            INCIDENCIAS               #####
################################################
datos_PDIA_Departamentos <- datos_PDIA[,8:31]
poblacion_Departamentos <- Poblacion[5:28,] 
incidencias_Departamentos <- datos_PDIA_Departamentos
for (i in 1:dim(datos_PDIA_Departamentos)[2]){
  incidencias_Departamentos[,i]<- datos_PDIA_Departamentos[,i]/as.numeric(poblacion_Departamentos[i,2])*100000
}


################################################
#####      DATOS FUNCIONALES               #####
################################################

baseFourier = create.fourier.basis(rangeval = c(1,849), nbasis = 27)
baseBspline = create.bspline.basis(rangeval = c(1,849), nbasis = 27,norder = 5)
suavizadoFourier = smooth.basis(argvals = 1:849, y = as.matrix(incidencias_Departamentos), fdParobj = baseFourier)
suavizadoBspline = smooth.basis(argvals = 1:849, y = as.matrix(incidencias_Departamentos), fdParobj = baseBspline)

################################################
#####       TABLA CON ESTADISTICOS         #####
################################################
# REGIO
# MEDIA
# MAXIMO ALCANZADO
# CASOS EN EL ULTIMO DIA
# CASOS EN LOS ULTIMOS 14 DIAS
# INCIDENCIA
# TENDENCIA

tab = tibble(
  Regió = "C. Valenciana",
  Mitjana = round(mean(datos_PDIA$C.Valenciana),3),
  Máxim_Alcançat = round(max(datos_PDIA$C.Valenciana),3),
  Casos_Últim_Día = datos_PDIA$C.Valenciana[ultima_pos],
  Casos_Ultims_14_Dies = sum(datos_PDIA$C.Valenciana[ini:ultima_pos]),
  Casos_Totals = sum(datos_PDIA$C.Valenciana),
  Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
  Tendencia = (sum(datos_PDIA$C.Valenciana[ini_prev:ini]) - sum(datos_PDIA$C.Valenciana[ini:ultima_pos])),
  Varianza = round(sd(datos_PDIA$C.Valenciana),3)
) %>%
  add_row(
    Regió = "Homes",
    Mitjana = round(mean(datos_PDIA$`Homes/Hombres`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Homes/Hombres`),3),
    Casos_Últim_Día = datos_PDIA$`Homes/Hombres`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Homes/Hombres`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Homes/Hombres`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Homes/Hombres`[ini_prev:ini]) - sum(datos_PDIA$`Homes/Hombres`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`Homes/Hombres`),3)
  ) %>%
  add_row(
    Regió = "Dones",
    Mitjana = round(mean(datos_PDIA$`Dones/Mujeres`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Dones/Mujeres`),3),
    Casos_Últim_Día = datos_PDIA$`Dones/Mujeres`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Dones/Mujeres`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Dones/Mujeres`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Dones/Mujeres`[ini_prev:ini]) - sum(datos_PDIA$`Dones/Mujeres`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`Dones/Mujeres`),3)
  ) %>%
  add_row(
    Regió = "Prov. Alacant",
    Mitjana = round(mean(datos_PDIA$`Prov. Alacant/Alicante`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. Alacant/Alicante`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. Alacant/Alicante`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. Alacant/Alicante`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. Alacant/Alicante`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[2])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Prov. Alacant/Alicante`[ini_prev:ini]) - sum(datos_PDIA$`Prov. Alacant/Alicante`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`Prov. Alacant/Alicante`),3)
  ) %>%
  add_row(
    Regió = "Prov. Castello",
    Mitjana = round(mean(datos_PDIA$`Prov. Castelló/Castellón`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. Castelló/Castellón`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. Castelló/Castellón`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. Castelló/Castellón`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. Castelló/Castellón`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[3])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Prov. Castelló/Castellón`[ini_prev:ini]) - sum(datos_PDIA$`Prov. Castelló/Castellón`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`Prov. Castelló/Castellón`),3)
  ) %>%
  add_row(
    Regió = "Prov. Valencia",
    Mitjana = round(mean(datos_PDIA$`Prov. València`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. València`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. València`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. València`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. València`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[4])*100000, 3), 
    Tendencia = (sum(datos_PDIA$`Prov. València`[ini_prev:ini]) - sum(datos_PDIA$`Prov. València`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`Prov. València`),3)
  ) %>%
  add_row(
    Regió = "Dep. Vinaros",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[5])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),3)
  )%>%
  add_row(
    Regió = "Dep. Castello",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[6])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),3)
  ) %>%
  add_row(
    Regió = "Dep. La Plana",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[7])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Sagunt",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[8])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),3)
  ) %>%
  add_row(
    Regió = "Dep. VCIA Clinic-La Malva-Rosa",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[9])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),3)
  ) %>%
  add_row(
    Regió = "Dep. VCIA Arnau de Vilanova Lliria",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[10])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Valencia- La Fe",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[11])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),3)
  ) %>%
  add_row(
    Regió = "Dep. Requena",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[12])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Valencia Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[13])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),3)
  ) %>%
  add_row(
    Regió = "Dep. Valencia Doctor",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[14])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),3)
  ) %>%
  add_row(
    Regió = "Dep. La Ribera",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[15])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Gandia",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[16])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Denia",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[17])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Xativa-Ontinyent",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[18])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),3)
  ) %>%
  add_row(
    Regió = "Dep. Alcoi",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[19])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),3)
  ) %>%
  add_row(
    Regió = "Dep. La Marina Baixa",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[20])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Sant Joan D'Alacant",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[21])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),3)
  ) %>%
  add_row(
    Regió = "Dep. Elda",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[22])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Alacant Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[23])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),3)
  ) %>%
  add_row(
    Regió = "Dep. Elx Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[24])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),3)
  ) %>%
  add_row(
    Regió = "Dep. Orihuela",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[25])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Torrevieja",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[26])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),3)
  ) %>%
  add_row(
    Regió = "Dep. Manises",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[27])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),3)
  ) %>%
  add_row(
    Regió = "Dep. Elx-Crevillent",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[28])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini:ultima_pos])),
    Varianza = round(sd(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),3)
  )



################################################
#####          IDENTIFICADOR               #####
################################################

# IDENTIFICAR CADA SELEECION DEL INPUT CON UN NUMERO IDENTIFICATIVO

nombr_id = c("C.Valenciana" = 1
             , "Homes/Hombres" = 2
             , "Dones/Mujeres" = 3
             , "Prov. Alacant/Alicante" = 4
             , "Prov. Castelló/Castellón" = 5
             , "Prov. València" = 6
             , "DEPARTAMENT DE SALUT DE VINAROS" = 7
             , "DEPARTAMENT DE SALUT DE CASTELLO" = 8
             , "DEPARTAMENT DE SALUT DE LA PLANA" = 9
             , "DEPARTAMENT DE SALUT DE SAGUNT" = 10
             , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = 11
             , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" = 12
             , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = 13
             , "DEPARTAMENT DE SALUT DE REQUENA" = 14
             , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = 15
             , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = 16
             , "DEPARTAMENT DE SALUT DE LA RIBERA" = 17
             , "DEPARTAMENT DE SALUT DE GANDIA" = 18
             , "DEPARTAMENT DE SALUT DE DENIA" = 19
             , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = 20
             , "DEPARTAMENT DE SALUT D'ALCOI" = 21
             , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = 22
             , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" =23
             , "DEPARTAMENT DE SALUT D'ELDA" = 24
             , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = 25
             , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = 26
             , "DEPARTAMENT DE SALUT D'ORIHUELA" = 27
             , "DEPARTAMENT DE SALUT DE TORREVIEJA" = 28
             , "DEPARTAMENT DE SALUT DE MANISES" = 29
             , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = 30
)  

# IDENTIFICAR TODAS AQUELLAS ZONAS QUE NO SON DEPARTAMENTOS CON 1, Y LAS QUE SON DEPARTAMENTOS CON SU NOMBRE

nombr_dep = c("C.Valenciana" = 1
              , "Homes/Hombres" = 1
              , "Dones/Mujeres" = 1
              , "Prov. Alacant/Alicante" = 1
              , "Prov. Castelló/Castellón" = 1
              , "Prov. València" = 1
              , "DEPARTAMENT DE SALUT DE VINAROS" = "VINAROS"
              , "DEPARTAMENT DE SALUT DE CASTELLO" = "CASTELLON"
              , "DEPARTAMENT DE SALUT DE LA PLANA" = "LA PLANA"
              , "DEPARTAMENT DE SALUT DE SAGUNT" = "SAGUNTO"
              , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = "VALENCIA - CLINICO"
              , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" =  "VALENCIA - ARNAU DE VILANOVA"
              , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = "VALENCIA - LA FE"
              , "DEPARTAMENT DE SALUT DE REQUENA" = "REQUENA"
              , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = "C.E. JUAN LLORENS - TORRENT - ALD"
              , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = "VALENCIA - DR. PESET"
              , "DEPARTAMENT DE SALUT DE LA RIBERA" = "LA RIBERA"
              , "DEPARTAMENT DE SALUT DE GANDIA" = "GANDIA"
              , "DEPARTAMENT DE SALUT DE DENIA" = "DENIA"
              , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = "XATIVA - ONTINYENT"
              , "DEPARTAMENT DE SALUT D'ALCOI" = "ALCOI"
              , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = "VILA JOIOSA"
              , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" = "ALICANTE - SAN JUAN"
              , "DEPARTAMENT DE SALUT D'ELDA" = "ELDA"
              , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = "ALICANTE"
              , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = "ELX"
              , "DEPARTAMENT DE SALUT D'ORIHUELA" = "ORIHUELA"
              , "DEPARTAMENT DE SALUT DE TORREVIEJA" = "TORREVIEJA"
              , "DEPARTAMENT DE SALUT DE MANISES" = "MANISES"
              , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = "ELX-CREVILLENT"
) 

# IDENTIFICAR TODAS AQUELLAS ZONAS QUE NO SON DEPARTAMENTOS CON 0, Y LAS QUE SON DEPARTAMENTOS CON SU NUMERO DE DEPARTAMENTOS

num_dep = c("C.Valenciana" = 0
            , "Homes/Hombres" = 0
            , "Dones/Mujeres" = 0
            , "Prov. Alacant/Alicante" = 0
            , "Prov. Castelló/Castellón" = 0
            , "Prov. València" = 0
            , "DEPARTAMENT DE SALUT DE VINAROS" = 01
            , "DEPARTAMENT DE SALUT DE CASTELLO" = 02
            , "DEPARTAMENT DE SALUT DE LA PLANA" = 03
            , "DEPARTAMENT DE SALUT DE SAGUNT" = 04
            , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = 05
            , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" =  06
            , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = 07
            , "DEPARTAMENT DE SALUT DE REQUENA" = 08
            , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = 09
            , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = 10
            , "DEPARTAMENT DE SALUT DE LA RIBERA" = 11
            , "DEPARTAMENT DE SALUT DE GANDIA" = 12
            , "DEPARTAMENT DE SALUT DE DENIA" = 13
            , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = 14
            , "DEPARTAMENT DE SALUT D'ALCOI" = 15
            , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = 16
            , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" = 17
            , "DEPARTAMENT DE SALUT D'ELDA" = 18
            , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = 19
            , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = 20
            , "DEPARTAMENT DE SALUT D'ORIHUELA" = 21
            , "DEPARTAMENT DE SALUT DE TORREVIEJA" = 22
            , "DEPARTAMENT DE SALUT DE MANISES" = 23
            , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = 24
) 

###########################################################################
###########################################################################
###########################################################################
#    INTERFAZ
###########################################################################
###########################################################################
###########################################################################

ui <- fluidPage(
    theme = shinytheme("superhero"),
    
    # TITULO Y LOGO
    titlePanel( div(column(width = 8, height = 8, h1("Modelització de casos PDIA positius a la Comunitat Valenciana")),
                    column(width = 4, height = 6, tags$img(src='Logo.png'))),
                windowTitle="Modelització de casos PDIA positius a la Comunitat Valenciana"),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # INTRO
            tags$p("A continuació anem a comentar una sèrie de casos amb  Proves Diagnòstiques d’Infecció Activa, més conegut com PDIA, positives en la Comunitat Valenciana, segons data de diagnòstic de laboratori. Les PDIA inclouen dues proves de detecció d’infecció activa, la PCR (sigles de polymerase chain reaction, reacció en cadena de la polimerasa) i el test d’antígens, d’acord amb els procediments i documents tècnics vigents elaborats pel Ministeri de Sanitat."),
            
            # BOTON DE SELEECION DE LA ZONA
            selectInput (inputId = "Zona",
                         label = "ZONA:",
                         selected = "C.Valenciana",
                         choices = c("C.Valenciana" = "C.Valenciana"
                                     , "Homes" = "Homes/Hombres"
                                     , "Dones" = "Dones/Mujeres"
                                     , "Prov. Alacant" = "Prov. Alacant/Alicante"
                                     , "Prov. Castelló" = "Prov. Castelló/Castellón"
                                     , "Prov. València" = "Prov. València"
                                     , "Dep. Vinaros" = "DEPARTAMENT DE SALUT DE VINAROS"
                                     , "Dep. Castelló" = "DEPARTAMENT DE SALUT DE CASTELLO"
                                     , "Dep. La Plana" = "DEPARTAMENT DE SALUT DE LA PLANA"
                                     , "Dep. Sagunt" = "DEPARTAMENT DE SALUT DE SAGUNT"
                                     , "Dep. VCIA Clinic-Malva Rosa" = "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA"
                                     , "Dep. VCIA Aranau Vilanova Lliria" = "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA"
                                     , "Dep. Valencia La Fe" = "DEPARTAMENT DE SALUT DE VALENCIA - LA FE"
                                     , "Dep. Requena" = "DEPARTAMENT DE SALUT DE REQUENA"
                                     , "Dep. Valencia Hosp. General" = "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL"
                                     , "Dep. Valencia Docotor Peset" = "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET"
                                     , "Dep. La Ribera" = "DEPARTAMENT DE SALUT DE LA RIBERA"
                                     , "Dep. Gandia" = "DEPARTAMENT DE SALUT DE GANDIA" 
                                     , "Dep. Denia" = "DEPARTAMENT DE SALUT DE DENIA"
                                     , "Dep. Xativa Ontinyent " = "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT"
                                     , "Dep. Alcoi" = "DEPARTAMENT DE SALUT D'ALCOI"
                                     , "Dep. La Marina Baixa" = "DEPARTAMENT DE SALUT DE LA MARINA BAIXA"
                                     , "Dep. Sant Joan D'Alacant" = "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT"
                                     , "Dep. Elda" = "DEPARTAMENT DE SALUT D'ELDA"
                                     , "Dep. Alacant Hosp. General" = "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL"
                                     , "Dep. Elx Hosp. General" = "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL"
                                     , "Dep. Orihuela" = "DEPARTAMENT DE SALUT D'ORIHUELA"
                                     , "Dep. Torrevieja" = "DEPARTAMENT DE SALUT DE TORREVIEJA"
                                     , "Dep. Manises" = "DEPARTAMENT DE SALUT DE MANISES"
                                     , "Dep. Elx-Crevillent"= "DEPARTAMENT DE SALUT D'ELX-CREVILLENT"
                         ) 
            ),
            p("D'altra banda, oferim la possibilitat de mostrar una classificació dels departaments aplicant un model de classificació no supervisada a les incidències dels departaments."),
            p("Selecciona el nombre de clústers amb els què vols realitzar la classificació."), 
            radioButtons("numClustes", label = h3("Nombre de grups"),
                         choices = list("3 Grups" = 3, "4 Grups" = 4),  
                         selected = 3),
            p("Les dades recollides a la base de dades són de l'1 de Febrer de 2020 fins al 29 de maig de 2022"),
            
            # ENLACE AL COGIDO
            tags$a(href="https://github.com/juanponsg/Estudia-e-Investiga.git", "Código")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(id = "main",
                        tabPanel(
                            title='INICIO', 
                            
                            # MOSTRAR WIDGETS EN UNA FILA CON TRES COLUMNAS Y SUS CORRESPONDIENTES BOTONES DE INFORMACION
                            fluidRow(
                                div(column(4,div(column(3, tags$h4("Mitjana")),column(1,actionButton("InfoMitjana", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana")),
                                    column(4,div(column(4, tags$h4("Incidencia")),column(1,actionButton("InfoIncidencia", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("incidencia")),
                                    column(4,div(column(7, tags$h4("Mitjana 14 dies")),column(1,actionButton("InfoMitjana14", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana14")))
                            ),
                            
                            # MOSTRAR TITULOS MAPA Y GRAFICA CON LOS BOTONES DE INFORMACION EN UNA LINEA Y DOS COLUMNAS
                            fluidRow(
                                div(column(6,div(column(5, tags$blockquote("Mapa")),column(1,actionButton("InfoMapaSerie", (tags$img(src="info.png",width = 24, height = 24)))) ) ),
                                    column(6,div(column(5, tags$blockquote("Evolució")),column(1,actionButton("InfoGraficaSerie", (tags$img(src="info.png",width = 24, height = 24)))))))
                            ),
                            
                            # MOSTRAR EL MAPA Y LA GRAFICA EN UNA MISMA FILA Y DOS COLUMNAS
                            fluidRow(
                                div(column(6, plotOutput("mapa")),
                                    column(6, plotOutput("evolucion")), align = "center")
                            ) 
                        ),
                        tabPanel(
                          title='Clustering',
                          h2("Clasificació no supervisada, K-means"),
                          p('A continuació mostrem una possible classificació de les incidències dels departaments de salut. Per això hem obtingut les corbes que representen aquestes incidències en dues bases de funcions.'), 
                          p("- Bases de Fourier, una base de 27 elements ( 1 constant, 13 pits i 13 cosinus)."),
                          p("- Base B-spline, una base amb 27 splines d'ordre 5"),
                          fluidRow(
                            div(column(6,div(column(10, tags$blockquote("Clasificació respecte les Bases de Fourier")),column(1,actionButton("InfoMapaFou", (tags$img(src="info.png",width = 24, height = 24)))) ) ),
                                column(6,div(column(9, tags$blockquote("Clasificació respecte les Bases B-splines")),column(1,actionButton("InfoMapaBsp", (tags$img(src="info.png",width = 24, height = 24)))))))
                          ),
                          fluidRow(
                            div(column(6, plotOutput("mapa_kmeans_fou")),
                                column(6, plotOutput("mapa_kmeans_bsp")), align = "center")
                          ) ,
                          fluidRow(
                            div(column(6,div(column(8, tags$blockquote("Incidencia amb Bases de Fourier")),column(1,actionButton("InfoIncidenciaFou", (tags$img(src="info.png",width = 24, height = 24)))) ) ),
                                column(6,div(column(8, tags$blockquote("Incidencia amb Bases B-Splines")),column(1,actionButton("InfoIncidenciaBsp", (tags$img(src="info.png",width = 24, height = 24)))))))
                          ),
                          fluidRow(
                            div(column(6, plotOutput("graficaIncidencia_Fou")),
                                column(6, plotOutput("graficaIncidencia_Bsp")), align = "center")
                          )
                        )
            )
            
            
        )
    )
)

###########################################################################
###########################################################################
###########################################################################
#       PARTE SERVIDOR
###########################################################################
###########################################################################
###########################################################################
server <- function(input, output) {

    ################################################
    #####        WIDGETS RANGO VALORES         #####
    ################################################
    
    output$mitjana <- flexdashboard::renderGauge({
      valor <- tab$Mitjana[nombr_id[input$Zona]]
      gauge(valor, min = 0, max = tab$Mitjana[1], gaugeSectors(
        success = c(0, tab$Mitjana[1]/10), warning = c(tab$Mitjana[1]/10, tab$Mitjana[1]), danger = c(tab$Mitjana[1]/2, tab$Mitjana[1])
      ))
    })
    
    output$incidencia <- flexdashboard::renderGauge({
      valor <- tab$Incidencia[nombr_id[input$Zona]]
      
      gauge(valor, min = 0, max = 250, gaugeSectors(
        success = c(0, 50), warning = c(50, 150), danger = c(150, 250)
      ))
    })
    
    output$mitjana14 <- flexdashboard::renderGauge({
      valor <- round(tab$Casos_Ultims_14_Dies[nombr_id[input$Zona]]/14,3)
      gauge(valor, min = 0, max = tab$Casos_Ultims_14_Dies[1]/14, gaugeSectors(
        success = c(0, tab$Casos_Ultims_14_Dies[1]/140), warning = c(tab$Casos_Ultims_14_Dies[1]/140, tab$Casos_Ultims_14_Dies[1]/28), danger = c(tab$Casos_Ultims_14_Dies[1]/28, tab$Casos_Ultims_14_Dies[1]/14)
      ))
    })
    

    ################################################
    #####     GRAFICA CASOS POSTIVIOS          #####
    ################################################
    
    output$evolucion <- renderPlot({
        btn1 <- input$Zona
        btn0 <- "Data diagnòstic laboratori/fecha diagnóstico laboratorio"
        
        
        ggplot(datos_PDIA,aes(x = datos_PDIA[[btn0]], y = datos_PDIA[[btn1]]))+geom_line()+ scale_x_date()+labs(
            x = "Data",
            y = "Nombre de casos"
        )
        
    })
  
  ################################################
  #####        MAPA INCIDENCIA               #####
  ################################################
  # UTILIZAMOS UN CONDICIONAL YA QUE ALGUNAS ZONAS 
  # NO SON UN DEPARTAMENTOS Y ESTOS TIENEN UN 
  # NUM_DEPARTAMENTO CON VALOR 1
    
    output$mapa <- renderPlot({
      valor = nombr_dep[nombr_id[input$Zona]]
      valor2 = num_dep[input$Zona]
      if (valor == 1 ){
        
        INCIDENCIA<-tab$Incidencia[7:30]
        ggplot() +
          geom_sf(data = departamentos_mapa,aes(fill = INCIDENCIA), color = "black")  +  scale_colour_gradientn(colors = terrain.colors(20)) 
        
        
      }else {
        #mapa_depart <- mapa_departamentos %>% filter(DEPARTAMENTO == valor)
        INCIDENCIA<-tab$Incidencia[7:30]
        ggplot() +
          geom_sf(data = departamentos_mapa,aes(fill = INCIDENCIA), color = "black")  +  scale_colour_gradientn(colors = terrain.colors(20)) +
          geom_sf(data = departamentos_mapa[valor2,],aes(fill = INCIDENCIA[valor2]), color = "red") +
          geom_sf_text(
            data = departamentos_mapa[valor2,],
            aes(label = INCIDENCIA[valor2]),
            check_overlap = FALSE,
            size = 5,
            color = "black") 
      }
    })
    ################################################
    #####        BOTONES DE INFORMACON         #####
    ################################################

    observeEvent(input$InfoMitjana, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Mitjana respecte a la mitjana de la Comunitat Valenciana", type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoIncidencia, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Incidència acumulada en els últims 14 dies", type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoMitjana14, {
      shinyalert(title = input$Zona, text = "Mitjana dels Últims 14 dies respecte als últims 14 dies a la Comunitat Valenciana", type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    
    observeEvent(input$InfoMapaSerie, {
      shinyalert(title = input$Zona, text = "Mapa dels departaments de salut acolorits segons els casos PDIA positius"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoGraficaSerie, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Gràfica de l'evolució dels casos positius de Covid-19 des del 2020-01-02 fins a la última data d'actualització"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    
    observeEvent(input$InfoIncidenciaFou, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Comparació entre els centroides resultants d'aplicar K-means a les corbes que representen les sèries temporals de la incidència mitjançant Bases de Fourier amb la corba de la incidència del departament seleccionat"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoIncidenciaBsp, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Comparació entre els centroides resultants d'aplicar K-means a les corbes que representen les sèries temporals de la incidència mitjançant Bases B-splines amb la corba de la incidència del departament seleccionat"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoMapaFou, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Mapa del resultat de la classificació dels departaments aplicant K-means als revolts que representin les sèries temporals de la incidència a partir de Bases de Fourier"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$InfoMapaBsp, {
      # Show a simple modal
      shinyalert(title = input$Zona, text = "Mapa del resultat de la classificació dels departaments aplicant K-means als revolts que representin les sèries temporals de la incidència a partir de Bases B-sp`lines"
                 , type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE)
    })
    

    ################################################
    #####      CLUSTERING INCIDNECIAS          #####
    ################################################
    K <- reactive({
        K <- as.numeric(input$numClustes)
        if(K==3){
          K<-c(1,14,20)
        }
        else if(K==4){
          K<-c(2,14,8,21)
        }
        K
    })
    
  
    kmedias_Fou<- reactive({
        kmeans.fd(suavizadoFourier$fd,max.iter = 2, ncl = K(),draw = FALSE,cluster.size = 3)
    })
    
    kmedias_Bsp<- reactive({
        kmeans.fd(suavizadoBspline$fd,max.iter = 2, ncl = K(),draw = FALSE,cluster.size = 3)
    })
    
    
    ################################################
    #####       GRÁFICAS INCIDNECIAS           #####
    ################################################
    grafica_incidencia_Fou <- renderPlot ({
      kmedias_Fou<-kmeans.fd(suavizadoFourier$fd,ncl = K(),draw = FALSE,cluster.size = 3)
      par(mfrow=c(1,1)) 
      btn1 <- num_dep[input$Zona]
      if(btn1!=0){
        plot(suavizadoFourier$fd[btn1],col="yellow",lwd=3,ylim=c(0,450))
        lines(kmedias_Fou$centers,lwd=2,col=c(2,3,4,5))
        legend(x="topleft",y=c("Mitja","Centroide clu. 1", "Centroide clu. 2", "Centroide clu. 3", "Centroide clu. 4"),col=c(1:5),lwd=2)
      }else{
        ggplot(incidencias_Departamentos,aes(x=datos_PDIA[[1]],y=incidencias_Departamentos[[1]]))+ scale_x_date()+labs(
          x = "Data",
          y = "Incidencia"
        ) + 
          geom_label(aes(x=datos_PDIA[[1]][400],y=200,
                         label = "Selecciona un departament"),
                     stat = "unique",
                     size = 5, color = "white", fill = "black") 
      }
    })
    
    grafica_incidencia_Bsp <- renderPlot({
      kmedias_Bsp<-kmeans.fd(suavizadoBspline$fd,ncl = K(),draw = FALSE,cluster.size = 3)
      par(mfrow=c(1,1)) 
      btn1 <- num_dep[input$Zona]
      if(btn1!=0){
        plot(suavizadoBspline$fd[btn1],col="yellow",lwd=3,ylim=c(0,450))
        lines(kmedias_Bsp$centers,lwd=2,col=c(2,3,4,5))
        legend(x="topleft",y=c("Mitja","Centroide clu. 1", "Centroide clu. 2", "Centroide clu. 3", "Centroide clu. 4"),col=c(1:5),lwd=2)
      }else{
        ggplot(incidencias_Departamentos,aes(x=datos_PDIA[[1]],y=incidencias_Departamentos[[1]]))+ scale_x_date()+labs(
          x = "Data",
          y = "Incidencia"
        ) + 
          geom_label(aes(x=datos_PDIA[[1]][400],y=200,
                         label = "Selecciona un departament"),
                     stat = "unique",
                     size = 5, color = "white", fill = "black") 
      }
    })
    
    output$graficaIncidencia_Fou <- grafica_incidencia_Fou
    
    output$graficaIncidencia_Bsp <- grafica_incidencia_Bsp
    
    
    ################################################
    #####         MAPAS CLUSTERING             #####
    ################################################
    
    mapa_kmedias_fou <- renderPlot({
      kmedias_Fou<-kmeans.fd(suavizadoFourier$fd,ncl = K(),draw = FALSE,cluster.size = 3)
      colores_Kmed_Fou<-rep("blue",24)
      colores_Kmed_Fou[kmedias_Fou()$cluster==2]<-"darkblue"
      colores_Kmed_Fou[kmedias_Fou()$cluster==3]<-"green"
      colores_Kmed_Fou[kmedias_Fou()$cluster==4]<-"red"
      ident_depart = num_dep[input$Zona]
      
      ggplot(data = departamentos_mapa) +
        geom_sf(aes(fill = colores_Kmed_Fou), color = "black") + 
        geom_sf(data = departamentos_mapa[ident_depart,],aes(fill = colores_Kmed_Fou[ident_depart]), color = "red") +
        ggtitle("Mapa de los Departamentos de Salud", subtitle = "Clasificación K-medias de los coeficientes de las Bases de Fourier") +
        guides(fill = guide_legend(title = "Clusters")) +
        scale_fill_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
    })
    
    mapa_kmedias_bsp <- renderPlot({
      kmedias_Bsp<-kmeans.fd(suavizadoBspline$fd,ncl = K(),draw = FALSE,cluster.size = 3)
      colores_Kmed_Bsl<-rep("blue",24)
      colores_Kmed_Bsl[kmedias_Bsp()$cluster==2]<-"darkblue"
      colores_Kmed_Bsl[kmedias_Bsp()$cluster==3]<-"green"
      colores_Kmed_Bsl[kmedias_Bsp()$cluster==4]<-"red"
      ident_depart = num_dep[input$Zona]
      
      ggplot() +
        geom_sf(data = departamentos_mapa,aes(fill = colores_Kmed_Bsl), color = "black") + 
        geom_sf(data = departamentos_mapa[ident_depart,],aes(fill = colores_Kmed_Bsl[ident_depart]), color = "red") +
        ggtitle("Mapa de los Departamentos de Salud", subtitle = "Clasificación K-medias de los coeficientes de las Bases B-splines") +
        guides(fill = guide_legend(title = "Clusters")) +
        scale_fill_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
    })
    
    output$mapa_kmeans_fou <- mapa_kmedias_fou
    
    output$mapa_kmeans_bsp <- mapa_kmedias_bsp
  
}

# Run the application 
shinyApp(ui = ui, server = server)


