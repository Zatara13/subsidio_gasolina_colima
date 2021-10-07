## ¿Qué pasa al subsidiar al IEPS?
## Por Zatara

## Algunas personas creen erroneamente que "aumentar  el estímulo al IEPS" es "apoyar al pueblo". En realidad, se trata de una medida regresiva
## Que transfiere dinero público a las personas más acaudaladas. En estas visualizaciones de datos veremos quienes se benefician de este subsidio.


################### Librer?as de trabajo ###############
## FunciÃ³n para descargar paquetes en autom?tico
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


## Cargamos librer?as de trabajo
foo(c("readr", ## leer bases de datos en formato csv
      "tidyverse", ## manipulaci?n de bases de datos
      "ggsci", ## colores bonitos para gr?ficas I
      "ggthemes", ## Colors bonitos para gr?ficas II
      "srvyr", ## Estimaciones de encuestas
      "kableExtra", ## Crear tablas bonitas
      "readxl"
)
)

rm(foo)

## Datos de gastos por hogares
## 2020
## Concentrado hogar
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="concentradohogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "concentradohogar.csv")
unlink(td)

#Leer el archivo
concentrado_hogar_2020<-read.csv(fpath)

## Limpiar área de trabajo


## Para estimar el gasto por gasolinas, cargamos las bases de datos de gastopersonas y gastohogar
## Gasto hogar
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_gastoshogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="gastoshogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "gastoshogar.csv")
unlink(td)

#Leer el archivo
gastos_hogar_2020<-read.csv(fpath)

## Gasto personas
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_gastospersona_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="gastospersona.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "gastospersona.csv")
unlink(td)

#Leer el archivo
gastos_persona_2020<-read.csv(fpath)


rm(foo,
   fpath,
   td,
   tf,
   url)
################ Procesamiento de datos ##################
## Creamos un archivo con los gastos por hogar, teniendo en cuenta consumo de las gasolinas

gastos_hogar <- gastos_hogar_2020 %>% 
  mutate(
    numren = 999999
    ) %>% 
  select(
    ï..folioviv,
    foliohog,
    numren,
    clave,
    gasto_tri
  )

gastos_persona <- gastos_persona_2020 %>% 
  select(
    ï..folioviv,
    foliohog,
    numren,
    clave,
    gasto_tri
  )

gastos_gasolina <- bind_rows(gastos_hogar,
                             gastos_persona)

gastos_gasolina <- gastos_gasolina %>% 
  filter(clave %in% c("F007",
                      "F008",
                      "F009")) %>%
  mutate(clave = case_when(clave == "F007"~ "magna",
                           clave == "F008"~ "premium",
                           clave == "F009" ~ "diesel")) %>% 
  group_by(ï..folioviv,
            foliohog,
            clave) %>% 
  summarise(gastos_combustibles = sum(gasto_tri)) %>% 
  spread(clave,
         gastos_combustibles) %>% 
  mutate(diesel = replace_na(diesel, 0),
         magna = replace_na(magna, 0),
         premium = replace_na(premium,0))

concentrado_hogar_n <- full_join(concentrado_hogar_2020,
                                 gastos_gasolina,
                                 by = c("ï..folioviv",
                                        "foliohog")
                                 )
rm(concentrado_hogar_2020,
   gastos_gasolina)

concentrado_hogar_n <- concentrado_hogar_n %>% 
  select(ï..folioviv,
          foliohog,
          ubica_geo,
          upm,
          est_dis,
          factor,
          ing_cor,
          combus,
          magna,
          premium,
          diesel) %>% 
  mutate(diesel = replace_na(diesel, 0),
         magna = replace_na(magna, 0),
         premium = replace_na(premium,0)
         )


## Calculamos datos para Colima
## ENIGH 2020
enigh20_colima <- concentrado_hogar_n %>% 
  ## Filtramos para Colima
  filter(ubica_geo %in% c(6001,
                          6002,
                          6003,
                          6004,
                          6005,
                          6006,
                          6007,
                          6008,
                          6009,
                          6010)
         ) 

## Eliminamos archivos no necesarios
rm(
   concentrado_hogar_n
   )


##################### Deciles de Ingreso para Análisis ####################
## Identificamos deciles de ingreso en BD
## Para calcular los deciles de ingreso, utilicé la recomendación sugerida en el descriptor de cálculo diseñado por el INEGI
## Aquí está el enlace del documento base: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/doc/enigh2020_ns_descripcion_calculo_r.pdf
## Adapté el código de la página 7

## 2020
# Deja activa la tabla enigh20_colima
attach(enigh20_colima)
# Ordena Conc de acuerdo a ing_cor,folioviv,foliohog.
## NOTA: Por algún motivo extraño, mi computadora guarda la variable folioviv como ï..folioviv.
## Si el código te da problemas en la siguiente línea, cambia la variable ï..folioviv por folioviv
enigh20_colima<- orderBy(~+ing_cor+ï..folioviv+foliohog, data=enigh20_colima)
# Suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor)
# Se divide la suma de factores entre diez para sacar el tamaño del decil 
#(se debe de truncar el resultado quitando los decimales).
tam_dec<-trunc(tot_hogares/10)
# Muestra la suma del factor en variable hog.
enigh20_colima$tam_dec=tam_dec
# Se renombra la tabla concentrado a BD1.
BD1 <- enigh20_colima
# Dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# Se ordena de menor a mayor segun la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# Se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
### Entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9){
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])
                                                                  [1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9){
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10

enigh20_colima <- BD1

rm(BD1,
   a1,
   b1,
   i,
   tam_dec,
   tot_hogares)

######################## Estimación de beneficios ######################
## Identificamos hogares que gastan en gasolina, y sexo de jefe de los hogares
enigh20_colima <- enigh20_colima %>% 
  ## Creamos columnas con variables categoricas útiles
  mutate(gasto_gasolina =  case_when(combus > 0 ~ 1, 
                                     TRUE ~ 0))
## Calculamos un "subsidio"
## Para hacerlo, necesitamos calcular consumo de magnum, premium y diesel, y por cada litro consumido calcular el impuesto.
## Después, vamos a multiplicar los impuestos pagados por el subsidio que se dará
## Utilizaremos el precio al 7 de octubre para el municipio de Colima, tomados de aquí: https://gasolinamexico.com.mx/estados/colima/
enigh20_colima_subsidio <- enigh20_colima %>% 
  mutate(consumo_magna = magna / 20.22,
         consumo_premium = premium / 22.27,
         consumo_diesel = diesel / 21.77) %>% 
  mutate(impuesto_magna = consumo_magna * 5.1148,
         impuesto_premium = consumo_premium * 4.3192,
         impuesto_diesel = consumo_diesel * 5.6212) %>% 
  mutate(subsidio_magna = impuesto_magna * 0.5350,
         subsidio_premium = impuesto_premium * 0.2575,
         subsidio_diesel = impuesto_diesel * 0.3937) %>% 
  mutate(subsidio_hogar = subsidio_magna + subsidio_premium + subsidio_diesel)


## Hacemos el diseño muestral
## Diseño muestral 2020
design20 <- enigh20_colima_subsidio %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

rm(enigh20_colima_subsidio,
   enigh20_colima)

## Gasto medio en gasolina por deciles de ingreso (la media se calcula sobre quienes gastan en gasolina, no sobre el decil)
gastomedio_gasolinas <- design20 %>% 
  filter(gasto_gasolina == 1) %>% 
  group_by(DECIL) %>% 
  summarise(gasto_combustible=survey_mean(combus,
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(gasto_combustible_cv=
           gasto_combustible_cv*100,
         DECIL = as.factor(DECIL)
  )

## Porcentaje de hogares que gastan en gasolina
hogs_decil_20 <- design20 %>% 
  group_by(DECIL) %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  )


## Hogares que gastaban en gasolina
gastan_gasolina <- design20 %>%
  filter(gasto_gasolina==1)%>%
  group_by(DECIL) %>% 
  summarise(gasto_gasolina=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(gasto_gasolina_cv=
           gasto_gasolina_cv*100,
  )

## Unimos tablas para calculas porcentajes
gasto_gasolina_pct <- merge(hogs_decil_20,
                          gastan_gasolina,
                          by = "DECIL") %>% 
  mutate(pct_hog_gg = gasto_gasolina / hogares * 100) %>% 
  mutate(año = 2020,
         DECIL = as.factor(DECIL)) %>% 
  select(año,
         DECIL,
         pct_hog_gg,
         gasto_gasolina_cv)


## Subsidio medio por deciles
subsidio_deciles <- design20 %>% 
  filter(gasto_gasolina == 1) %>% 
  group_by(DECIL) %>% 
  summarise(subsidio_hogar=survey_mean(subsidio_hogar,
                                          vartype = c("cv", "ci"),
                                          level=0.95))%>%
  mutate(subsidio_hogar_cv=
           subsidio_hogar_cv*100,
         DECIL = as.factor(DECIL)
  )

## Tamaño de transferencia

tam_transferencia <- merge(subsidio_deciles,
                           gastan_gasolina,
                           by = "DECIL") %>% 
  mutate(tamaño_transferencia = subsidio_hogar * gasto_gasolina,
         DECIL = as.factor(DECIL)) %>% 
  select(DECIL,
         tamaño_transferencia) %>% 
  arrange(DECIL)

## Calculamos porcentaje de ingresos gastado en gasolinas
## Calculamos primero el ingreso medio
ingreso_medio <- design20 %>% 
  group_by(DECIL) %>% 
  summarise(ingreso_medio=survey_mean(ing_cor,
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(ingreso_medio_cv=
           ingreso_medio_cv*100
  )

## Porcentaje de gasto en gasolina
pct_ingmed_gasolina <- merge(
  ingreso_medio,
  gastomedio_gasolinas,
  by = c("DECIL")
) %>% 
  mutate(pct_ing_gas = gasto_combustible / ingreso_medio * 100,
         pct_ing_gas_low = gasto_combustible_low / ingreso_medio * 100,
         pct_ing_gas_upp = gasto_combustible_upp / ingreso_medio * 100,
         pct_ing_gas_cv = gasto_combustible_cv) %>% 
  select(DECIL,
         pct_ing_gas,
         pct_ing_gas_low,
         pct_ing_gas_upp,
         pct_ing_gas_cv)

################ Visualización de datos #############
## Gráfica de barras: Porcentaje de hogares que gastan en gasolina
gasto_gasolina_pct %>%
  ggplot(aes(x = DECIL,
             y = pct_hog_gg)
         ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5) ## Alineamos el título al centro
        ) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  scale_x_discrete(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"))+
  labs(title = "Porcentaje de hogares que gastan trimestralmente en gasolina",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Porcentaje de hogares",
       caption = "Fuente: ENIGH 2020, INEGI.
       Elaborado por @jkvisfocri")

## Gasto medio en gasolinas
gastomedio_gasolinas %>%
  ggplot(aes(x = DECIL,
             y = gasto_combustible)
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5) ## Alineamos el título al centro
        ) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  scale_x_discrete(breaks = c(1:10),
                   labels = c("I",
                              "II",
                              "III",
                              "IV",
                              "V",
                              "VI",
                              "VII",
                              "VIII",
                              "IX",
                              "X"))+
  labs(title = "Gasto medio trimestral en combustibles por decil de ingresos",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Gasto medio trimestral",
       caption = "Fuente: ENIGH 2020, INEGI.
       Elaborado por @jkvisfocri")

## Porcentaje de ingreso medio empleado en gasolinas
pct_ingmed_gasolina %>% 
  ggplot(aes(x = DECIL,
             y = pct_ing_gas)
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5) ## Alineamos el título al centro
  ) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  scale_x_discrete(breaks = c(1:10),
                   labels = c("I",
                              "II",
                              "III",
                              "IV",
                              "V",
                              "VI",
                              "VII",
                              "VIII",
                              "IX",
                              "X"))+
  labs(title = "Porcentaje de ingresos empleados en comprar combustibles",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Porcentaje de ingresos",
       caption = "Fuente: ENIGH 2020, INEGI.
       Elaborado por @jkvisfocri")


## Subsidios
## Subsidio medio por hogar

## Gasto medio en gasolinas
subsidio_deciles %>%
  ggplot(aes(x = DECIL,
             y = subsidio_hogar)
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5) ## Alineamos el título al centro
  ) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  scale_x_discrete(breaks = c(1:10),
                   labels = c("I",
                              "II",
                              "III",
                              "IV",
                              "V",
                              "VI",
                              "VII",
                              "VIII",
                              "IX",
                              "X"))+
  labs(title = "Subsidio medio al hogar por deciles de ingresos",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Pesos",
       caption = "Fuente: ENIGH 2020, INEGI.
       Elaborado por @jkvisfocri")

## Tamaño de la transferencia por deciles
tam_transferencia %>%
  mutate(tamaño_transferencia = tamaño_transferencia / 1000) %>% 
  ggplot(aes(x = DECIL,
             y = tamaño_transferencia)
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5) ## Alineamos el título al centro
  ) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  scale_x_discrete(breaks = c(1:10),
                   labels = c("I",
                              "II",
                              "III",
                              "IV",
                              "V",
                              "VI",
                              "VII",
                              "VIII",
                              "IX",
                              "X"))+
  labs(title = "Tamaño del estímulo fiscal a gasolinas por deciles de ingresos",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Miles de pesos",
       caption = "Fuente: ENIGH 2020, INEGI.
       Elaborado por @jkvisfocri")
