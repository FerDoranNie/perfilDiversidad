####################################
#Creado por Fernando Dorantes Nieto
#                                   <(°) 
#                                     ( >)"
#                                      /|
####################################


##### En caso de que los siguientes paquetes no estén instalados
##### Se pueden instalar de la siguiente manera
##### install.packages("nombre_del_paquete_a_instalar", dep=TRUE)


# Librerías ---------------------------------------------------------------
library(iNEXT) #### estimaciones
library(dplyr) #### manejo de datos
library(lattice) #### gráficos cool
library(ggplot2) #### gráficos cool pero me gusta más lattice XD


# Sembrando el directorio de trabajo --------------------------------------

setwd("~/ReposDesarollo/perfilDiversidad/") ### directorio de trabajo
##### esta dirección cambia con respecto a donde usted guarda los archivos

aranias <- read.csv("datos/data_Spider.csv", header = TRUE, 
                 stringsAsFactors = FALSE)


### visión preliminar de las columnas de los datos
names(aranias)


###Para hacer el estimador de abundancia necesitamos obtener las abundancias de todas -
### las especies 
### usando R Base esto puede ser así

aranias[,4:length(aranias)] #### Con este se obtienen las columnas de las especies

### Pero si usamos la librería DPLYR se eliminan las columnas no deseadas por su nombre y se dejan solo las especies
especies <- select(aranias, -mes, -sitio, -transecto)

numerosEspecies <- colSums(especies) ##### sumamos las columnas para obtener abundancias totales


# Curva de rango abundancia -----------------------------------------------
##### Hagamos una curva de rango-abundancia
nombresEspecies <- names(numerosEspecies)
numerosEspecies <- as.numeric(numerosEspecies)

#### se vuelve dataframe
acumuladoEspecies <- data.frame(nombres= nombresEspecies, cantidad= numerosEspecies) 

#### ordenamos en base a la cantidad
acumuladoEspecies <- acumuladoEspecies[with(acumuladoEspecies, 
                                            order(cantidad, decreasing = T)),]
  

##### le añadimos el rango
acumuladoEspecies$rango <- 1:length(acumuladoEspecies$nombres)

####### graficamos
ggplot(acumuladoEspecies, aes(x = rango, y=cantidad ))+
  geom_line(color='steelblue')+
  geom_point(color='steelblue')+
  theme_classic()+
  geom_text(aes(x = rango, y=cantidad, label=nombres),
            hjust=-0.25, vjust=0.5, angle=45, size=3)+
  scale_x_continuous(limits=c(1,37))+
  scale_y_continuous(limits=c(0,400))+
  xlab("Rango")+
  ylab("Abundancia")




# Perfiles de diversidad --------------------------------------------------

### Para estimar solo necesitamos los datos de abundancia, si lo sé que aburrido que lo haga todo automático
estimadores <- iNEXT(numerosEspecies, datatype = "abundance")

##### lo que hace INEXT es dar como resultado una lista de dataframes 
##### la que nos interesa es la que se llama AsyEst

diversidadEstimador <- estimadores$AsyEst
diversidadEstimador <- as.data.frame(diversidadEstimador)
nombresDiversidad <- rownames(diversidadEstimador)

diversidadEstimador <- data.frame(names= nombresDiversidad, diversidadEstimador)

rownames(diversidadEstimador)<- NULL
### Filtramos todos los estimadores que queremos
diversidadEstimador$orden <- c("Q0", "Q1", "Q2")
diversidadEstimador$ordenNumerico <- 1:3

diversidadEstimador <- select(diversidadEstimador, names, orden, ordenNumerico,
                              Estimator, X95..Lower, X95..Upper)


#### Recordemos que Q0, Q1 Y Q2 corresponden a sus respectivos análisis entonces

#### Vamos a graficar los estimadores con sus intervalos de confianza al 95%

ggplot(diversidadEstimador, aes(x = ordenNumerico, y= Estimator))+
  geom_point(shape="*", size=8)+
  geom_line()+
  theme_classic()+
  scale_x_continuous(breaks = 1:3, labels = c("Q0", "Q1", "Q2"))+
  xlab("Órdenes de diversidad")+
  ylab("Estimadores")+
  geom_text(aes(x = ordenNumerico, y= Estimator, label=Estimator),
            hjust=0.5, vjust=-0.5, angle=-15)+
  geom_errorbar(aes(ymin=X95..Lower,ymax=X95..Upper),
                linetype="dotted")+
  scale_y_continuous(breaks = seq(0,65, 5), limits = c(0, 65))










