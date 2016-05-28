#!/usr/bin/env Rscript
# -----------------------------------------------------------------------
#                   MODELO DE AUTOMATA CELULAR
# Codigo basado en:
# http://www.r-bloggers.com/cellular-automata-the-beauty-of-simplicity/
# -----------------------------------------------------------------------

# Se importa la libreria sp
library(sp)

# Defino las dimensiones de la muestra
width <- 1000
depth <- 100

# La clase GridTopology tiene como argumentos:
#             cellcentre = Coordenadas del centro
#             cellsize = el tamaño de cada celula
#             cells =  el tamaño de la muestra
gt = GridTopology(cellcentre=c(1,1),cellsize=c(1,1),cells=c(width, depth))

# SpatialGrid: class for defining a full, rectangular grid of arbitrary dimension
gt = SpatialGrid(gt)

# Guardo los datos en la variable z, con la opcion replace = True
z <- data.frame(status=sample(0:0, width, replace=T))

# Asigno un estado inicial setteando la variable Z
z[width/2, 1] <- 1
z[width/2+1, 1] <- 1

# Start the clock!
start.time <- Sys.time()

for (i in (width+1):(width*depth))
{
  ilf <- i-width-1
  iup <- i-width
  irg <- i-width+1
  if (i%%width==0) irg <- i-2*width+1
  if (i%%width==1) ilf <- i-1
  if((z[ilf,1]+z[iup,1]+z[irg,1]>0)&(z[ilf,1]+z[iup,1]+z[irg,1]<3))
  {st <- 1} else {st <- 0}
  nr<-as.data.frame(st)
  colnames(nr)<-c("status")
  z<-rbind(z,nr)
}

# Stop the Clock
end.time <- Sys.time()

print(end.time-start.time)

# Print
sgdf = SpatialGridDataFrame(gt, z)
image(sgdf, col=c("white", "black"))

