#Cargamos las librerías que vamos a usar y generamos el theme para los plots
library(ggplot2)
library(readr)
theme_original <- function (base_size = 20, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill="#e1e1e1"),
      panel.grid.minor = element_line(size=0.1, color="white"),
      panel.grid.major = element_line(size=0.2, color="white"),
      text = element_text(family = base_family, face = "plain",
                          color = "black", size = base_size,
                          hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = margin(), debug = FALSE)
    )
}

#ej1a
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  datos <- read_csv(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1a/", i, ".txt", sep=""), col_names = FALSE)
  plot_datos <- ggplot(data=datos, aes(datos$X1)) + 
  geom_histogram(breaks=seq(min(datos$X1), max(datos$X1), by = (max(datos$X1)-min(datos$X1))/20), col="black", fill="white") + 
  #labs(title=paste("Histograma de Pc para 1000 realizaciones de una red cuadrada de ", i, sep="")) +
  labs(x="Pc", y="Frecuencia") +
  theme_original()
  print(plot_datos)
  print(mean(datos$X1))
  print(sd(datos$X1))
}

#ej1b
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  n = 10000
  datos <- read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1b/", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  datos[, -1] <- datos[, -1]/100
  datos[, 2] <- rowSums(datos[, -1])/10
  q  <- 1-datos[, 1]
  k  <- datos[, 2]
  sd <- sqrt(k*q*datos[, 1]/n)
  p  <- datos[min(which(k >= 0.5)), 1]
  e  <- sd[min(which(k >= 0.5)), 1]
  
  #p<-apply(datos[, -1], 2, function(x){
  #  min(which(x >= 0.5))
  #})
  #p<-c(t(datos[p, 1]))
  #print(mean(p))
  #print(sd(p))
  #write.table(as.data.frame(datos), file = paste("~/fisica_computacional/percolacion/programacion/corridas/ej1b/a", i, ".txt", sep=""), sep = "\t", col.names = FALSE, row.names = FALSE)
  #m     <- apply(datos[, -1], 1, mean)
  #s     <- apply(datos[, -1], 1, sd)
  datos <- data.frame(x=datos[, 1], y=datos[, 2], l=datos[, 2]-sd, u=datos[, 2]+sd)
  colnames(datos) <- c("x", "y", "l", "u")
  plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=x), colour = "black") +
    geom_ribbon(aes(ymin=l, ymax=u, x=x), alpha = 0.3, fill = "red") +
    labs(x="p", y="Fracción percolante") +
    theme_original()
  print(plot_datos)
  print(p)
  print(e)
}

#ej1c
pc = 0.5928 
datos <- data.frame(L=c(4, 8, 16, 64, 128, 200, 256), mu=c(0.5639, 0.5815, 0.5890, 0.5922, 0.5925, 0.5926, 0.5927))
datos$mu <- abs(datos$mu-pc)
datos <- log(datos)
ggplot(datos, aes(x=L,y=mu)) + geom_point() + stat_smooth(method = "lm", col = "red", se=FALSE, size=0.5) +
labs(x="log(L)", y="log(|<Pc_L>-<Pc_inf>|)") +
  theme_original()
#plot(datos$L, datos$mu)
fiteo<-lm(mu~L, data=datos)
#abline(fiteo)
summary(fiteo)
nu <- -1/coef(fiteo)[2]
nu
#ej1d
for(i in c("16x16", "32x32", "64x64", "128x128")){
  datos   <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1d/", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, na = "na"))

  #m<-apply(datos[, -1], 1, function(x){
  #  median(as.numeric(x[!is.na(x)]))
  #})
  
  #s<-apply(datos[, -1], 1, function(x){
  #  sd(as.numeric(x[!is.na(x)]))
  #})
  
  m<-apply(datos[, -1], 2, function(x){
    which(x == max(x))
  })
  m<-lapply(m, function(x){
    median(datos[x, 1])
  })
  m <- unlist(m)
  m <- m[!is.nan(m)]
  print(median(m, na.rm = TRUE))
  print(sd(m, na.rm = TRUE))
  

  #s<-apply(datos[, -1], 1, function(x){
  #  sd(as.numeric(x[!is.na(x)]))
  #})
  #datos <- data.frame(x=datos[, 1], y=m, l=m-s, u=m+s)
  #plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=X1), colour = "black") +
  #  geom_ribbon(aes(ymin=l, ymax=u, x=X1), alpha = 0.3, fill = "red") +
  #  labs(x="p", y="r2") +
  #  theme_original()
  #print(plot_datos)
  #print(datos[which.max(m), 1])
}

#ej1dbis
datos <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1d/ej1dbis.txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, na = "na"))
colnames(datos) <- c("s", "ns")
datos <- datos/(1024*1024*30000)
datos <- log(datos)
plot(datos)
datos_filtrados <- datos[datos$s > -21 & datos$s < -18, ]
plot(datos_filtrados)
fiteo<-lm(ns ~ s, datos_filtrados)
abline(fiteo)
summary(fiteo)

#tau <- c()
#fin <- max(unique(datos[, 1]))

#datos <- datos[datos[, 2] > 1, ] #Sacamos los clusters de 1 solo elemento

#Sacamos los clusters percolantes de cada realización para que no metan ruido
#d<-data.frame(x=datos[, 1], y=datos[, 2])
#a<-aggregate.data.frame(d, list(d$x), function(z){
#  z[z<max(z)]
#})

#datos <- unlist(apply(a, 1, function(z){ 
#  datos[which(datos[, 1] == z["Group.1"] & datos[, 2] != z["y"]), 2]
#}))

#datos <- unlist(a$y)
#datos <- datos[, 2]
#datos_crudos <- table(datos/(800*800*30000))
#datos_filtrados <- datos_crudos
#datos_filtrados <- datos_filtrados[log(as.numeric(names(datos_filtrados))) < -17 & log(as.numeric(names(datos_filtrados))) > -20]

#plot(log(as.numeric(names(datos_filtrados))), log(datos_filtrados))
#fiteo<-lm(y.Freq ~ x, data.frame(x = log(as.numeric(names(datos_filtrados))), y = log(datos_filtrados)))
#abline(fiteo)
#summary(fiteo)

#ej2
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  datos <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej2/", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, na = "na"))
  m<-apply(datos[, -1], 1, function(x){
    mean(x)
  })
  s<-apply(datos[, -1], 1, function(x){
    sd(x)
  })
  
  datos <- data.frame(x=datos[, 1], y=m, l=m-s, u=m+s)
  plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=x), colour = "black") +
    geom_ribbon(aes(ymin=l, ymax=u, x=x), alpha = 0.3, fill = "red") +
    labs(x="p", y="Intensidad Pc") +
    theme_original()
  print(plot_datos)
}

#ej3
datos <- as.data.frame(read_delim("~/fisica_computacional/percolacion/programacion/corridas/ej3/ej3.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
#m<-apply(datos[, -1], 2, function(z){
tau <- c()
for(i in 2:nrow(datos)){
  z <- datos[, i]
  x <- datos[z>0, 1]
  y <- z[z>0]
  a<-lm(y~x, data.frame(x=log(x), y=log(y)))
  tau <- c(tau, a$coefficients[2])
  #mean(x)
}#)
mean(tau)
sd(tau)

m<-apply(datos[, -1], 1, function(x){
  x <- x[x>0]
  mean(x)
})
s<-apply(datos[, -1], 1, function(x){
  x <- x[x>0]
  sd(x)
})

datos <- data.frame(x=log(datos[, 1]), y=log(m), l=log(m-s), u=log(m+s))
summary(lm(y~x, datos))
plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=x), colour = "black") +
  geom_ribbon(aes(ymin=l, ymax=u, x=x), alpha = 0.3, fill = "red") +
  labs(x="log(L)", y="log(Masa Pc)") +
  theme_original()
print(plot_datos)
plot(datos$x, datos$y)
abline(lm(y~x, datos))

#ej4
datos           <- as.data.frame(read_delim("~/fisica_computacional/percolacion/programacion/corridas/ej4/masas.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
colnames(datos) <- c("p", "s", "ns")

#Sacamos los fragmentos tales que 0.01<s/s0<0.12
p <- unique(datos$p)
datos_filtrados <- data.frame(p=NULL, s=NULL, ns=NULL)
for(i in p){
  m               <- datos[which(datos$p == i), ]
  datos_filtrados <- rbind(datos_filtrados, m[(m$s/max(m$s)) > 0.01 & (m$s/max(m$s)) < 0.12, ])
}
tau                       <- 1.98
sigma                     <- 36/91
pc                        <- 0.5927
datos_filtrados$ns        <- datos_filtrados$ns/(64*64*30000)
s                         <- unique(datos_filtrados$s)
nsc                       <- s^(-tau)
names(nsc)                <- s
epsilon                   <- (datos_filtrados$p-pc)/pc
y                         <- datos_filtrados$ns/nsc[as.character(datos_filtrados$s)]
z                         <- datos_filtrados$s^sigma*epsilon
plot(z, y)



tau             <- 1.98
sigma           <- 36/91
pc              <- 0.5927
datos <- datos[which(datos[, 1] == unique(datos[, 1][300])), ]
datos[, 3]      <- datos[, 3]/(64*64*30000)
s               <- 2:max(datos$s)
nsc             <- s^(-tau)
names(nsc)      <- s
epsilon         <- ((datos$p-pc)/pc)
y               <- datos$ns/nsc[as.character(datos$s)]
z               <- datos$s^sigma*epsilon
plot(z, y)
#datos <- datos[datos[, "s"]>=2 & datos[, "s"]<=15, ] #Nos quedamos con los s entre 2 y 15
#datos <- datos[datos[, "s"]>=2 & datos[, "s"]<=15, ] #Nos quedamos con los p entre 2 y 15
#m<-apply(datos[, -1], 2, function(z){
for(i in 2:nrow(datos)){
  z <- datos[, i]
  x <- datos[z>0, 1]
  y <- z[z>0]
  a<-lm(y~x, data.frame(x=log(x), y=log(y)))
  tau <- c(tau, a$coefficients[2])
  #mean(x)
}#)
mean(tau)
sd(tau)

m<-apply(datos[, -1], 1, function(x){
  x <- x[x>0]
  mean(x)
})
s<-apply(datos[, -1], 1, function(x){
  x <- x[x>0]
  sd(x)
})

datos <- data.frame(x=log(datos[, 1]), y=log(m), l=log(m-s), u=log(m+s))
summary(lm(y~x, datos))
plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=x), colour = "black") +
  geom_ribbon(aes(ymin=l, ymax=u, x=x), alpha = 0.3, fill = "red") +
  labs(x="log(L)", y="log(Masa Pc)") +
  theme_original()
print(plot_datos)
plot(datos$x, datos$y)
abline(lm(y~x, datos))
