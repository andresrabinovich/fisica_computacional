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
datos_totales <- data.frame(p=numeric(0), L=character())
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  datos <- read_csv(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1a/", i, ".txt", sep=""), col_names = FALSE)
  datos_totales <- rbind(datos_totales, cbind(datos, rep(i, nrow(datos))))
  print(mean(datos$X1))
  print(sd(datos$X1))
}
colnames(datos_totales) <- c("p", "L")

ggplot(datos_totales, aes(x=p, fill=L)) + 
  geom_histogram(alpha=0.5, position="identity", breaks=seq(min(datos_totales$p), max(datos_totales$p), by = (max(datos_totales$p)-min(datos_totales$p))/50)) + 
  labs(x="Pc", y="Frecuencia") +
  theme_original()

#ej1b
datos_totales <- data.frame(p=numeric(0), P=numeric(0), sd=numeric(0), L=character())
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  datos <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1b/", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
  
  #Los datos se guardaron de una forma rara así que recupero las cuentas originales
  #y calculo el promedio
  datos[, -1] <- datos[, -1]*10
  datos[, 2]  <- rowSums(datos[, -1])/10000

  #Tomamos el valor de Pc como el valor de p tq la mitad de los clusters son percolantes
  percolante <- min(which(datos[, 2] >= 0.5))
  
  #El error lo tomo como el error en una binomial de probabilidad datos[, 2]
  #con un ensayo de Bernoulli y con n muestras
  q  <- 1-datos[, 2]
  p  <- datos[, 2]
  n  <- 10000
  k  <- 1
  sd <- sqrt(k*q*p/n)
  
  datos_totales <- rbind(datos_totales, data.frame(p=datos[, 1], P=datos[, 2], sd=sd, L=rep(i, nrow(datos))))
  print(datos[percolante, 1])
  print(sd[percolante])
}
ggplot(datos_totales) + geom_point(aes(y=P, x=p, color = L), size=1) +
  #geom_ribbon(aes(ymin=(P-sd), ymax=(P+sd), x=p), alpha = 0.0, fill = "blue") +
  labs(x="p", y="Fracción percolante") +
  theme_original()


#ej1c
pc = 0.59274
datos <- data.frame(L=c(4, 8, 16, 20, 32, 40, 50, 64, 70, 80, 90, 100, 128, 200, 256), 
                    mu=c(0.5639, 0.5815, 0.5890, 0.5899, 0.5924, 0.5918, 0.5920, 0.5923, 0.5923, 0.5925, 0.5922, 0.5926, 0.5925, 0.5926, 0.5927))
datos <- datos[6:14, ]
datos$mu <- abs(datos$mu-pc)/pc
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

datos <- data.frame(L=c(4, 8, 16, 32, 64, 128), 
                    mu=c(0.569569, 0.584584, 0.59059, 0.593593, 0.592592, 0.593593))
datos <- datos[1:6, ]
datos$mu <- abs(datos$mu-pc)/pc
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

ggplot(datos, aes(x=L,y=mu)) + geom_point() + stat_smooth(method = "lm", col = "red", se=FALSE, size=0.5) +
  labs(x="log(L)", y="log(|<Pc>l-<Pc>inf|)") +
  theme_original()

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

for(i in c(16, 32, 64, 128)[-5]){
  datos <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1d/ej1dbis", i, "x", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, na = "na"))
  colnames(datos) <- c("p", "s", "ns")
  datos$ns <- datos$ns/(i*i*10000)
  datos[, -1] <- log(datos[, -1])
  plot(datos[, -1])
  datos <- datos[datos$s > 3 & datos$s < 4, ]
  
  R <- matrix(ncol=3, nrow=0)
  for(p in unique(datos[, 1])){
    datos_filtrados <- datos[which(datos[, 1] == p), ]
    plot(datos_filtrados$s, datos_filtrados$ns)
    abline(lm(ns ~ s, datos_filtrados))
    s <- summary(lm(ns ~ s, datos_filtrados))
    R <- rbind(R, c(-1*s$coefficients["s", 1], s$coefficients["s", 2], s$r.squared))
  }
  colnames(R) <- c("Tau", "Error", "R2")
  mejor_valor <- which.max(R[, "R2"])
  R[mejor_valor, c("Tau", "Error")]
  unique(datos[, 1])[mejor_valor]
  plot(R[, "Tau"], R[, "R2"])
}

#ej1dbis
datos <- as.data.frame(read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1d/ej1dbis.txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, na = "na"))
colnames(datos) <- c("s", "ns")
datos$ns <- datos$ns/(64*64*30000)
datos <- log(datos)
plot(datos)
datos_filtrados <- datos[datos$s > 2 & datos$s < 5, ]
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
  
  a<-data.frame(epsilon=(datos$X1), P=m)
  plot(a)
  b<-a[a$epsilon > 0.4 & a$epsilon < 0.8, ]
  plot(b)
  
  plot(log(b$epsilon), log(b$P))
  b <- b[log(b$epsilon) < -0.5 & log(b$epsilon) > -0.55, ]
  plot(log(b))
  lm(log(P) ~ log(abs(epsilon-0.5925)), as.data.frame(b))
  
  plot(log(abs(b$epsilon-0.5925)), log(b$P))
  
  datos <- data.frame(x=datos[, 1], y=m, l=m-s, u=m+s)
  plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=x), colour = "black") +
    geom_ribbon(aes(ymin=l, ymax=u, x=x), alpha = 0.3, fill = "red") +
    labs(x="p", y="Intensidad Pc") +
    theme_original()
  print(plot_datos)
}

#ej3
datos <- as.data.frame(read_delim("~/fisica_computacional/percolacion/programacion/corridas/ej3/ej3.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
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
datos           <- as.data.frame(read.delim("~/fisica_computacional/percolacion/programacion/corridas/ej4/masas.txt"))
colnames(datos) <- c("p", "s", "ns")

#Sacamos los fragmentos tales que 0.01<s/s0<0.12
#p <- sample(unique(datos$p), replace = FALSE, 500)
p <- unique(datos$p)
p<-p[p>0.4 & p <0.7]
datos_filtrados <- data.frame(p=NULL, s=NULL, ns=NULL)
for(i in p){
  m               <- datos[which(datos$p == i), ]
  datos_filtrados <- rbind(datos_filtrados, m[(m$s/max(m$s)) > 0.01 & (m$s/max(m$s)) < 0.12, ])
}
datos_filtrados2          <- datos_filtrados
datos_filtrados           <- datos_filtrados2
tau                       <- 1.74
sigma                     <- 36/91
pc                        <- 0.5927
q0                        <- exp(1)^(-4)
#datos_filtrados           <- datos_filtrados[sample(1:nrow(datos_filtrados), replace = FALSE, 1000), ]
datos_filtrados$ns        <- datos_filtrados$ns/(64*64*30000)
#datos_filtrados$s         <- datos_filtrados$s/(64*64*30000)
s                         <- unique(datos_filtrados$s)
nsc                       <- q0*s^(-tau)
names(nsc)                <- s
epsilon                   <- (datos_filtrados$p-pc)/pc
y                         <- datos_filtrados$ns/nsc[as.character(datos_filtrados$s)]
z                         <- (datos_filtrados$s^sigma)*epsilon
plot(z, y)
ggplot(data.frame(z=z, nu=(y))) + geom_point(aes(x=z, y=nu), size=0.1)

#ej5
datos              <- as.data.frame(read.delim("~/fisica_computacional/percolacion/programacion/corridas/ej4/masas.txt"))
colnames(datos)    <- c("p", "s", "ns")
datos_filtrados    <- datos[datos$s > 1 & datos$s <= 15, ]
datos_filtrados$ns <- datos_filtrados$ns/(64*64*30000)
#datos_filtrados    <- datos_filtrados[datos_filtrados[, 1] > 0.58 & datos_filtrados[, 1] < 0.6, ]
pc                 <- 0.5927
datos_filtrados$e  <- (datos_filtrados$p-pc)/pc
for(i in 2:15){
  x <- which(datos_filtrados$s == i)
  #plot(epsilon[x], datos_filtrados[x, 3])
}
#datos_filtrados$s <- as.factor(datos_filtrados$s)
ggplot(datos_filtrados) + geom_point(aes(x=e, y=ns, color = as.factor(s)))

maximos <- aggregate(ns ~ s, datos_filtrados, FUN = max)
maximos <- merge(maximos, datos_filtrados)
maximos[order(maximos$s), ]
y<-log(abs(maximos$p-pc))
x<-log(maximos$s)
datos<-data.frame(x=x, y=y)
ggplot(datos, aes(x=x,y=y)) + geom_point() + stat_smooth(method = "lm", col = "red", se=FALSE, size=0.5) +
  labs(x="log(s)", y="log(|<Pmax>-<Pc_L>|)") +
  theme_original()
#plot(datos$L, datos$mu)
fiteo<-lm(y~x, data=datos)
#abline(fiteo)
summary(fiteo)

#ej6
tamano             <- c(6, 32, 128, 256)
red                <- 4
datos              <- as.data.frame(read.delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej6/masas", tamano[red], ".txt", sep="")))
colnames(datos)    <- c("p", "s", "ns")
datos2             <- as.data.frame(read.delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej6/masas2", tamano[red], ".txt", sep="")))
colnames(datos2)   <- c("p", "s", "ns")
datos              <- rbind(datos, datos2)
datos              <- cbind(1:nrow(datos), datos)
colnames(datos)    <- c("id", "p", "s", "ns")
pc                 <- c(0.5743, 0.5924, 0.5925, 0.5927)

#Nos quedamos solo con los p tq 0.4<p<0.7
datos_filtrados    <- datos[datos$p > 0.4 & datos$p < 0.7, ]

#Nomalizamos ns
datos_filtrados$ns <- datos_filtrados$ns/(tamano[red]*tamano[red]*10000)

#Agregamos el s^2
datos_filtrados$s2 <- datos_filtrados$s^2

#Agregamos el producto de ns con s2 para cada termino
datos_filtrados$nss2 <- datos_filtrados$ns*datos_filtrados$s2

#Calculamos el segundo momento por cada p
M2                 <- aggregate(nss2 ~ p, datos_filtrados, FUN = sum)

#Agregamos epsilon
M2$epsilon         <- (M2$p - pc[red])/pc[red]

#Obtenemos el máximo
maximo <- which.max(M2$nss2)

#Mostramos las curvas
ggplot(M2) + geom_point(aes(x=epsilon, y=nss2))

plot(abs(M2$epsilon), M2$nss2)
plot(log(abs(M2$epsilon)), log(M2$nss2))
points(M2$epsilon[maximo], M2$nss2[maximo], col="red")

plot(log(abs(M2$epsilon)), log(M2$nss2))

#Pruebas
plot(log(abs(M2$epsilon))[maximo:(maximo+50)], log(M2$nss2)[maximo:(maximo+50)])

for(i in 1:20){
  points(log(abs(M2$epsilon))[maximo+i], log(M2$nss2)[maximo+i], col="red")
  abline(lm(log(nss2)~log(abs(epsilon)), M2[(maximo+i):(maximo+35), ]), col="red")
  print(summary(lm(log(nss2)~log(abs(epsilon)), M2[(maximo):(maximo+i), ]))$r.squared)
  print(i)
}

cor <- matrix(ncol=3, nrow=0)
for(j in 2:30){
  corrimiento_izquierda <- 1*j
  for(i in (-1*corrimiento_izquierda+1):(-1*corrimiento_izquierda+40)){
    cor <- rbind(cor, c(i, j, summary(lm(log(nss2)~log(abs(epsilon)), M2[(maximo+corrimiento_izquierda):(maximo+i), ]))$r.squared))

    #cor <- rbind(cor, c(i, j, summary(lm(log(nss2)~log(abs(epsilon)), M2[(maximo-i):(maximo+corrimiento_izquierda), ]))$r.squared))
  }
}
plot(log(abs(M2$epsilon))[maximo:(maximo-50)], log(M2$nss2)[maximo:(maximo-50)])

#Fiteamos gamma tomando el máximo y n puntos a derecha y despues n puntos a izquierda, todo log
gamma <- data.frame(cantidad_puntos=numeric(0), gamma=numeric(0), direccion=character())
corrimiento_derecha <- 15
corrimiento_izquierda <- -30
for(i in 1:35){
  #abline(lm(log(nss2)~log(abs(epsilon)), M2[maximo:(maximo+i), ]), col="red")
  #abline(lm(log(nss2)~log(abs(epsilon)), M2[maximo:(maximo-i), ]), col="blue")
  gamma <- rbind(gamma, data.frame(cantidad_puntos=i, gamma=coef(lm(log(nss2)~log(abs(epsilon)), M2[(maximo+corrimiento_derecha):(maximo+corrimiento_derecha+i), ]))[2], direccion="derecha"))
  gamma <- rbind(gamma, data.frame(cantidad_puntos=i, gamma=coef(lm(log(nss2)~log(abs(epsilon)), M2[(maximo+corrimiento_izquierda-i):(maximo+corrimiento_izquierda), ]))[2], direccion="izquierda"))
}

#Graficamos los distintos valores de gamma
ggplot(gamma) + geom_point(aes(x=cantidad_puntos, y=gamma, color = direccion))

#Encontramos la intersección y el valor de gamma
gamma[35, "gamma"]
