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
  datos <- read_delim(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1b/", i, ".txt", sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  datos[, -1] <- datos[, -1]/100
  p<-apply(datos[, -1], 2, function(x){
    min(which(x >= 0.5))
  })
  p<-c(t(datos[p, 1]))
  print(mean(p))
  print(sd(p))
  #write.table(as.data.frame(datos), file = paste("~/fisica_computacional/percolacion/programacion/corridas/ej1b/a", i, ".txt", sep=""), sep = "\t", col.names = FALSE, row.names = FALSE)
  m     <- apply(datos[, -1], 1, mean)
  s     <- apply(datos[, -1], 1, sd)
  datos <- data.frame(x=datos[, 1], y=m, l=m-2*s, u=m+2*s)
  plot_datos <- ggplot(datos) + geom_line(aes(y=y, x=X1), colour = "black") +
    geom_ribbon(aes(ymin=l, ymax=u, x=X1), alpha = 0.3, fill = "red") +
    labs(x="p", y="Fracción percolante") +
    theme_original()
  print(plot_datos)
}

#ej1d
a<-cbind(c(1,2,3,7,11), log(c(3,2,2,1,1)))
plot(a)
abline(a=2.651163, b=-0.177326)
abline(a = )