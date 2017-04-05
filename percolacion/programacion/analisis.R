library(ggplot2)
theme_original <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill="#e1e1e1"),
      panel.grid.minor = element_line(size=0.1, color="white"),
      panel.grid.major = element_line(size=0.2, color="white")
    )
}

#ej1a
for(i in c("4x4", "8x8", "16x16", "32x32", "64x64", "128x128")){
  pc <- read_csv(paste("~/fisica_computacional/percolacion/programacion/corridas/ej1a/pc", i, ".txt", sep=""), col_names = FALSE)
  plot_pc <- ggplot(data=pc, aes(pc$X1)) + 
  geom_histogram(breaks=seq(min(pc$X1), max(pc$X1), by = (max(pc$X1)-min(pc$X1))/20), col="black", fill="white") + 
  labs(title=paste("Histograma de Pc para 1000 realizaciones de una red cuadrada de ", i, sep="")) +
  labs(x="Pc", y="Frecuencia") +
  theme_original()
  print(plot_pc)
  print(mean(pc$X1))
  print(sd(pc$X1))
}
