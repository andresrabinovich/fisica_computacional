import numpy
numpy.random.seed(123)
p             = 0.5
alto          = 10
ancho         = 10
red_aleatoria = numpy.random.random_sample((ancho,alto))
lattice       = numpy.zeros((ancho,alto)) 
for x in range(ancho):
	for y in range(alto):
		if red_aleatoria[x][y] <= p:
			lattice[x][y] = 1
		else:
			lattice[x][y] = 0
print(lattice)
print(numpy.sum(lattice))

