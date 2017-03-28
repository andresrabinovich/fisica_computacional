import numpy
numpy.random.seed(123)
p             = 0.5
alto          = 10
ancho         = 10
red_aleatoria = numpy.random.random_sample((ancho,alto))
lattice       = numpy.zeros((ancho,alto)) 
clusters      = numpy.zeros((ancho,alto)) 
max_cluster   = 0
for y in range(alto):
	for x in range(ancho):
		if red_aleatoria[x][y] <= p:
			lattice[x][y]  = 1
			if y > 0 and clusters[x][y-1] != 0:
				clusters[x][y] = clusters[x][y-1]
				if clusters[x-1][y] != 0 and clusters[x-1][y] != clusters[x][y-1]:
					#clusters[numpy.transpose(numpy.nonzero(clusters == clusters[x][y-1]))] = clusters[x-1][y]
					1	
			elif x > 0 and clusters[x-1][y] != 0:
				clusters[x][y] = clusters[x-1][y]
			else:
				max_cluster    += 1
				clusters[x][y]  = max_cluster
		else:
			lattice[x][y] = 0
print(lattice)
print(clusters)
print(numpy.sum(lattice))

