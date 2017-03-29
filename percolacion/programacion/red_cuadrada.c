#include <time.h>
#include <stdio.h>
#include <stdlib.h>

void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]);
int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]);
void a_lattice(int alto, int ancho, int lattice[alto][ancho]);
void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador);
void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int y_max);
void verificar_percolacion(int alto, int ancho, int lattice[alto][ancho]);
int main(){
	int i;
	int x, y;
	float r;
	float p   = 0.25;
	int alto  = 10;
	int ancho = 10;
	srand(123);
	int max_cluster = 0;
	int lattice[alto][ancho];
	int clusters[alto][ancho];
	inicializar_lattice(alto, ancho, lattice, 0);
	inicializar_lattice(alto, ancho, clusters, 0);
	for(y = 0; y < alto; y++){
		for(x = 0; x < ancho; x++){
			r = (float)rand() / (float)RAND_MAX;
			if(r <= p){
				lattice[x][y] = 1;				
				if (y > 0 && clusters[x][y-1] != 0){
					clusters[x][y] = clusters[x][y-1];
					if (clusters[x-1][y] != 0 && clusters[x-1][y] != clusters[x][y-1]){
						actualizar_clusters(alto, ancho, clusters, clusters[x-1][y], clusters[x][y-1], y);
					}
				}else if (x > 0 && clusters[x-1][y] != 0){
					clusters[x][y] = clusters[x-1][y];
				}else{
					max_cluster    += 1;
					clusters[x][y]  = max_cluster;
				}
			}
		}
	}
	imprimir_lattice(alto, ancho, lattice);
	imprimir_lattice(alto, ancho, clusters);
	return;
}

void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]){
	int x, y;
	printf("\n\n");
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			printf("%2d|", lattice[x][y]);				
		}
		printf("\n");
	}
	printf("\n\n");
}

void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador){
	int x, y;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			lattice[x][y] = inicializador;
		}
	}
}

int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]){
	int x, y, suma = 0;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			suma += lattice[x][y];				
		}
	}
	return(suma);
}

void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int y_max){
	int x, y;
	for(y = 0; y <= y_max; y++){
		for(x = 0; x < ancho; x++){
			if(clusters[x][y] == actual) clusters[x][y] = cambio;
		}
	}
}

void verificar_percolacion(int alto, int ancho, int lattice[alto][ancho]){
	int x, y;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			
		}
	}
}
