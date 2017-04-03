#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]);
int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]);
void a_lattice(int alto, int ancho, int lattice[alto][ancho]);
void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador);
void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int x_max);
int verificar_percolacion(int alto, int ancho, int clusters[alto][ancho]);
int main(){
	srand(12345);
	int realizaciones = 1000;
	int semillas[realizaciones];
	float pc[realizaciones];
	int realizacion;
	for(realizacion = 0; realizacion < realizaciones; realizacion++){
		semillas[realizacion] = rand();	
	}
	int iteracion;
	int x, y;
	float r;
	float p   = 0.5;
	int alto  = 64;
	int ancho = 64;
	int max_cluster;
	int lattice[alto][ancho];
	int clusters[alto][ancho];
	int precision = 20;
	clock_t begin = clock();
	for(realizacion = 0; realizacion < realizaciones; realizacion++){
		iteracion = 1;
		while(iteracion <= precision){
			srand(semillas[realizacion]);
			max_cluster = 0;
			inicializar_lattice(alto, ancho, lattice, 0);
			inicializar_lattice(alto, ancho, clusters, 0);
			for(x = 0; x < ancho; x++){
				for(y = 0; y < alto; y++){
					r = (float)rand() / (float)RAND_MAX;
					if(r <= p){
						lattice[y][x] = 1;	
						if (y > 0 && clusters[y-1][x] != 0){
							clusters[y][x] = clusters[y-1][x];
							if (x > 0 && clusters[y][x-1] != 0 && clusters[y][x-1] != clusters[y-1][x]){
								actualizar_clusters(alto, ancho, clusters, clusters[y][x-1], clusters[y-1][x], x);
							}
						}else if (x > 0 && clusters[y][x-1] != 0){
							clusters[y][x] = clusters[y][x-1];
						}else{
							max_cluster    += 1;
							clusters[y][x]  = max_cluster;
						}
				
					}
				}
			}
			//imprimir_lattice(alto, ancho, lattice);
			//imprimir_lattice(alto, ancho, clusters);
			if(verificar_percolacion(alto, ancho, clusters) == 1){
				p = p - (1/pow(2, iteracion+1));		
			}else{
				p = p + (1/pow(2, iteracion+1));		
			}
			iteracion++;
		}
		//printf("Pcritica: %f\n", p);
		pc[realizacion]	= p;
	}
	char str[80];
	sprintf(str, "pc%dx%d.txt", alto, ancho);
    FILE *archivo;
    archivo = fopen(str,"w");
	float promedio = 0;
    for (realizacion = 0; realizacion < realizaciones; realizacion++) {
    	fprintf(archivo, "%f\n", pc[realizacion]);
		promedio = promedio + pc[realizacion];
    }
	fclose(archivo);
	printf("Pcritica: %f\n", promedio/(float)realizaciones);
	clock_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("Transcurrió: %f segundos\n", time_spent);

	return(0);
}

void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]){
	int x, y;
	printf("\n\n");
	for(y = 0; y < alto; y++){
		for(x = 0; x < ancho; x++){
			printf("%2d|", lattice[y][x]);				
		}
		printf("\n");
	}
	printf("\n\n");
}

void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador){
	int x, y;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			lattice[y][x] = inicializador;
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

void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int x_max){
	int x, y;
	for(x = 0; x <= x_max; x++){
		for(y = 0; y < alto; y++){
			if(clusters[y][x] == actual) clusters[y][x] = cambio;
		}
	}
}

int verificar_percolacion(int alto, int ancho, int clusters[alto][ancho]){
	int x, y;
	int indice_inicio = 0;
	int indice_fin    = 0;
	int posibles_percolantes[alto][2];
	int percolantes[alto];
	inicializar_lattice(alto, 2, posibles_percolantes, 0);
	for(y = 0; y < alto; y++){
		if(clusters[y][0] != 0){
			posibles_percolantes[indice_inicio][0] = clusters[y][0];
			indice_inicio++;			
		}
		if(clusters[y][ancho-1] != 0){
			posibles_percolantes[indice_fin][1] = clusters[y][ancho-1];	
			indice_fin++;		
		}
	}
	for(x = 0; x < indice_inicio; x++){
		for(y = 0; y < indice_fin; y++){
			if(posibles_percolantes[x][0] == posibles_percolantes[y][1]){
				return(1);			
			}
		}
	}
	return(0);
}
