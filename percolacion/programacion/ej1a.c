//###########################################################################
//Programa que genera una red cuadrada populada con probabilidad p
//y verifica la existencia de cluster percolante para obtener
//la probabilidad critica pc de aparición del mismo.
//Compilar con gcc ej1a.c -o ej1a -lm
//Correr con ej1a Lado_de_la_red cantidad_de_realizaciones_de_la_red
//###########################################################################

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//############################
//DECLARACION DE LAS FUNCIONES
//############################
void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]);
int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]);
void a_lattice(int alto, int ancho, int lattice[alto][ancho]);
void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador);
void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int x_max);
int verificar_percolacion(int alto, int ancho, int clusters[alto][ancho]);

//#####################
//COMIENZO DEL PROGRAMA
//#####################
int main(int argc, char **argv){
	//Seteamos la semilla aleatoria para generar al azar argv[2] semillas aleatorias, una para cada realización de la red y obtener valores repetibles
	srand(12345);
	
	//Traemos la cantidad de realizaciones (argv[2]) o 1000 por defecto
	int realizaciones;
	if(argc == 3){
		realizaciones = atoi(argv[2]);
	}else{
		realizaciones = 1000;
	}
	
	//Generamos las realizaciones semillas aleatorias
	int semillas[realizaciones];
	int realizacion;
	for(realizacion = 0; realizacion < realizaciones; realizacion++){
		semillas[realizacion] = rand();	
	}

	//Declaramos las variables que vamos a usar
	float pc[realizaciones];
	int max_cluster;
	int iteracion;
	int x, y;
	float r;
	int alto, ancho;
	
	//Traemos el lado de la red o 16 por defecto
	if(argc > 1){
		alto  = atoi(argv[1]);
	}else{
		alto  = 16;
	}
	ancho = alto;
	int lattice[alto][ancho];
	int clusters[alto][ancho];

	//Configuraciones para la probabilidad de ocupación inicial y la precisión buscada (la cantidad de iteraciones sobre una misma red)
	float p = 0.5;
	int precision = 20;
	printf("Percolando red cuadrada de %dx%d\n", alto, ancho);

	//Medimos el tiempo que tarda el script en correr
	clock_t begin = clock();

	//Comenzamos a realizar la red
	for(realizacion = 0; realizacion < realizaciones; realizacion++){

		//Iteramos p+-1/2^(iteracion+1) hasta la precisión deseada
		iteracion = 1;
		while(iteracion <= precision){

			//Seteamos la semilla aleatoria correspondiente a ésta realización
			srand(semillas[realizacion]);

			//Iniciamos la red (lattice) con todo en 0
			inicializar_lattice(alto, ancho, lattice, 0);

			//Iniciamos la red que contiene a que cluster pertenece cada nodo y el valor del próximo cluster no usado (max_cluster) lo ponemos en 0
			inicializar_lattice(alto, ancho, clusters, 0);
			max_cluster = 0;

			//Populamos la red por columna y en simultaneo usamos el algoritmo de Hoshen-Kopelman para detectar clusters
			for(x = 0; x < ancho; x++){
				for(y = 0; y < alto; y++){
					r = (float)rand() / (float)RAND_MAX;
					//Ponemos un 1 en la red con probabilidad p
					if(r <= p){
						lattice[y][x] = 1;
			
						//Algoritmo de Hoshen-Kopelman para detectar clusters. Se fija si el nodo de arriba suyo está poblado y pertenece a algún cluster. Si pertenece a algún cluster
						//se pone a si mismo en ese cluster. Después se fija si a su lado izquierdo hay un nodo poblado perteneciente a un cluster distinto. Si pertenece a algún cluster distinto
						//actualiza todos los elementos de ese cluster (función actualizar_clusters) distinto con el número de cluster correcto 
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
			//Imrpime la red y los respectivos clusters para verificación
			//imprimir_lattice(alto, ancho, lattice);
			//imprimir_lattice(alto, ancho, clusters);

			//Se fija si hubo o no percolación y modifica p en función del resultado
			if(verificar_percolacion(alto, ancho, clusters) == 1){
				p = p - (1/pow(2, iteracion+1));		
			}else{
				p = p + (1/pow(2, iteracion+1));
			}
			iteracion++;
		}
		//printf("Pcritica: %f\n", p);
		//Guardamos el valor de p crítico de ésta realización 
		pc[realizacion]	= p;
	}

	//Guardamos todas las pcriticas de todas las realizaciones en un archivo para su posterior análisis	
	char str[80];
	sprintf(str, "corridas/ej1a/%dx%d.txt", alto, ancho);
    FILE *archivo;
    archivo = fopen(str,"w");
	float promedio = 0;
    for (realizacion = 0; realizacion < realizaciones; realizacion++) {
    	fprintf(archivo, "%f\n", pc[realizacion]);
		promedio = promedio + pc[realizacion];
    }
	fclose(archivo);
	
	//Mostramos el promedio de las pcriticas
	printf("Pcritica: %f\n", promedio/(float)realizaciones);
	
	//Mostramos el tiempo que tardó la corrida
	clock_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("Transcurrió: %f segundos\n", time_spent);

	return(0);
}

//#######################
//DEFINICIÓN DE FUNCIONES
//#######################

//Función que imprime una red
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

//Función que inicializa todos los elementos de una red con valor inicializador
void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador){
	int x, y;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			lattice[y][x] = inicializador;
		}
	}
}

//Función que suma todos los elementos de la red para validación (en una red de 10x10 con p = 0.1, la suma debería estar cerca de 10)
int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]){
	int x, y, suma = 0;
	for(x = 0; x < ancho; x++){
		for(y = 0; y < alto; y++){
			suma += lattice[x][y];				
		}
	}
	return(suma);
}

//Actualiza la pertenencia de los elementos de un cluster
void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int x_max){
	int x, y;
	for(x = 0; x <= x_max; x++){
		for(y = 0; y < alto; y++){
			if(clusters[y][x] == actual) clusters[y][x] = cambio;
		}
	}
}

//Verifica la existencia de cluster percolante
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
