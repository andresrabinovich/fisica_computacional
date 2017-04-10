//###########################################################################
//Programa que genera una red cuadrada populada con probabilidad p
//y verifica la existencia de cluster percolante para obtener
//la probabilidad critica pc de aparición del mismo.
//Compilar con gcc ej1b.c -o ej1b -lm
//Correr con ej1b Lado_de_la_red cantidad_de_realizaciones_de_la_red
//###########################################################################

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

//##########################
//DECLARACION DE ESTRUCTURAS
//##########################
typedef struct{
	float x;
	float y;
}s_punto;

typedef struct{
	s_punto* puntos;
	int cantidad_puntos;
}s_puntos;

//############################
//DECLARACION DE LAS FUNCIONES
//############################
void imprimir_lattice(int alto, int ancho, int lattice[alto][ancho]);
int sumar_lattice(int alto, int ancho, int lattice[alto][ancho]);
void a_lattice(int alto, int ancho, int lattice[alto][ancho]);
void inicializar_lattice(int alto, int ancho, int lattice[alto][ancho], int inicializador);
void actualizar_clusters(int alto, int ancho, int clusters[alto][ancho], int actual, int cambio, int x_max);
int verificar_percolacion(int alto, int ancho, int clusters[alto][ancho]);
s_puntos contar_clusters(int alto, int ancho, int max_cluster, int clusters[alto][ancho]);

//#####################
//COMIENZO DEL PROGRAMA
//#####################
int main(int argc, char **argv){
	//Seteamos la semilla aleatoria para generar al azar el resto de las semillas aleatorias, una para cada particiones de la red y obtener valores repetibles
	srand(12345);
	
	//Traemos la cantidad de particiones (argv[2]) o 1000 por defecto
	int particion, particiones;
	if(argc == 3){
		particiones = atoi(argv[2]);
	}else{
		particiones = 1000;
	}
	
	//Generamos las realizaciones semillas aleatorias
	int realizaciones = 1000;
	int semillas[realizaciones];
	int realizacion;
	for(realizacion = 0; realizacion < realizaciones; realizacion++){
		semillas[realizacion] = rand();	
	}

	//Declaramos las variables que vamos a usar. Vamos a hacer 1000 iteraciones de cada uno de los p
	int fraccion_percolante[particiones];
	int max_cluster;
	int x, y, i;
	float r;
	int alto, ancho;
	
	//Traemos el lado de la red o 16 por defecto
	if(argc > 1){
		alto  = atoi(argv[1]);
	}else{
		alto  = 4;
	}
	ancho = alto;
	int lattice[alto][ancho];
	int clusters[alto][ancho];

	//Configuraciones para la probabilidad de ocupación inicial y la variacion en p
	float p_inicial = 0.56;
	float p_final   = 0.6; 	
	float p         = p_inicial;	
	float delta_p   = (p_final-p_inicial)/(float)(particiones-1);

	printf("Percolando red cuadrada de %dx%d\n", alto, ancho);

	//Medimos el tiempo que tarda el script en correr
	clock_t begin = clock();

	//Iteramos hasta p = 1
	//for(particion = 0; particion < particiones; particion++){

		//Comenzamos a realizar la red
		//for(realizacion = 0; realizacion < realizaciones; realizacion++){
		for(realizacion = 0; realizacion < 1; realizacion++){

			//Seteamos la semilla aleatoria correspondiente a ésta realización
			srand(semillas[realizacion]);

			//Iniciamos la red (lattice) con todo en 0
			//inicializar_lattice(alto, ancho, lattice, 0);

			//Iniciamos la red que contiene a que cluster pertenece cada nodo y el valor del próximo cluster no usado (max_cluster) lo ponemos en 0
			//inicializar_lattice(alto, ancho, clusters, 0);
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
			
					}else{
						lattice[y][x]  = 0;
						clusters[y][x] = 0;
					}
				}
			}
			//Imprime la red y los respectivos clusters para verificación
			//imprimir_lattice(alto, ancho, lattice);
			//imprimir_lattice(alto, ancho, clusters);
			//Cuenta la cantidad de clusters que hay de cada tamaño
			s_puntos puntos = contar_clusters(alto, ancho, max_cluster, clusters);
			int punto;			
			//printf("%d\n",  puntos.cantidad_puntos);
			for(punto = 0; punto < puntos.cantidad_puntos; punto++){
				printf("%f %f %f\n", puntos.puntos[punto].x, puntos.puntos[punto].y, log(puntos.puntos[punto].y));
			}
			free(puntos.puntos);
		}
		//printf("%d\n", particion);
		//printf("Pcritica: %f\n", p);
		p = p + delta_p;
	//}
	/*
	//Guardamos todas las fracciones de todas las particiones en un archivo para su posterior análisis	
	char str[1024], str2[1024];
	sprintf(str, "corridas/ej1b/%dx%d.txt", alto, ancho);
    FILE *archivo;
    archivo = fopen(str,"w");
	p = 0;
    for (particion = 0; particion < particiones; particion++) {
		sprintf(str, "%f", p);
		for(repeticion = 0; repeticion < repeticiones; repeticion++){
			strcat(str, "\t");
			sprintf(str2, "%f", (float)fraccion_percolante[particion][repeticion]/(float)realizaciones);
			strcat(str, str2);
		}
		strcat(str, "\n");
   		fprintf(archivo, "%s", str);
		p = p + delta_p;
    }
	fclose(archivo);
	*/
	//Mostramos el tiempo que tardó la corrida
	clock_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("Transcurrió: %f segundos\n", time_spent);
	
	return(0);
}

//#######################
//DEFINICIÓN DE FUNCIONES
//#######################

//Función que cuenta la cantidad de clusters de determinado tamaño
s_puntos contar_clusters(int alto, int ancho, int max_cluster, int clusters[alto][ancho]){
	int x, y, i, cantidad_puntos = 0, maximo_tamano = 0;
	int tamano_de_clusters[max_cluster];
	for(i = 0; i <= max_cluster; i++) tamano_de_clusters[i] = 0;
	for(y = 0; y < alto; y++){
		for(x = 0; x < ancho; x++){
			tamano_de_clusters[clusters[y][x]]++;
		}
	}
	for(i = 1; i <= max_cluster; i++) {
		if(tamano_de_clusters[i] > maximo_tamano) maximo_tamano = tamano_de_clusters[i];
		//printf("%d %d\n", i, tamano_de_clusters[i]);
	}
	//printf("\n\n\n");
	int cantidad_por_tamano[maximo_tamano];
	for(i = 0; i <= maximo_tamano; i++) cantidad_por_tamano[i] = 0;
	for(i = 1; i <= max_cluster; i++) {
		cantidad_por_tamano[tamano_de_clusters[i]]++;
	}
	for(i = 1; i <= maximo_tamano; i++){
		//printf("%d %d\n", i, cantidad_por_tamano[i]);
		if(cantidad_por_tamano[i] > 0) cantidad_puntos++;	
	}
	//printf("%d\n", cantidad_puntos);
	s_puntos puntos;
	puntos.puntos = malloc(sizeof(s_punto) * cantidad_puntos);
	puntos.cantidad_puntos = 0;
	for(i = 1; i <= maximo_tamano; i++){
		if(cantidad_por_tamano[i] > 0){
			puntos.puntos[puntos.cantidad_puntos].x = (float)i;
			puntos.puntos[puntos.cantidad_puntos].y = (float)cantidad_por_tamano[i];
			puntos.cantidad_puntos++;
		}
	}
	return(puntos);
}

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