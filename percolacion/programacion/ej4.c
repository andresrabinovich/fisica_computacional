//###########################################################################
//Programa que genera una red cuadrada populada con probabilidad p
//y verifica la existencia de cluster percolante para obtener
//la probabilidad critica pc de aparición del mismo.
//Compilar con gcc ej1b.c -o ej1b -lm
//Correr con ej1b Lado_de_la_red cantidad_de_repeticiones_de_la_red
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
s_puntos contar_clusters(int alto, int ancho, int ultimo_cluster, int clusters[alto][ancho]);
void media(s_puntos puntos, float mu[2]);
float sumar_vector(float s[], int tamano);
float *ajuste_lineal(s_puntos puntos);
float chi_cuadrado(s_puntos puntos, float b[2]);
float r_cuadrado(s_puntos puntos, float b[2]);
int masa_cluster(int alto, int ancho, int clusters[alto][ancho], int cluster);
void actualizar_etiquetas(int cantidad_etiquetas, int etiquetas[cantidad_etiquetas][2]);
void unir(int x, int y, int *etiquetas);
int encontrar(int x, int *etiquetas);

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
	
	//Generamos las repeticiones semillas aleatorias
	int repeticiones = 10000;
	int semillas[repeticiones];
	int repeticion;
	for(repeticion = 0; repeticion < repeticiones; repeticion++){
		semillas[repeticion] = rand();	
	}

	//Declaramos las variables que vamos a usar. Vamos a hacer #repeticiones iteraciones de cada uno de los p
	int ultimo_cluster, ultima_etiqueta;
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
	
	//Configuraciones para la probabilidad de ocupación inicial y la variacion en p
	float p_inicial = 0;
	float p_final   = 1;

	//Declaramos otras variables del programa
	float p         = p_inicial;
	float delta_p   = (p_final-p_inicial)/(float)(particiones-1);
	
	//int masas_cluster_percolante[particiones][repeticiones];
	char masas[163840], masas_auxiliar[1024];
    
        //Medimos el tiempo que tarda el script en correr
	clock_t begin = clock();
	int izquierda, arriba;
	//Iteramos hasta p = 1
    	char str[16384];
	sprintf(str, "corridas/ej4/masas.txt");
	FILE *archivo;
	archivo = fopen(str,"w");
	
        //Empezamos a percolar
        printf("Percolando red cuadrada de %dx%d\n", alto, ancho);
	int lattice[alto][ancho];
        int clusters[alto][ancho];
        int masa_total = ancho*alto;
	int etiquetas[masa_total/2]; //Si tenemos un nodo ocupado y uno vacío tipo tablero de ajedrez, cada uno es un cluster y son la max cantidad de clusters posibles.
		
	for(particion = 0; particion < particiones; particion++){

		//Comenzamos a realizar la red
		for(repeticion = 0; repeticion < repeticiones; repeticion++){
		  

		    int masas_de_clusters[masa_total/2]; 
	
		    sprintf(masas, "%s", "");            
			//Seteamos la semilla aleatoria correspondiente a ésta realización
			srand(semillas[repeticion]);
			ultimo_cluster = 0;
	      ultima_etiqueta = 0;
	      for(i = 0; i < masa_total/2;i++) etiquetas[i] = i;
    
			//Populamos la red por columna y en simultaneo usamos el algoritmo de Hoshen-Kopelman para detectar clusters
			for(x = 0; x < ancho; x++){
				for(y = 0; y < alto; y++){
					r = (float)rand() / (float)RAND_MAX;
					//Ponemos un 1 en la red con probabilidad p
					if(r <= p){
						lattice[y][x]  = 1; //Si queremos imprimir la red original
                        clusters[y][x] = 1;
						//Algoritmo de Hoshen-Kopelman para detectar clusters. Se fija si el nodo de arriba suyo está poblado y pertenece a algún cluster. Si pertenece a algún cluster
						//se pone a si mismo en ese cluster. Después se fija si a su lado izquierdo hay un nodo poblado perteneciente a un cluster distinto. Si pertenece a algún cluster distinto                        
                        if(x > 0 && y > 0){
                            izquierda = clusters[y][x-1];
                            arriba = clusters[y-1][x];
                            if (izquierda == 0 && arriba == 0){
                                ultimo_cluster = ultimo_cluster + 1;
                                clusters[y][x] = ultimo_cluster;
                                masas_de_clusters[clusters[y][x]] = 0;
                            }else if (izquierda != 0 && arriba == 0){
                                clusters[y][x] = encontrar(izquierda, etiquetas);
                            }else if (izquierda == 0 && arriba != 0){ 
                                clusters[y][x] = encontrar(arriba, etiquetas);
                            }else{ 
                                unir(arriba, izquierda, etiquetas); 
                                clusters[y][x] = encontrar(izquierda, etiquetas);
                            }
                        }else if(y > 0){
                            arriba = clusters[y-1][x];
                            if (arriba == 0){
                                ultimo_cluster = ultimo_cluster + 1;
                                clusters[y][x] = ultimo_cluster;
                                masas_de_clusters[clusters[y][x]] = 0;
                            }else{
                                clusters[y][x] = encontrar(arriba, etiquetas);
                            }                            
                        }else if(x > 0){
                            izquierda = clusters[y][x-1];
                            if (izquierda == 0){ 
                                ultimo_cluster = ultimo_cluster + 1;
                                clusters[y][x] = ultimo_cluster;
                                masas_de_clusters[clusters[y][x]] = 0;
                            }else{
                                clusters[y][x] = encontrar(izquierda, etiquetas);
                            } 
                        }else{
                            ultimo_cluster = ultimo_cluster + 1;
                            clusters[y][x] = ultimo_cluster;
                            masas_de_clusters[clusters[y][x]] = 0;
                        }
                        //masas_de_clusters[clusters[y][x]]++;
						/*
                        if (y > 0 && clusters[y-1][x] != 0){
							clusters[y][x] = clusters[y-1][x];
                            
                            //masas_de_clusters[clusters[y][x]-1]++;
							if (x > 0 && clusters[y][x-1] != 0 && clusters[y][x-1] != clusters[y-1][x]){
                                //masas_de_clusters[clusters[y-1][x]-1] += masas_de_clusters[clusters[y][x-1]-1];
                                //masas_de_clusters[clusters[y][x-1]-1] = 0;
								//actualizar_clusters(alto, ancho, clusters, clusters[y][x-1], clusters[y-1][x], x); //clusters[y][x-1] es el actual y clusters[y-1][x] es el cambio
                                //printf("%d %d\n", clusters[y-1][x], clusters[y][x-1]);
                                etiquetas[ultima_etiqueta][0] = clusters[y][x-1];
                                etiquetas[ultima_etiqueta][1] = clusters[y-1][x];
                                //Para intentar minimizar las etiquetas repetidas chequea la etiqueta anterior para que no sea la misma
                                if(ultima_etiqueta == 0 || etiquetas[ultima_etiqueta-1][0] != etiquetas[ultima_etiqueta][0] || etiquetas[ultima_etiqueta-1][1] != etiquetas[ultima_etiqueta][1]) ultima_etiqueta++;
                                
							}
						}else if (x > 0 && clusters[y][x-1] != 0){
							clusters[y][x] = clusters[y][x-1];
                            //masas_de_clusters[clusters[y][x]-1]++;
						}else{
							ultimo_cluster    += 1;
                            //masas_de_clusters[ultimo_cluster-1] = 1;
							clusters[y][x]  = ultimo_cluster;
						}
                        */
					}else{
						lattice[y][x]  = 0;
						clusters[y][x] = 0;
					}
					//printf("%d ", clusters[y][x]);
				}
				//printf("\n");
			}


            /*
            for (int i=0; i<m; i++)
                for (int j=0; j<n; j++)
                if (matrix[i][j]) {
                int x = uf_find(matrix[i][j]);
                if (new_labels[x] == 0) {
                new_labels[0]++;
                new_labels[x] = new_labels[0];
                }
                matrix[i][j] = new_labels[x];
                }
            
            int total_clusters = new_labels[0];
            */
		
			int *etiquetas_nuevas = calloc(sizeof(int), masa_total/2); // allocate array, initialized to zero
            for (y = 0; y < alto; y++){
                for (x = 0; x < ancho; x++){
                    if (clusters[y][x]){
                        int cluster_nuevo = encontrar(clusters[y][x], etiquetas);  
                        if (etiquetas_nuevas[cluster_nuevo] == 0) {
                            etiquetas_nuevas[0]++;
                            etiquetas_nuevas[cluster_nuevo] = etiquetas_nuevas[0];
                        }
                        clusters[y][x] = etiquetas_nuevas[cluster_nuevo];
                        masas_de_clusters[clusters[y][x]]++;
                    }
                }
            }
            
            int total_de_clusters = etiquetas_nuevas[0];
            free(etiquetas_nuevas);	
	    
	    //guardamos en un archivo cada uno de los clusters
            for(i = 1; i <= total_de_clusters; i++){
                if(masas_de_clusters[i] > 1){
                    sprintf(masas_auxiliar, "%f\t%d\n", p, masas_de_clusters[i]);
                    strcat(masas, masas_auxiliar);
                }
            }
            fprintf(archivo, "%s", masas);
            /*
            if(cluster_percolante = verificar_percolacion(alto, ancho, clusters)){
                masa_cluster_percolante[particion][repeticion] = masas_de_clusters[cluster_percolante];
            }else{
                masa_cluster_percolante[particion][repeticion] = 0;
            }
            */
 
            /*
            int n_labels = masa_total/2;
            int j, m=alto, n=ancho;
            int *new_labels = calloc(sizeof(int), n_labels); // allocate array, initialized to zero
  
            for (int i=0; i<m; i++)
                for (int j=0; j<n; j++)
                if (clusters[i][j]) {
                int x = encontrar(clusters[i][j], etiquetas);
                if (new_labels[x] == 0) {
                new_labels[0]++;
                new_labels[x] = new_labels[0];
                }
                clusters[i][j] = new_labels[x];
                }
            
            int total_clusters = new_labels[0];

            free(new_labels);
                */
            //imprimir_lattice(alto, ancho, lattice);
            //imprimir_lattice(alto, ancho, clusters);
            //printf("Cantidad de clusters %d\n", etiquetas_nuevas[0]);
            /*
            for(i = 1; i < ultimo_cluster; i++){
                printf("%d %d\n", encontrar(etiquetas[i], etiquetas), masas_de_clusters[encontrar(etiquetas[i], etiquetas)]);
            }
            
            actualizar_etiquetas(ultima_etiqueta, etiquetas);
            for(i = 0; i < ultima_etiqueta; i++){
                printf("%d %d\n", etiquetas[i][0], etiquetas[i][1]);
            } 
            */
            //printf("Intensidad Pc: %f\n", intensidad_cluster_percolante[particion][repeticion]);
             
		}
		p = p + delta_p;
		printf("Percolando con p=%f\n", p);		
	}
	
	//Guardamos todas las fracciones de todas las particiones en un archivo para su posterior análisis	

    /*
    for (particion = 0; particion < particiones; particion++) {
		sprintf(str, "%d", l_inicial + particion);
		for(repeticion = 0; repeticion < repeticiones; repeticion++){
			//strcat(str, "\t");
			//sprintf(str2, "%f", chi2[particion][repeticion]);
			//strcat(str, str2);
			strcat(str, "\t");
            sprintf(str2, "%d", masa_cluster_percolante[particion][repeticion]);
			strcat(str, str2);
		}
		strcat(str, "\n");
   		fprintf(archivo, "%s", str);
		//p = p + delta_p;
    }*/
	
	fclose(archivo);
    
	//Mostramos el tiempo que tardó la corrida
	clock_t end = clock();
	double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("Transcurrió: %f segundos\n", time_spent);
	
	return(0);
}

//#######################
//DEFINICIÓN DE FUNCIONES
//#######################

void unir(int x, int y, int *etiquetas){
    etiquetas[encontrar(x, etiquetas)] = encontrar(y, etiquetas);
}

int encontrar(int x, int *etiquetas){
    int z, y = x;
    while (etiquetas[y] != y){
        y = etiquetas[y];
    }
    while (etiquetas[x] != x){
        z = etiquetas[x];
        etiquetas[x] = y;
        x = z;
    }
   return(y);
}

//Calcula el r cuadrado (coeficiente de determinacion) para el ajuste lineal
float r_cuadrado(s_puntos puntos, float b[2]){
	float c_p[puntos.cantidad_puntos];
	float c_y[puntos.cantidad_puntos];
	float c_yy[puntos.cantidad_puntos];
	float s_y[puntos.cantidad_puntos];
	float s_yy[puntos.cantidad_puntos];
	int i;
	float mu[2];
	media(puntos, mu);
	for(i = 0; i < puntos.cantidad_puntos; i++){
		
        //Estos tres datos sirven para calcular el error cuadratico dado por el ajuste
        c_p[i] = b[1]*puntos.puntos[i].x + b[0]; //Estimado por el modelo
		c_y[i] = puntos.puntos[i].y -c_p[i]; //La diferencia entre el observado y el estimado
		c_yy[i] = c_y[i]*c_y[i];
        
        //Estos dos datos sirven para calcular el error cuadratico total
		s_y[i] = puntos.puntos[i].y - mu[1]; //La diferencia entre el observado y la media
		s_yy[i] = s_y[i]*s_y[i]; 
	}
	
	//Calcula el error cuadratico dado por el ajuste
	float sse = sumar_vector(c_yy, puntos.cantidad_puntos);
	
    //Calcula la suma cuadratica total
    float sst = sumar_vector(s_yy, puntos.cantidad_puntos);
	
    //Calcula cuanto de mis datos es explicado por el ajuste
	float ssr = sst-sse;
	if(sst == 0) return(10000000);
    
    //Devuelve la relación entre lo explicado por mi ajuste y el error total (r2)
	return(ssr/sst);
}

//Calcula el chi cuadrado para el ajuste lineal
float chi_cuadrado(s_puntos puntos, float b[2]){
	float c_p[puntos.cantidad_puntos];
	float c_y[puntos.cantidad_puntos];
	float c_yy[puntos.cantidad_puntos];
	float chi[puntos.cantidad_puntos];
	int i;
	for(i = 0; i < puntos.cantidad_puntos; i++){
		c_p[i] = b[1]*puntos.puntos[i].x + b[0]; //Estimado por el modelo
		c_y[i] = puntos.puntos[i].y -c_p[i]; //La diferencia entre el observado y el estimado
		c_yy[i] = c_y[i]*c_y[i];
		if(c_p[i] == 0) return(10000000);
		chi[i] = c_yy[i]/c_p[i]; //Cada uno de los elementos del chi2
	}
	return(sumar_vector(chi, puntos.cantidad_puntos));
}

//Función que realiza un ajuste lineal a los puntos y devuelve los coeficientes del ajuste.
float *ajuste_lineal(s_puntos puntos){
	float mu[2];
	float *b = malloc(sizeof(float)*2);	
	int i;
	media(puntos, mu);
	float s_x[puntos.cantidad_puntos], s_y[puntos.cantidad_puntos], s_xx[puntos.cantidad_puntos], s_xy[puntos.cantidad_puntos];
	for(i = 0; i < puntos.cantidad_puntos; i++){
		s_x[i] = puntos.puntos[i].x - mu[0];
		s_y[i] = puntos.puntos[i].y - mu[1];
		s_xx[i] = s_x[i]*s_x[i];
		s_xy[i] = s_x[i]*s_y[i];
	}
	b[1] = sumar_vector(s_xy, puntos.cantidad_puntos)/sumar_vector(s_xx, puntos.cantidad_puntos);
	b[0] = mu[1]-b[1]*mu[0];
	return(b);
}

//Función que devuelve la suma de todos los elementos de un vector
float sumar_vector(float s[], int tamano){
	int i;
	float suma = 0;
	for(i = 0; i < tamano; i++){
		suma = suma + s[i];
	}
	return(suma);
}

//Función que cacula el valor medio de cada cordeenada de varios puntos por separado
void media(s_puntos puntos, float mu[2]){
	mu[0] = 0;
	mu[1] = 0;
	int i;
	for(i = 0; i < puntos.cantidad_puntos; i++){
		mu[0] = mu[0] + puntos.puntos[i].x;
		mu[1] = mu[1] + puntos.puntos[i].y;
	}
	mu[0] = mu[0]/puntos.cantidad_puntos;
	mu[1] = mu[1]/puntos.cantidad_puntos;
	return;
}

//Función que cuenta la cantidad de clusters de determinado tamaño
s_puntos contar_clusters(int alto, int ancho, int ultimo_cluster, int clusters[alto][ancho]){
	int x, y, i, cantidad_puntos = 0, maximo_tamano = 0;
	int tamano_de_clusters[ultimo_cluster];
	for(i = 0; i <= ultimo_cluster; i++) tamano_de_clusters[i] = 0;
	for(y = 0; y < alto; y++){
		for(x = 0; x < ancho; x++){
			tamano_de_clusters[clusters[y][x]]++;
		}
	}
	for(i = 1; i <= ultimo_cluster; i++) {
		if(tamano_de_clusters[i] > maximo_tamano) maximo_tamano = tamano_de_clusters[i];
		//printf("%d %d\n", i, tamano_de_clusters[i]);
	}
	//printf("\n\n\n");
	int cantidad_por_tamano[maximo_tamano];
	for(i = 0; i <= maximo_tamano; i++) cantidad_por_tamano[i] = 0;
	for(i = 1; i <= ultimo_cluster; i++) {
		cantidad_por_tamano[tamano_de_clusters[i]]++;
	}
	
	//Descarto de las puntas para que ajuste mejor y no tener problemas de red de tamaño finito, desde 5 hasta 50 (discutir estos límites arbitrarios)
	maximo_tamano = (maximo_tamano > 40) ? 40: maximo_tamano;
	for(i = 5; i < maximo_tamano; i++){
		//printf("%d %d\n", i, cantidad_por_tamano[i]);
		if(cantidad_por_tamano[i] > 1) cantidad_puntos++;	
	}
	//printf("%d\n", cantidad_puntos);
	s_puntos puntos;
	puntos.puntos = malloc(sizeof(s_punto) * cantidad_puntos);
	puntos.cantidad_puntos = 0;
	for(i = 5; i < maximo_tamano; i++){
		if(cantidad_por_tamano[i] > 1){
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

//Actualiza los pares de etiquetas
void actualizar_etiquetas(int cantidad_etiquetas, int etiquetas[cantidad_etiquetas][2]){
	int i, j, etiqueta_aux, actualizo;
	for(i = 0; i < cantidad_etiquetas; i++){
		for(j = 0; j < cantidad_etiquetas; j++){
            if(i != j){
                actualizo = 0;
                if(etiquetas[j][0] == etiquetas[i][1]){
                    etiquetas[j][0] = etiquetas[i][0];
                    actualizo = 1;
                }else if(etiquetas[j][1] == etiquetas[i][1]){
                    etiquetas[j][1] = etiquetas[i][0];                
                    actualizo = 1;
                }
                if(actualizo && etiquetas[j][1] < etiquetas[j][0]){
                    etiqueta_aux    = etiquetas[j][0];    
                    etiquetas[j][0] = etiquetas[j][1];
                    etiquetas[j][1] = etiqueta_aux;
                }
            }
		}
	}
	return;
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
				return(posibles_percolantes[x][0]);	
			}
		}
	}
	return(0);
}

//Funcion que encuentra la masa de un determinado cluster
int masa_cluster(int alto, int ancho, int clusters[alto][ancho], int cluster){
    int x, y, masa = 0;
    for(y = 0; y < alto; y++){
        for(x = 0; x < ancho; x++){
            if(clusters[y][x] == cluster) masa++;
        }
    }
    return(masa);
}
