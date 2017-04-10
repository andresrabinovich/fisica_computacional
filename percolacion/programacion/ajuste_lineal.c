#include <stdio.h>
#include <stdlib.h>

float *ajuste_lineal(float puntos[6][2]);
void media(float puntos[6][2], float mu[2]);
float chi_cuadrado();
float sumar_vector(float s[6]);
float r_cuadrado(float puntos[6][2], float b[2]);

int main(){
	
	float puntos[6][2];
	puntos[0][0] = 34;
	puntos[0][1] = 5;
	puntos[1][0] = 108;
	puntos[1][1] = 17;
	puntos[2][0] = 64;
	puntos[2][1] = 11;
	puntos[3][0] = 88;
	puntos[3][1] = 8;
	puntos[4][0] = 99;
	puntos[4][1] = 14;
	puntos[5][0] = 51;
	puntos[5][1] = 5;
	float *b = ajuste_lineal(puntos);
	float chi2 = chi_cuadrado(puntos, b);
	float r2 = r_cuadrado(puntos, b);
	printf("%f %f\n", chi2, r2);
	free(b);
	return(0);
}

float *ajuste_lineal(float puntos[6][2]){
	float mu[2];
	float *b = malloc(sizeof(float)*2);	
	int i;
	media(puntos, mu);
	float s_x[6], s_y[6], s_xx[6], s_xy[6];
	for(i = 0; i < 6; i++){
		s_x[i] = puntos[i][0] - mu[0];
		s_y[i] = puntos[i][1] - mu[1];
		s_xx[i] = s_x[i]*s_x[i];
		s_xy[i] = s_x[i]*s_y[i];
	}
	b[1] = sumar_vector(s_xy)/sumar_vector(s_xx);
	b[0] = mu[1]-b[1]*mu[0];
	return(b);
}

float sumar_vector(float s[6]){
	int i;
	float suma = 0;
	for(i = 0; i < 6; i++){
		suma = suma + s[i];
	}
	return(suma);
}

void media(float puntos[6][2], float mu[2]){
	mu[0] = 0;
	mu[1] = 0;
	int i;
	for(i = 0; i < 6; i++){
		mu[0] = mu[0] + puntos[i][0];
		mu[1] = mu[1] + puntos[i][1];
	}
	mu[0] = mu[0]/6;
	mu[1] = mu[1]/6;
	return;
}

float chi_cuadrado(float puntos[6][2], float b[2]){
	float c_p[6];
	float c_y[6];
	float c_yy[6];
	float chi[6];
	int i;
	for(i = 0; i < 6; i++){
		c_p[i] = b[1]*puntos[i][0] + b[0];
		c_y[i] = puntos[i][1] -c_p[i];
		c_yy[i] = c_y[i]*c_y[i];
		chi[i] = c_yy[i]/c_p[i];
	}
	return(sumar_vector(chi));
}

float r_cuadrado(float puntos[6][2], float b[2]){
	float c_p[6];
	float c_y[6];
	float c_yy[6];
	float s_y[6];
	float s_yy[6];
	int i;
	float mu[2];
	media(puntos, mu);
	for(i = 0; i < 6; i++){
		c_p[i] = b[1]*puntos[i][0] + b[0];
		c_y[i] = puntos[i][1] -c_p[i];
		c_yy[i] = c_y[i]*c_y[i];
		s_y[i] = puntos[i][1] - mu[1];
		s_yy[i] = s_y[i]*s_y[i];
	}
	float sse = sumar_vector(c_yy);
	float sst = sumar_vector(s_yy);
	printf("%f %f\n", sse, sst);
	float ssr = sst-sse;
	return(ssr/sst);
}
