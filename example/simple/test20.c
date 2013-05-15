#include <pthread.h>
#include <stdio.h>
#include <errno.h>

static int glob = 0;
static pthread_mutex_t mtx1 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx2 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx3 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx4 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx5 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx6 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx7 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx8 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx9 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx10 = PTHREAD_MUTEX_INITIALIZER; 
static pthread_mutex_t mtx11 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx12 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx13 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx14 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx15 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx16 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx17 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx18 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx19 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx20 = PTHREAD_MUTEX_INITIALIZER; 

static void *                  
threadFunc1(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx1);
		pthread_mutex_lock(&mtx2);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx2);
		pthread_mutex_unlock(&mtx1);
	//}
	return NULL;
}

static void *                  
threadFunc2(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx2);
		pthread_mutex_lock(&mtx1);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx1);
		pthread_mutex_unlock(&mtx2);
	//}
	return NULL;
}

static void *                  
threadFunc3(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx3);
		pthread_mutex_lock(&mtx4);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx4);
		pthread_mutex_unlock(&mtx3);
	//}
	return NULL;
}

static void *                  
threadFunc4(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx4);
		pthread_mutex_lock(&mtx3);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx3);
		pthread_mutex_unlock(&mtx4);
	//}
	return NULL;
}


static void *                  
threadFunc5(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx5);
		pthread_mutex_lock(&mtx6);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx6);
		pthread_mutex_unlock(&mtx5);
	//}
	return NULL;
}

static void *                  
threadFunc6(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx6);
		pthread_mutex_lock(&mtx5);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx5);
		pthread_mutex_unlock(&mtx6);
	//}
	return NULL;
}

static void *                  
threadFunc7(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx7);
		pthread_mutex_lock(&mtx8);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx8);
		pthread_mutex_unlock(&mtx7);
	//}
	return NULL;
}

static void *                  
threadFunc8(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx8);
		pthread_mutex_lock(&mtx7);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx7);
		pthread_mutex_unlock(&mtx8);
	//}
	return NULL;
}

static void *                  
threadFunc9(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx9);
		pthread_mutex_lock(&mtx10);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx10);
		pthread_mutex_unlock(&mtx9);
	//}
	return NULL;
}

static void *                  
threadFunc10(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx10);
		pthread_mutex_lock(&mtx9);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx9);
		pthread_mutex_unlock(&mtx10);
	//}
	return NULL;
}

static void *                  
threadFunc11(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx11);
		pthread_mutex_lock(&mtx12);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx12);
		pthread_mutex_unlock(&mtx11);
	//}
	return NULL;
}

static void *                  
threadFunc12(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx12);
		pthread_mutex_lock(&mtx11);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx11);
		pthread_mutex_unlock(&mtx12);
	//}
	return NULL;
}

static void *                  
threadFunc13(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx13);
		pthread_mutex_lock(&mtx14);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx14);
		pthread_mutex_unlock(&mtx13);
	//}
	return NULL;
}

static void *                  
threadFunc14(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx14);
		pthread_mutex_lock(&mtx13);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx13);
		pthread_mutex_unlock(&mtx14);
	//}
	return NULL;
}


static void *                  
threadFunc15(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx15);
		pthread_mutex_lock(&mtx16);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx16);
		pthread_mutex_unlock(&mtx15);
	//}
	return NULL;
}

static void *                  
threadFunc16(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx16);
		pthread_mutex_lock(&mtx15);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx15);
		pthread_mutex_unlock(&mtx16);
	//}
	return NULL;
}

static void *                  
threadFunc17(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx17);
		pthread_mutex_lock(&mtx18);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx18);
		pthread_mutex_unlock(&mtx17);
	//}
	return NULL;
}

static void *                  
threadFunc18(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx18);
		pthread_mutex_lock(&mtx17);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx17);
		pthread_mutex_unlock(&mtx18);
	//}
	return NULL;
}

static void *                  
threadFunc19(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx19);
		pthread_mutex_lock(&mtx20);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx20);
		pthread_mutex_unlock(&mtx19);
	//}
	return NULL;
}

static void *                  
threadFunc20(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx20);
		pthread_mutex_lock(&mtx19);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx19);
		pthread_mutex_unlock(&mtx20);
	//}
	return NULL;
}

int
main(int argc, char *argv[])
{
    pthread_t t1, t2 , t3,t4 , t5 , t6,t7, t8 , t9,t10 ,t11, t12 , t13,t14 , t15 , t16,t17, t18 , t19,t20;
    int loops, s;

    loops = 100000;

    s = pthread_create(&t1, NULL, threadFunc1, &loops);
   
    s = pthread_create(&t2, NULL, threadFunc2, &loops);

	s = pthread_create(&t3, NULL, threadFunc3, &loops);
	
	s = pthread_create(&t4, NULL, threadFunc4, &loops);
   
    s = pthread_create(&t5, NULL, threadFunc5, &loops);

	s = pthread_create(&t6, NULL, threadFunc6, &loops);
	  
   
    s = pthread_create(&t7, NULL, threadFunc7, &loops);

	s = pthread_create(&t8, NULL, threadFunc8, &loops);
	
	s = pthread_create(&t9, NULL, threadFunc9, &loops); 
    
	s = pthread_create(&t10, NULL, threadFunc10, &loops); 
	
	    s = pthread_create(&t11, NULL, threadFunc11, &loops);
   
    s = pthread_create(&t12, NULL, threadFunc12, &loops);

	s = pthread_create(&t13, NULL, threadFunc13, &loops);
	
	s = pthread_create(&t14, NULL, threadFunc14, &loops);
   
    s = pthread_create(&t15, NULL, threadFunc15, &loops);

	s = pthread_create(&t16, NULL, threadFunc16, &loops);
	  
   
    s = pthread_create(&t17, NULL, threadFunc17, &loops);

	s = pthread_create(&t18, NULL, threadFunc18, &loops);
	
	s = pthread_create(&t19, NULL, threadFunc19, &loops); 
    
	s = pthread_create(&t20, NULL, threadFunc20, &loops); 

    printf("glob = %d\n", glob);
    return 0;
}















