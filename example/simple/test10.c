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
		pthread_mutex_lock(&mtx3);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx3);
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
		pthread_mutex_lock(&mtx5);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx5);
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
		pthread_mutex_lock(&mtx1);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx1);
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
		pthread_mutex_lock(&mtx7);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx7);
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
		pthread_mutex_lock(&mtx9);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx9);
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
		pthread_mutex_lock(&mtx1);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx1);
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
		pthread_mutex_lock(&mtx1);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx1);
		pthread_mutex_unlock(&mtx10);
	//}
	return NULL;
}

int
main(int argc, char *argv[])
{
    pthread_t t1, t2 , t3,t4 , t5 , t6,t7, t8 , t9,t10;
    int loops, s;

    loops = 100000;

    s = pthread_create(&t1, NULL, threadFunc1, &loops);
   
    s = pthread_create(&t2, NULL, threadFunc2, &loops);

	s = pthread_create(&t3, NULL, threadFunc3, &loops);
	
	s = pthread_create(&t4, NULL, threadFunc4, &loops);
   
    s = pthread_create(&t5, NULL, threadFunc5, &loops);

	/*s = pthread_create(&t6, NULL, threadFunc6, &loops);
	  
   
    s = pthread_create(&t7, NULL, threadFunc7, &loops);

	s = pthread_create(&t8, NULL, threadFunc8, &loops);
	
	s = pthread_create(&t9, NULL, threadFunc9, &loops); */
    
	//s = pthread_create(&t10, NULL, threadFunc10, &loops); 

    printf("glob = %d\n", glob);
    return 0;
}















