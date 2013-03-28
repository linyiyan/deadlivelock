#include <pthread.h>
#include <stdio.h>
#include <errno.h>

static int glob = 0;
static pthread_mutex_t mtx1 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx2 = PTHREAD_MUTEX_INITIALIZER;

static void *                  
threadFunc1(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	for(j=0 ; j<loops ; j++){
		rec: pthread_mutex_lock(&mtx1);
		/* s = pthread_mutex_trylock(&mtx2);
		if(s==EBUSY){
			pthread_mutex_unlock(&mtx1);
			goto rec;
		}*/
		pthread_mutex_lock(&mtx2);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx2);
		pthread_mutex_unlock(&mtx1);
	}
	return NULL;
}

static void *                  
threadFunc2(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	for(j=0 ; j<loops ; j++){
		rec: pthread_mutex_lock(&mtx2);
		s=pthread_mutex_trylock(&mtx1);
		if(s==EBUSY){
			pthread_mutex_unlock(&mtx2);
			goto rec;
		}
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx1);
		pthread_mutex_unlock(&mtx2);
	}
	return NULL;
}

int
main(int argc, char *argv[])
{
    pthread_t t1, t2;
    int loops, s;

    loops = 100000;

    s = pthread_create(&t1, NULL, threadFunc1, &loops);
   
    s = pthread_create(&t2, NULL, threadFunc2, &loops);
  
    s = pthread_join(t1, NULL);
   
    s = pthread_join(t2, NULL);
  
    printf("glob = %d\n", glob);
    return 0;
}















