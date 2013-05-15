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

static pthread_mutex_t mtx21 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx22 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx23 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx24 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx25 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx26 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx27 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx28 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx29 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx30 = PTHREAD_MUTEX_INITIALIZER; 
static pthread_mutex_t mtx31 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx32 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx33 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx34 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx35 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx36 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx37 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx38 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx39 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mtx40 = PTHREAD_MUTEX_INITIALIZER; 

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

static void *                  
threadFunc21(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx21);
		pthread_mutex_lock(&mtx22);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx22);
		pthread_mutex_unlock(&mtx21);
	//}
	return NULL;
}

static void *                  
threadFunc22(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx22);
		pthread_mutex_lock(&mtx21);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx21);
		pthread_mutex_unlock(&mtx22);
	//}
	return NULL;
}

static void *                  
threadFunc23(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx23);
		pthread_mutex_lock(&mtx24);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx24);
		pthread_mutex_unlock(&mtx23);
	//}
	return NULL;
}

static void *                  
threadFunc24(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx24);
		pthread_mutex_lock(&mtx23);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx23);
		pthread_mutex_unlock(&mtx24);
	//}
	return NULL;
}


static void *                  
threadFunc25(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx25);
		pthread_mutex_lock(&mtx26);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx26);
		pthread_mutex_unlock(&mtx25);
	//}
	return NULL;
}

static void *                  
threadFunc26(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx26);
		pthread_mutex_lock(&mtx25);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx25);
		pthread_mutex_unlock(&mtx26);
	//}
	return NULL;
}

static void *                  
threadFunc27(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx27);
		pthread_mutex_lock(&mtx28);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx28);
		pthread_mutex_unlock(&mtx27);
	//}
	return NULL;
}

static void *                  
threadFunc28(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx28);
		pthread_mutex_lock(&mtx27);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx27);
		pthread_mutex_unlock(&mtx28);
	//}
	return NULL;
}

static void *                  
threadFunc29(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx29);
		pthread_mutex_lock(&mtx30);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx30);
		pthread_mutex_unlock(&mtx29);
	//}
	return NULL;
}

static void *                  
threadFunc30(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx30);
		pthread_mutex_lock(&mtx29);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx29);
		pthread_mutex_unlock(&mtx30);
	//}
	return NULL;
}

static void *                  
threadFunc31(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx31);
		pthread_mutex_lock(&mtx32);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx32);
		pthread_mutex_unlock(&mtx31);
	//}
	return NULL;
}

static void *                  
threadFunc32(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx32);
		pthread_mutex_lock(&mtx31);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx31);
		pthread_mutex_unlock(&mtx32);
	//}
	return NULL;
}

static void *                  
threadFunc33(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx33);
		pthread_mutex_lock(&mtx34);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx34);
		pthread_mutex_unlock(&mtx33);
	//}
	return NULL;
}

static void *                  
threadFunc34(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx34);
		pthread_mutex_lock(&mtx33);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx33);
		pthread_mutex_unlock(&mtx34);
	//}
	return NULL;
}


static void *                  
threadFunc35(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx35);
		pthread_mutex_lock(&mtx36);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx36);
		pthread_mutex_unlock(&mtx35);
	//}
	return NULL;
}

static void *                  
threadFunc36(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx36);
		pthread_mutex_lock(&mtx35);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx35);
		pthread_mutex_unlock(&mtx36);
	//}
	return NULL;
}

static void *                  
threadFunc37(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx37);
		pthread_mutex_lock(&mtx38);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx38);
		pthread_mutex_unlock(&mtx37);
	//}
	return NULL;
}

static void *                  
threadFunc38(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx38);
		pthread_mutex_lock(&mtx37);
		glob += 1;
		printf("in t2 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx37);
		pthread_mutex_unlock(&mtx38);
	//}
	return NULL;
}

static void *                  
threadFunc39(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx39);
		pthread_mutex_lock(&mtx40);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx40);
		pthread_mutex_unlock(&mtx39);
	//}
	return NULL;
}

static void *                  
threadFunc40(void *arg)
{
	int loops = *((int* )arg);
	int j,s;
	//for(j=0 ; j<loops ; j++){
		pthread_mutex_lock(&mtx40);
		pthread_mutex_lock(&mtx39);
		glob += 1;
		printf("in t1 glob = %d\n", glob);
		pthread_mutex_unlock(&mtx39);
		pthread_mutex_unlock(&mtx40);
	//}
	return NULL;
}

int
main(int argc, char *argv[])
{
    pthread_t t1, t2 , t3,t4 , t5 , t6,t7, t8 , t9,t10 ,t11, t12 , t13,t14 , t15 , t16,t17, t18 , t19,t20;
	pthread_t t21, t22 , t23,t24 , t25 , t26,t27, t28 , t29,t30 ,t31, t32 , t33,t34 , t35 , t36,t37, t38 , t39,t40;
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
	
	    s = pthread_create(&t21, NULL, threadFunc21, &loops);
   
    s = pthread_create(&t22, NULL, threadFunc22, &loops);

	s = pthread_create(&t23, NULL, threadFunc23, &loops);
	
	s = pthread_create(&t24, NULL, threadFunc24, &loops);
   
    s = pthread_create(&t25, NULL, threadFunc25, &loops);

	s = pthread_create(&t26, NULL, threadFunc26, &loops);
	  
   
    s = pthread_create(&t27, NULL, threadFunc27, &loops);

	s = pthread_create(&t28, NULL, threadFunc28, &loops);
	
	s = pthread_create(&t29, NULL, threadFunc29, &loops); 
    
	s = pthread_create(&t30, NULL, threadFunc30, &loops); 
	
	    s = pthread_create(&t31, NULL, threadFunc31, &loops);
   
    s = pthread_create(&t32, NULL, threadFunc32, &loops);

	s = pthread_create(&t33, NULL, threadFunc33, &loops);
	
	s = pthread_create(&t34, NULL, threadFunc34, &loops);
   
    s = pthread_create(&t35, NULL, threadFunc35, &loops);

	s = pthread_create(&t36, NULL, threadFunc36, &loops);
	  
   
    s = pthread_create(&t37, NULL, threadFunc37, &loops);

	s = pthread_create(&t38, NULL, threadFunc38, &loops);
	
	s = pthread_create(&t39, NULL, threadFunc39, &loops); 
    
	s = pthread_create(&t40, NULL, threadFunc40, &loops); 

    printf("glob = %d\n", glob);
    return 0;
}















