#include <pthread.h>
#include <stdio.h>
#include <errno.h>
static pthread_mutex_t mtx1= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx2= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx3= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx4= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx5= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx6= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx7= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx8= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx9= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx10= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx11= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx12= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx13= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx14= PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t mtx15= PTHREAD_MUTEX_INITIALIZER
static void* threadFun1(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx3)!=0){}
pthread_mutex_lock(&mtx1);
if(pthread_mutex_trylock(&mtx2)==0){
pthread_mutex_unlock(&mtx3);
pthread_mutex_unlock(&mtx1);
pthread_mutex_unlock(&mtx2);
}
else {
pthread_mutex_unlock(&mtx3);
pthread_mutex_unlock(&mtx1);
}
}
return NULL;
}
static void* threadFun2(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx4)!=0){}
while(1){
if(pthread_mutex_trylock(&mtx5)==0){
if(pthread_mutex_trylock(&mtx6)==0){
pthread_mutex_unlock(&mtx6);
pthread_mutex_unlock(&mtx5);
pthread_mutex_unlock(&mtx4);
break;
}
else{
pthread_mutex_unlock(&mtx5);
}
}
}
}
return NULL;
}
static void* threadFun3(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx6)!=0){}
pthread_mutex_lock(&mtx4);
if(pthread_mutex_trylock(&mtx5)==0){
pthread_mutex_unlock(&mtx6);
pthread_mutex_unlock(&mtx4);
pthread_mutex_unlock(&mtx5);
}
else {
pthread_mutex_unlock(&mtx6);
pthread_mutex_unlock(&mtx4);
}
}
return NULL;
}
static void* threadFun4(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx7)!=0){}
while(1){
if(pthread_mutex_trylock(&mtx8)==0){
if(pthread_mutex_trylock(&mtx9)==0){
pthread_mutex_unlock(&mtx9);
pthread_mutex_unlock(&mtx8);
pthread_mutex_unlock(&mtx7);
break;
}
else{
pthread_mutex_unlock(&mtx8);
}
}
}
}
return NULL;
}
static void* threadFun5(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx9)!=0){}
pthread_mutex_lock(&mtx7);
if(pthread_mutex_trylock(&mtx8)==0){
pthread_mutex_unlock(&mtx9);
pthread_mutex_unlock(&mtx7);
pthread_mutex_unlock(&mtx8);
}
else {
pthread_mutex_unlock(&mtx9);
pthread_mutex_unlock(&mtx7);
}
}
return NULL;
}
static void* threadFun6(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx10)!=0){}
while(1){
if(pthread_mutex_trylock(&mtx11)==0){
if(pthread_mutex_trylock(&mtx12)==0){
pthread_mutex_unlock(&mtx12);
pthread_mutex_unlock(&mtx11);
pthread_mutex_unlock(&mtx10);
break;
}
else{
pthread_mutex_unlock(&mtx11);
}
}
}
}
return NULL;
}
static void* threadFun7(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx12)!=0){}
pthread_mutex_lock(&mtx10);
if(pthread_mutex_trylock(&mtx11)==0){
pthread_mutex_unlock(&mtx12);
pthread_mutex_unlock(&mtx10);
pthread_mutex_unlock(&mtx11);
}
else {
pthread_mutex_unlock(&mtx12);
pthread_mutex_unlock(&mtx10);
}
}
return NULL;
}
static void* threadFun8(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx13)!=0){}
while(1){
if(pthread_mutex_trylock(&mtx14)==0){
if(pthread_mutex_trylock(&mtx15)==0){
pthread_mutex_unlock(&mtx15);
pthread_mutex_unlock(&mtx14);
pthread_mutex_unlock(&mtx13);
break;
}
else{
pthread_mutex_unlock(&mtx14);
}
}
}
}
return NULL;
}
static void* threadFun9(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx15)!=0){}
pthread_mutex_lock(&mtx13);
if(pthread_mutex_trylock(&mtx14)==0){
pthread_mutex_unlock(&mtx15);
pthread_mutex_unlock(&mtx13);
pthread_mutex_unlock(&mtx14);
}
else {
pthread_mutex_unlock(&mtx15);
pthread_mutex_unlock(&mtx13);
}
}
return NULL;
}
static void* threadFun10(void *arg){
int loop = (int)(*((int*)arg));
	while(loop-- > 0){
while(pthread_mutex_trylock(&mtx16)!=0){}
while(1){
if(pthread_mutex_trylock(&mtx17)==0){
if(pthread_mutex_trylock(&mtx18)==0){
pthread_mutex_unlock(&mtx18);
pthread_mutex_unlock(&mtx17);
pthread_mutex_unlock(&mtx16);
break;
}
else{
pthread_mutex_unlock(&mtx17);
}
}
}
}
return NULL;
}
int main()
{
pthread_t t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
int loops;
pthread_create(&t1, NULL, threadFunc1, &loops);
pthread_create(&t2, NULL, threadFunc2, &loops);
pthread_create(&t3, NULL, threadFunc3, &loops);
pthread_create(&t4, NULL, threadFunc4, &loops);
pthread_create(&t5, NULL, threadFunc5, &loops);
pthread_create(&t6, NULL, threadFunc6, &loops);
pthread_create(&t7, NULL, threadFunc7, &loops);
pthread_create(&t8, NULL, threadFunc8, &loops);
pthread_create(&t9, NULL, threadFunc9, &loops);
pthread_create(&t10, NULL, threadFunc10, &loops);return 0;
}
