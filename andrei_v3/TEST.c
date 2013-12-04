/* Code Generated from SMPL */
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

pthread_t threads[4];

void *thread_0(void *args){
	void **arg_list = (void **)args;
	int var0 = *(int *)arg_list[0];
	int var1 = *(int *)arg_list[1];
	sum(var0,var1);
}

void *thread_1(void *args){
	void **arg_list = (void **)args;
	int var0 = *(int *)arg_list[0];
	int var1 = *(int *)arg_list[1];
	sum(var0,var1);
}

void *thread_2(void *args){
	void **arg_list = (void **)args;
	int var0 = *(int *)arg_list[0];
	int var1 = *(int *)arg_list[1];
	sum(var0,var1);
}

void *thread_3(void *args){
	void **arg_list = (void **)args;
	int var0 = *(int *)arg_list[0];
	int var1 = *(int *)arg_list[1];
	sum(var0,var1);
}

int sum(int x,int y){
  printf("%d\n",x+y);
}

int main(){
int a=5;
int b=4;
int c=3;
int d=2;
void *args_thread_0[2];
args_thread_0[0] = (void *)&a;
args_thread_0[1] = (void *)&b;
pthread_create(&threads[0],NULL,thread_0,(void *)args_thread_0);
void *args_thread_1[2];
args_thread_1[0] = (void *)&a;
args_thread_1[1] = (void *)&c;
pthread_create(&threads[1],NULL,thread_1,(void *)args_thread_1);
void *args_thread_2[2];
args_thread_2[0] = (void *)&d;
args_thread_2[1] = (void *)&c;
pthread_create(&threads[2],NULL,thread_2,(void *)args_thread_2);
void *args_thread_3[2];
args_thread_3[0] = (void *)&b;
args_thread_3[1] = (void *)&c;
pthread_create(&threads[3],NULL,thread_3,(void *)args_thread_3);
int thread_counter;
for(thread_counter=0;thread_counter<4;thread_counter++){
pthread_join(threads[thread_counter],NULL);
}
printf("Done\n");
}
