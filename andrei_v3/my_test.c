/* Code Generated from SMPL */
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

pthread_t threads[2];

void *thread_2(void **args){
  int c = *(int *)args[0];
  int d = *(int *)args[1];
  sum(c,d);
}

void *thread_1(void **args){
  int a = *(int *)args[0];
  int b = *(int *)args[1];
  sum(a,b);
}

int sum(int x,int y){
  printf("%d\n", x+y);
}

void main(){
  int a=5;
  int b=4;
  int c=7;
  int d=0;
  void *args_thread_0[2];
  args_thread_0[0] = (void *)&a;
  args_thread_0[1] = (void *)&b;
  pthread_create(&threads[0],NULL,thread_1,(void *)args_thread_0);
  void *args_thread_1[2];
  args_thread_1[0] = (void *)&c;
  args_thread_1[1] = (void *)&d;
  pthread_create(&threads[1],NULL,thread_2,(void *)args_thread_1);
  int i = 0;
  for(i=0;i<2;i++) {
     pthread_join(threads[i], NULL);
  }
}
