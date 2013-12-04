#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void *thread_sum(void *);
pthread_mutex_t m=PTHREAD_MUTEX_INITIALIZER;
int sum=0;

void main(){
  int i,j;
  int limit = 1000000;
  int num_threads;

  printf("Enter number of threads: ");
  scanf("%d",&num_threads);

  pthread_t threads[num_threads];
  int uppers[num_threads];
  int lowers[num_threads];
  int args[num_threads][2];

  printf("Enter upper bound: ");
  scanf("%d",&limit);

  for(i=0;i<num_threads;i++){
    uppers[i] = (limit/num_threads)*(i+1);
    lowers[i] = (limit/num_threads)*i;
    args[i][0] = lowers[i];
    args[i][1] = uppers[i];
  }

  for(i=0;i<num_threads;i++){
    pthread_create(&threads[i],NULL,
                   thread_sum,(void *)&args[i]);
  }

  for(i=0;i<num_threads;i++){
    pthread_join(threads[i],NULL);
  }

  printf("The sum of the first %d numbers is %d.\n",limit,sum);
}

void *thread_sum(void *args){
  int i;
  int *p = (int *)args;
  int lower = p[0];
  int upper = p[1];

  int temp = 0;

  for(i=lower+1;i<=upper;i++){
    temp = temp+i;
  }

  pthread_mutex_lock(&m);
  sum = sum+temp;
  pthread_mutex_unlock(&m);

  pthread_exit(NULL);
}
