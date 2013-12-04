/* Code Generated from SMPL */
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

char stest[24] = "I am working fine now\n";
bool btest = true;
char ctest = 'c';
float ftest = 1.;
int itest = 13;

pthread_mutex_t m0=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m1=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2=PTHREAD_MUTEX_INITIALIZER;

int main(int a,int b){
printf("Hello World.. I have arrived\n");
1+2;
2-1;
1*2;
1/2;
1%50;
x&&y;
a||b;
x==y;
x!=y;
x<y;
x<=y;
x>=y;
x>(y+20);
x=10;
int x=10;
int x;
my_function(arg1,arg2);
return "hello";
return 2*(3+5)>2;
return true&&false;
return false;
return true;
break;
{
printf("hello");
a=5+5;
}
if(x>0) printf("Yay!");
if(x-2<3) {
printf(5);
int p=2;
}
else printf("Hello bitches");
while(true){
printf("THIS IS INFINITY");
}
for(i=0; i<5; i=i+1) {
pthread_mutex_lock(&m0);
sum=sum+i;
pthread_mutex_unlock(&m0);
}
for(i=0;i<num_threads;i++){
pthread_join(threads[i],NULL);
}
pthread_create(&threads[0],NULL,hello,a,b,c);
}

thread_func0();
thread_func1();
thread_func2();
