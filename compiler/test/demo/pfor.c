#include <stdio.h>
#include <string.h>

int sum = 0;

int isPrime(int n){
  int limit = n/2;
  int i = 0;
  for(i=2; i<=limit; i++)
    if(n%i == 0)
      return 0;
  return 1;
}

int main(){
  int i;  
  int n = 1000000;

  for(i=0;i<n;i++){
    if(isPrime(i))
      sum = sum+i;
  }

  printf("The sum of the first 1M primes is %d.\n",sum);
}
