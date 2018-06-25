#include <stdio.h>

float plus (float a, float b) {
  float _a1 = a + b;
  return _a1;
}


void main (){
    int result =  plus(1, 4);
    printf("%d", result);
    return 0;
}