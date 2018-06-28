#include <stdio.h>

// float plus (float a, float b) {
//   float _a1 = a + b;
//   return _a1;
// }

float sum ( float a ){
        float _result;
        if ( a > 0.0 ) {
                _result = ( a + sum ( a - 1.0 ) );
        } else {
                _result = ( 0.0 );
        }
        return _result;
}

void main (){
    int result =  sum(5);
    printf("%d", result);
    return 0;
}