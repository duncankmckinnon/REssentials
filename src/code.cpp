#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

bool isPrime(int x){
  if( x < 0 ){
    return( false );
  } else if( x < 4) {
    return( true );
  }
  int n = 2;
  int m = floor(sqrt(x));
  while( n <= m ){
    if( x % n == 0 ){
      return( false );
    }
    n++;
  }
  return( true );
}
