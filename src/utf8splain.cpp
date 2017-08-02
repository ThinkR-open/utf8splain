#include <Rcpp.h>
using namespace Rcpp;

int parse_one_binary( const char* s){
  int n = strlen(s) ;
  int res = 0 ;
  int m = 1 ;
  for(int i=n-1; i>=0; i--, m*=2){
    if( s[i] == '1' ) res += m ;
  }
  return res ;
}


// [[Rcpp::export]]
IntegerVector parse_binary(CharacterVector s) {
  return sapply( s, parse_one_binary ) ;
}
