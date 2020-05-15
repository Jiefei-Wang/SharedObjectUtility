#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test(SEXP x){
    DATAPTR(x);
}