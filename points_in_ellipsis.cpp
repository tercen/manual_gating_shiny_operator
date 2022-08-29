#include <vector>
#include "Rcpp.h"
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]

vector <short> points_in_ellipsis(vector<double> x,vector<double> y,
                                  double ex, double ey,
                                  double rx, double ry){
  
  vector <short> flags(x.size());
  
  //for (auto tup : boost::combine(x, y)) { 
  // for (auto i : x) { 
  for( unsigned int i = 0; i < x.size(); i ++ ){
    flags[i] = 0;
    
    if( ( ( pow(x[i]-ex, 2) / rx) + ( pow(y[i]-ey, 2) / ry)) <= 1){
      flags[i] = 1;
    }
  }
  
  return flags;
}

int main(){
  return 0;
}