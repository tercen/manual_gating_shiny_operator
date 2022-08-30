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

// [[Rcpp::export]]
vector <short> points_in_polygon( vector<double> x, vector<double> y,
                                  vector<double> px, vector<double> py){
  vector <short> flags(x.size());
  short i,j, c;
  double tx, ty;
  int nvert = px.size();
  for( unsigned int k = 0; k < x.size(); k ++ ){
    tx = x[k];
    ty = y[k];
    c = 0;
    i = 0;
    j = 0;
    
    for (i = 0, j = nvert-1; i < nvert; j = i++) {
      if ( ((py[i] > ty) != (py[j] > ty)) &&
           (tx < (px[j]-px[i]) * (ty-py[i]) / (py[j]-py[i]) + px[i]) ){
          c = !c;
        }
    }
    
    flags[k] = c;
  }
  
  return flags;
}

int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
         (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
      c = !c;
  }
  return c;
}


int main(){
  return 0;
}