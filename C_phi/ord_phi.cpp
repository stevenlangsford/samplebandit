#include <cstdio> //used only in main for test output: can delete.
#include <boost/random/mersenne_twister.hpp> //boost rng: can substitute any rng, used to make 'gen'
#include <boost/random/discrete_distribution.hpp> //boost key dependency.

//Compiled with: g++ -o ordphi ord_phi.cpp
using namespace std;

boost::random::mt19937 gen;//global for single init of seed

double ordobs_phi(double x){
  //Just ye olde normal cdf. Source: https://stackoverflow.com/questions/2328258/cumulative-normal-distribution-function-in-c-c
  // constants
  double a1 =  0.254829592;
  double a2 = -0.284496736;
  double a3 =  1.421413741;
  double a4 = -1.453152027;
  double a5 =  1.061405429;
  double p  =  0.3275911;

  // Save the sign of x
  int sign = 1;
  if (x < 0)
    sign = -1;
  x = fabs(x)/sqrt(2.0);

  // A&S formula 7.1.26
  double t = 1.0/(1.0 + p*x);
  double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
  return 0.5*(1.0 + sign*y);
}

int ordobs(double f1, double f2, double tolerance, double noise){
  double p[3];
  double diff = (f1-f2);
  //portion of a normal distribution centered at 'diff' with width 'noise' that falls before, between, and after boundaries at +/- tolerance:
  //(wrangled into standard normal cdf format)
  p[0] = ordobs_phi((-tolerance-diff)/noise);
  p[1] = ordobs_phi((tolerance-diff)/noise)-p[0];
  p[2] = 1-ordobs_phi((tolerance-diff)/noise);
  
  boost::random::discrete_distribution<> dist(p);
  return dist(gen)+1; //or symbols[dist(gen)] for some approptiate arr symbols {<,=,>}
}


int main(){
  double my_tolerance = .3;
  double my_noise = .3;
  freopen("ordphi_output.csv","w",stdout);
  cout << "ordobs,a,b,tolerance,noise\n";
  for(double a =-2; a<2; a = a + 0.25){
    for(double b =-2; b<2; b = b + 0.25){
      for(int rep = 0; rep<50; rep++){
	cout <<  ordobs(a, b, my_tolerance, my_noise) <<","<< a << "," << b << "," << my_tolerance <<","<< my_noise << "\n";
      }
    }
  }
  return 0;
}
