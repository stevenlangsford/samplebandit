#include <boost/random/mersenne_twister.hpp> //boost rng: can substitute any rng, used to make 'gen'
#include <boost/random/discrete_distribution.hpp> //boost key dependency.

boost::random::mt19937 gen;//set as global for single init of seed
OrdObs outcomes[3] = {LT,EQ,GT}; //These are just ints, don't need to redefine here if you can figure out which is which.

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

OrdObs Agent::observeOrd(float x1, float x2, float threshold) {
  float noise = .3;//maybe better to pass as argument, but trying to match existing call sig. The meaning of existing 'threshold' changes a little.
  float p[3];
  float diff = (x1-x2);
  //portion of a normal distribution centered at 'diff' with width 'noise' that falls before, between, and after boundaries at +/- threshold:
  //(wrangled into standard normal cdf format)
  p[0] = ordobs_phi((-threshold-diff)/noise);
  p[1] = ordobs_phi((threshold-diff)/noise)-p[0];
  p[2] = 1-ordobs_phi((threshold-diff)/noise);

  boost::random::discrete_distribution<> dist(p);


  return outcomes[dist(gen)];
}
