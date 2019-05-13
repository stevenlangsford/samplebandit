functions{
    real p_lessthan(real x, real coeff, real theta){
    return(1/(1+exp(coeff*x+theta)));
  }
  real p_greaterthan(real x, real coeff, real theta){
    return(1/(1+exp(-coeff*x+theta)));
  }

  real ordobs_lpmf(int obs, real f1, real f2, real coeff, real theta){
    vector[3] probvector;    
    probvector=[p_lessthan(f1-f2,coeff,theta)*(1-p_greaterthan(f1-f2,coeff,theta)),
		(1-p_lessthan(f1-f2,coeff,theta))*(1-p_greaterthan(f1-f2,coeff,theta)),
		(1-p_lessthan(f1-f2,coeff,theta))*(p_greaterthan(f1-f2,coeff,theta))]';
    probvector=probvector/sum(probvector);
    return(categorical_lpmf(obs | probvector));
  }//ordobs

}
data{
  int n_obs;
  int n_options;
  int obs[n_obs];
  int opt1[n_obs];
  int opt2[n_obs];
  real coeff;
  real theta;
}
parameters{
  real features[n_options];
}
model{
  for(i in 1:n_options){
    features[i]~normal(0,1);//priors.
  }
  for(i in 1:n_obs){
    obs[i]~ordobs(features[opt1[i]],features[opt2[i]],coeff,theta);
  }
}
