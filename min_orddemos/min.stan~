functions{
  real ordobs_lpmf(int obs, real f1, real f2, real tolerance, real noise){
    return(
	   categorical_lpmf(obs | [normal_cdf(-tolerance, f1-f2, noise),
				 normal_cdf(tolerance, f1-f2, noise)-normal_cdf(-tolerance, f1-f2, noise),
				   1-normal_cdf(tolerance, f1-f2, noise)]')
	   );
  }
}
data{
  int n_obs;
  int n_options;
  int obs[n_obs];
  int opt1[n_obs];
  int opt2[n_obs];
  real tolerance;
  real noise;
}
parameters{
  real features[n_options];
}
model{
  for(i in 1:n_options){
    features[i]~normal(0,1);//priors.
  }
  for(i in 1:n_obs){
    obs[i]~ordobs(features[opt1[i]],features[opt2[i]],tolerance,noise);
  }
}
