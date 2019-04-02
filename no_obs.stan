functions{
  real features_to_value(real[] features){
    return features[1]+features[2]; //might want to mess around with this. In particular, products are interesting, so are weights?
  }
}
data{
  //crazy to have no-obs, but nice to see the info value of the 1st observations
  //(people seriously might skip a 1st calcobs sometimes?)
  int n_features;
  int n_options;
}
transformed data{
  real calcnoise = 0.5; //other options for setting this? Pass as an arg, fit as a param? Should calcobsnoise be difference from ordobsnoise?
  real ordnoise = 0.5;// Hm. Probably not lower than calcobsnoise, but could be somewhere between double and equal? Play with this?
}
parameters{
  real features[n_options,n_features];
}
model{
  //priors: try some skew? Try different distributions for different features?
  for(i in 1:n_options){
    for(j in 1:n_features){
      features[i,j]~normal(0,1);
    }
  }  
}

generated quantities{
  real estval[n_options];
  //could est winmargin or est regret here?
  for(i in 1:n_options){
    estval[i] = features_to_value(features[i,]);
  }  
}
