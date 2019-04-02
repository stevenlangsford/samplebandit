functions{
  real features_to_value(real[] features){
    return features[1]+features[2]; //might want to mess around with this. In particular, products are interesting, so are weights?
  }
}
data{
  int n_features;
  int n_options;
  
  int n_calcobs;
  int calcobs_whichoption[n_calcobs];
  int calcobs_whichfeature[n_calcobs];
  real calcobs_obsvalue[n_calcobs];
}
transformed data{
  real calcnoise = 0.5; //other options for setting this? Pass as an arg, fit as a param? Should calcobsnoise be difference from ordobsnoise?
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
  
  //obs:
  for(i in 1:n_calcobs){
    calcobs_obsvalue[i]~normal(features[calcobs_whichoption[i],calcobs_whichfeature[i]],calcnoise);
  }
  
}

generated quantities{
  real estval[n_options];
  //could est winmargin or est regret here?
  for(i in 1:n_options){
    estval[i] = features_to_value(features[i,]);
  }  
}
