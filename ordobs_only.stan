functions{
  real features_to_value(real[] features){
    return features[1]+features[2]; //might want to mess around with this. In particular, products are interesting, so are weights?
  }
}
data{
  int n_features;
  int n_options;
  
  int n_ordobs;
  int ordoption1[n_ordobs];
  int ordoption2[n_ordobs];
  int ord_whichfeature[n_ordobs];
  real ordobs_obsvalue[n_ordobs];
}
transformed data{
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
  
  for(i in 1:n_ordobs){
    ordobs_obsvalue[i]~normal(
  			   features[ordoption1[i],ord_whichfeature[i]]-features[ordoption2[i],ord_whichfeature[i]],ordnoise); //note order is opt1-opt2.
  } 
  
}

generated quantities{
  real estval[n_options];
  //could est winmargin or est regret here?
  for(i in 1:n_options){
    estval[i] = features_to_value(features[i,]);
  }  
}
