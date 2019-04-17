functions{
  real features_to_value(real[] features){
    return features[1]+features[2]; //might want to mess around with this. In particular, products are interesting, so are weights?
  }
}
data{
  int n_features;
  int n_options;
  int n_trials;
  
  int n_calcobs;
  int calcobs_whichoption[n_calcobs];
  int calcobs_whichfeature[n_calcobs];
  int calcobs_whichtrial[n_calcobs];
  real calcobs_obsvalue[n_calcobs];


  real calcnoise;
  real ordnoise;
  /* int n_ordobs; */
  /* int ordoption1[n_ordobs]; */
  /* int ordoption2[n_ordobs]; */
  /* int ord_whichfeature[n_ordobs]; */
  /* int ord_whichtrial[n_ordobs]; */
  /* real ordobs_obsvalue[n_ordobs]; */
}

parameters{
  real features[n_trials,n_options,n_features];
}
model{
  //priors: try some skew? Try different distributions for different features?
  for(k in 1:n_trials){
    for(i in 1:n_options){
      for(j in 1:n_features){
	features[k,i,j]~normal(0,1);
      }
    }
  }
  
  //obs:
  for(i in 1:n_calcobs){
    calcobs_obsvalue[i]~normal(features[calcobs_whichtrial[i],calcobs_whichoption[i],calcobs_whichfeature[i]],calcnoise);
  }
  /* for(i in 1:n_ordobs){ */
  /*   ordobs_obsvalue[i]~normal( */
  /* 			      features[ord_whichtrial[i],ordoption1[i],ord_whichfeature[i]]-features[ord_whichtrial[i],ordoption2[i],ord_whichfeature[i]],ordnoise); //note order is opt1-opt2. */
  /* }  */
  
}

generated quantities{
  real estval[n_trials,n_options];
  //could est winmargin or est regret here?
  for(k in 1:n_trials){
    for(i in 1:n_options){
      estval[k,i] = features_to_value(features[k,i,]);
    }
  }
}
