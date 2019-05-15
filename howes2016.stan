functions{
    real myordobs_lpmf(int obs, real f1, real f2, real tolerance, real noise){
      vector[3] ff; //stands for 'fudge-factor-enabler'. :-( All ordobs need to be feasible, even if unexpected. Pure cdf gives regions of infeasibility that are very easy to fall into.
      ff = [normal_cdf(-tolerance, f1-f2, noise),
	    normal_cdf(tolerance, f1-f2, noise)-normal_cdf(-tolerance, f1-f2, noise),
	    1-normal_cdf(tolerance, f1-f2, noise)]';
       

      return(
	     categorical_lpmf(obs | (ff+[0.001,0.001,0.001]')/1.003)//1.003 is the with-fudge total.
	     );
    }
}
data{
  int n_trials; //a trial is a single presentation, ie a unique stim/ppnt combination with 1 response.
  int n_options;
  int n_features;
  int n_ppnts;
  
  int n_ordobs;
  int ord_ppntid[n_ordobs];
  int ordtrial[n_ordobs];
  int ordopt1[n_ordobs];//indicates which options were observed.
  int ordopt2[n_ordobs];
  int ordfeature[n_ordobs];//1 means prob, 2 means payoff.
  int ordobs[n_ordobs];//observations: 1 means a<b, 2 means a==b, 3 means a>b
  real ordtolerance[n_ppnts,2]; //first entry for prob, second for payout, as indexed by ordfeature.
  real ordnoise[n_ppnts,2];
  
  int n_calcobs;
  int calcppntid[n_calcobs];
  int calctrial[n_calcobs];
  int calcoption[n_calcobs];
  real calcobs[n_calcobs];
  real calcnoise[n_ppnts];
}
parameters{
  real<lower=0,upper=1> prob[n_trials,n_options]; //would be nice to bundle these into features[,] but not sure it's possible to limit just some elements.
  real payout[n_trials,n_options];
}
transformed parameters{
  real estval[n_trials,n_options];
  for(atrial in 1:n_trials){
    for(anoption in 1:n_options){
      estval[atrial,anoption] = prob[atrial,anoption]*payout[atrial,anoption];
    }
  }
}
model{
  for(atrial in 1:n_trials){//priors
    for(anoption in 1:n_options){
      prob[atrial,anoption]~beta(1,1);
      payout[atrial,anoption]~normal(20,7);//20,7 matches wedell stim
    }
  }
  
  for(i in 1:n_ordobs){
    if(ordfeature[i]==1){
  	ordobs[i]~myordobs(
  			   prob[ordtrial[i],ordopt1[i]],
  			   prob[ordtrial[i],ordopt2[i]],
  			   ordtolerance[ord_ppntid[i],1],
  			   ordnoise[ord_ppntid[i],1]);
    }else{
  	ordobs[i]~myordobs(
  			   payout[ordtrial[i],ordopt1[i]],
  			   payout[ordtrial[i],ordopt2[i]],
  			   ordtolerance[ord_ppntid[i],2],
  			   ordnoise[ord_ppntid[i],2]);
    }
  }
  
  for(i in 1:n_calcobs){
    calcobs[i]~normal(estval[calctrial[i],calcoption[i]],calcnoise[calcppntid[i]]);
  }
}

generated quantities{
  int choice[n_trials];
  for(k in 1:n_trials){
  choice[k]=0;//washout, should never appear? Ties hopefully unlikely? This is hard-max, consider also softmax choices?
  if(estval[k,1]>estval[k,2] && estval[k,1]>estval[k,3])choice[k]=1;
  if(estval[k,2]>estval[k,1] && estval[k,2]>estval[k,3])choice[k]=2;
  if(estval[k,3]>estval[k,1] && estval[k,3]>estval[k,2])choice[k]=3;    
  }
}
