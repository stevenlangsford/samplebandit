functions{
  real p_lessthan(real x, real coeff, real theta){
    return(1/(1+exp(coeff*x+theta)));
  }
  real p_greaterthan(real x, real coeff, real theta){
    return(1/(1+exp(-coeff*x+theta)));
  }

  real myordobs_lpmf(int obs, real mydist, real coeff, real theta){
    vector[3] probvector;    
    probvector=[p_lessthan(mydist,coeff,theta)*(1-p_greaterthan(mydist,coeff,theta)),
		(1-p_lessthan(mydist,coeff,theta))*(1-p_greaterthan(mydist,coeff,theta)),
		(1-p_lessthan(mydist,coeff,theta))*(p_greaterthan(mydist,coeff,theta))]';
    probvector=probvector/sum(probvector);
    return(categorical_lpmf(obs | (probvector+[.001,.001,.001]')/1.003)); //fudge factor because all ordobs need to be somewhat feasible, zeros are bad.
  }//ordobs
  
}
data{
  int n_trials;
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
  real ordcoeff[n_ppnts,2]; //first entry for prob, second for payout, as indexed by ordfeature.
  real ordtheta[n_ppnts,2];
  
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
      payout[atrial,anoption]~normal(20,7);//needs to ~match actual stim.
    }
  }
  
  for(i in 1:n_ordobs){
    if(ordfeature[i]==1){
  	ordobs[i]~myordobs(
  			  prob[ordtrial[i],ordopt1[i]]-prob[ordtrial[i],ordopt2[i]],
  			  ordcoeff[ord_ppntid[i],1],
  			  ordtheta[ord_ppntid[i],1]);
    }else{
  	ordobs[i]~myordobs(
  			  payout[ordtrial[i],ordopt1[i]]-payout[ordtrial[i],ordopt2[i]],
  			  ordcoeff[ord_ppntid[i],2],
  			  ordtheta[ord_ppntid[i],2]);
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
