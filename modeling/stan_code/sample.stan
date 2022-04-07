data{
  int<lower=1>  nTrial;
  int<lower=1>  nSub;
  int<lower=1>  nStim;
  int<lower=1>  nResp;
  int<lower=1>  K;
  int<lower=1>  s[nSub, nTrial];
  int<lower=1>  a[nSub, nTrial];
  int<lower=1>  ya[nSub, nTrial];
  int<lower=-1> r[nSub, nTrial];
  int<lower=1>  rew[nSub, nTrial];
  int<lower=1>  yax[nTrial*nSub];
  int<lower=1>  nData;
  
  matrix[nResp,nStim]   Qi;
  vector[nStim]         Vi;
} # end data definitions.


# define parameters in 'fitting space'
# -------------------------------------------------------------------------.
# parameter specifications.
parameters{

  # Hierarchical parameters.
  vector[K] X;
  vector<lower=0, upper=20>[K] sdX;
  
  # Subject level parameters.
  vector<lower=-10,  upper=10>[nSub] x1; # rho.
  vector<lower=-8,  upper=8>[nSub] x2; # epsilon.
  vector<lower=-8,  upper=8>[nSub] x3; # go bias.
  vector<lower=-8,  upper=8>[nSub] x4; # pavlovian bias.
  vector<lower=-8,  upper=8>[nSub] x5; # epsilon bias.

} # end parameter definition.


# compute transformed parameters plus likelihood.
# -------------------------------------------------------------------------.
transformed parameters{

  real rho[nSub];
  real<lower=0, upper=1> epsilon[nSub];
  real gobias[nSub];
  real pibias[nSub];
  vector[2] biaseps[nSub];
  
  matrix[nResp,nStim] Q;
  vector[nResp]       q;
  vector[nStim]       V;
  simplex[nResp]      p0;
  real                er;
  vector[nResp]       BGx[nTrial*nSub];
  
  for(iSub in 1:nSub){
    # retrieve sampled subject parameters and initial values.
    rho[iSub]          = exp(x1[iSub]);
    epsilon[iSub]      = inv_logit(x2[iSub]);
    biaseps[iSub,2]    = inv_logit(x2[iSub]-x5[iSub]);
    biaseps[iSub,1]    = epsilon[iSub] + (epsilon[iSub] - biaseps[iSub,2]);
    gobias[iSub]       = x3[iSub];
    pibias[iSub]       = x4[iSub];
  }
  
  for(iSub in 1:nSub){
    # retrieve and initial values.
    Q            = Qi * rho[iSub];
    V            = Vi;
  
    for(iTrial in 1:nTrial){
      # retrieve Q-values.
        for(iResp in 1:nResp){
          q[iResp] = Q[iResp,s[iSub,iTrial]]; 
        }
    for(iResp in 1:2){
      q[iResp] = q[iResp] + gobias[iSub] + pibias[iSub] * V[s[iSub,iTrial]]; 
    }
    # retrieve choice probabililty with softmax of Q-values.
    p0         = softmax(q); # probability of each action with softmax.
    for(iResp in 1:nResp){
      BGx[(iSub-1)*nTrial+iTrial,iResp] = p0[iResp];
    } # end iResp-loop.
  
    # update Q- and V-values.
    er         = rho[iSub] * r[iSub,iTrial];  # update outcome with outcome sensitivity.
  
    if ((ya[iSub,iTrial]==3) && (r[iSub,iTrial]==-1)) # punished nogo.
      Q[ya[iSub,iTrial],s[iSub,iTrial]] = Q[ya[iSub,iTrial],s[iSub,iTrial]] + biaseps[iSub,2] * (er - Q[ya[iSub,iTrial],s[iSub,iTrial]]);
    else if ((!ya[iSub,iTrial] == 3) && (r[iSub,iTrial]==1)) # rewarded go.
      Q[ya[iSub,iTrial],s[iSub,iTrial]] = Q[ya[iSub,iTrial],s[iSub,iTrial]] + biaseps[iSub,1] * (er - Q[ya[iSub,iTrial],s[iSub,iTrial]]);
    else # neutral outcome.
      Q[ya[iSub,iTrial],s[iSub,iTrial]] = Q[ya[iSub,iTrial],s[iSub,iTrial]] + epsilon[iSub] * (er - Q[ya[iSub,iTrial],s[iSub,iTrial]]);
    } # end iTrial-loop.
  } # end iSub-loop.

} # end transformed parameter definition.


# specify the prior distribution and likelihoods.
# -------------------------------------------------------------------------.
model{
  vector[nResp] theta;

  # Hierarchical priors: sample values from normal distributions.
  X[1]  ~ normal(0, 3); # feedback sensitivity.
  X[2]  ~ normal(0, 2); # instrumental learning rate.
  X[3]  ~ normal(0, 3); # go bias.
  X[4]  ~ normal(0, 3); # pavovian bias.
  X[5]  ~ normal(0, 2); # learning bias.
  sdX   ~ cauchy(0, 2); # note that this is a half-cauchy, recommended by e.g. Gelman (2006)/stan documentation.

  # Subject level priors are sampled from the hierarchical prior.
  x1 ~ normal(X[1], sdX[1]);
  x2 ~ normal(X[2], sdX[2]);
  x3 ~ normal(X[3], sdX[3]);
  x4 ~ normal(X[4], sdX[4]);
  x5 ~ normal(X[5], sdX[5]);

# Likelihood of chosen action given the prob of each responses. 
# This likelihood is what is being optimized.
  for(iData in 1:nData){
    yax[iData] ~ categorical(BGx[iData]);
  }
}
