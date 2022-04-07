data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> B;
  int<lower=1, upper=T> Tsubj[N,2];
  real outcome[N, T, 2];
  int<lower=-1, upper=1> pressed[N, T, 2];
  int<lower=1, upper=4> cue[N, T, 2];
}
transformed data {
  vector[4] initV;
  initV = rep_vector(0.0, 4);
} 
parameters {
  // declare as vectors for vectorizing
  vector[7] mu_p;  
  vector<lower=0>[7] sigma; 
  vector[N] xi_pr;        // noise 
  vector[N] ep_pr;        // learning rate 
  vector[N] b_pr;       // baseline go bias 
  vector[N] pi_0_rew_pr;      // base pavlovian bias
  vector[N] pi_delta_rew_pr;  // change pavlovian bias
  vector[N] rhoRew_pr;    // rho reward, inv temp
  vector[N] rhoPun_pr;    // rho punishment, inv temp 
}
transformed parameters {
  vector<lower=0,upper=1>[N] xi;
  vector<lower=0,upper=1>[N] ep;
  vector[N] b; 
  vector[N] pi_0_rew;
  vector[N] pi_delta_rew;
  vector<lower=0>[N] rhoRew;
  vector<lower=0>[N] rhoPun;
     
  for (i in 1:N) {
    xi[i] = Phi_approx( mu_p[1] + sigma[1] * xi_pr[i] );
    ep[i] = Phi_approx( mu_p[2] + sigma[2] * ep_pr[i] );
  }
  b = mu_p[3] + sigma[3] * b_pr; // vectorization
  pi_0_rew  = mu_p[4] + sigma[4] * pi_0_rew_pr;
  pi_delta_rew  = mu_p[5] + sigma[5] * pi_delta_rew_pr;
  rhoRew = exp( mu_p[6] + sigma[6] * rhoRew_pr );
  rhoPun = exp( mu_p[7] + sigma[7] * rhoPun_pr );
}
model {  
// gng_m4: RW(rew/pun) + noise + bias + pi model (M5 in Cavanagh et al 2013 J Neuro)
  // hyper parameters
  mu_p[1]  ~ normal(0, 1.0); 
  mu_p[2]  ~ normal(0, 1.0); 
  mu_p[3]  ~ normal(0, 10.0); 
  mu_p[4]  ~ normal(0, 10.0); 
  mu_p[5]  ~ normal(0, 1.0); 
  mu_p[6]  ~ normal(0, 1.0); 
  mu_p[7]  ~ normal(0, 1.0); 
  sigma ~ cauchy(0, 2.0);
  
  // individual parameters w/ Matt trick
  xi_pr  ~ normal(0, 1.0);   
  ep_pr  ~ normal(0, 1.0);   
  b_pr   ~ normal(0, 1.0); 
  pi_0_rew_pr  ~ normal(0, 1.0); 
  pi_delta_rew_pr  ~ normal(0, 1.0); 
  rhoRew_pr ~ normal(0, 1.0);
  rhoPun_pr ~ normal(0, 1.0);
  
  for (i in 1:N) {
    vector[4] wv_g;  // action wegith for go
    vector[4] wv_ng; // action wegith for nogo
    vector[4] qv_g;  // Q value for go
    vector[4] qv_ng; // Q value for nogo
    vector[4] sv;    // stimulus value 
    vector[4] pGo;   // prob of go (press)
    real go_bias;
    real pav_bias_rew;
    
    for (blk in 1:B) {
      wv_g  = initV;
      wv_ng = initV;
      qv_g  = initV;
      qv_ng = initV;
      sv    = initV;
      
      go_bias = b[i];
      if (blk==1) {
        pav_bias_rew = pi_0_rew[i];
      } else if (blk==2) {
        pav_bias_rew = pi_0_rew[i] + pi_delta_rew[i];
      }
      
      for (t in 1:Tsubj[i,blk])  {
        if (cue[i,t,blk] < 3){ // I set cue 1, 2 --> go_reward, nogo_reward.  
          wv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + go_bias + pav_bias_rew * sv[ cue[i,t,blk] ];
          wv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ];  // qv_ng is always equal to wv_ng (regardless of action)  
        } else {
          wv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + go_bias + sv[ cue[i,t,blk] ];
          wv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ];  // qv_ng is always equal to wv_ng (regardless of action)  
        }
        pGo[ cue[i,t,blk] ]   = inv_logit( wv_g[ cue[i,t,blk] ] - wv_ng[ cue[i,t,blk] ] ); 
        pGo[ cue[i,t,blk] ]   = pGo[ cue[i,t,blk] ] * (1 - xi[i]) + xi[i]/2;  // noise
        pressed[i,t,blk] ~ bernoulli( pGo[ cue[i,t,blk] ] );
        
        // after receiving feedback, update sv[t+1]
        if (outcome[i,t,blk] >= 0) {
          sv[ cue[i,t,blk] ] = sv[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - sv[ cue[i,t,blk] ] );
        } else {
          sv[ cue[i,t,blk] ] = sv[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - sv[ cue[i,t,blk] ] );
        }

        // update action values
        if (pressed[i,t,blk]) { // update go value 
          if (outcome[i,t,blk] >=0) {
            qv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - qv_g[ cue[i,t,blk] ]);
          } else {
            qv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - qv_g[ cue[i,t,blk] ]);
          }
        } else { // update no-go value  
          if (outcome[i,t,blk] >=0) {
            qv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - qv_ng[ cue[i,t,blk] ]);  
          } else{
            qv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - qv_ng[ cue[i,t,blk] ]);  
          }
        }  
      } // end of t loop
    } // end of b loop
  } // end of i loop
}
generated quantities {
  real<lower=0, upper=1> mu_xi;
  real<lower=0, upper=1> mu_ep;
  real mu_b;
  real mu_pi_0_rew;
  real mu_pi_delta_rew;
  real<lower=0> mu_rhoRew;
  real<lower=0> mu_rhoPun;
  
  real log_lik[N];
  
  mu_xi  = Phi_approx(mu_p[1]);
  mu_ep  = Phi_approx(mu_p[2]);
  mu_b   = mu_p[3];
  mu_pi_0_rew  = mu_p[4];
  mu_pi_delta_rew  = mu_p[5];
  mu_rhoRew = exp(mu_p[6]);
  mu_rhoPun = exp(mu_p[7]); 

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[4] wv_g;  // action wegith for go
      vector[4] wv_ng; // action wegith for nogo
      vector[4] qv_g;  // Q value for go
      vector[4] qv_ng; // Q value for nogo
      vector[4] sv;    // stimulus value 
      vector[4] pGo;   // prob of go (press) 
      real go_bias;
      real pav_bias_rew;
      
      log_lik[i] = 0;
      
      for (blk in 1:B) {
        wv_g  = initV;
        wv_ng = initV;
        qv_g  = initV;
        qv_ng = initV;
        sv    = initV;
        
        go_bias = b[i];
        if (blk==1) {
          pav_bias_rew = pi_0_rew[i];
        } else if (blk==2) {
          pav_bias_rew = pi_0_rew[i] + pi_delta_rew[i];
        }
        
        for (t in 1:Tsubj[i,blk])  {
          if (cue[i,t,blk] < 3){ // I set cue 1, 2 --> go_reward, nogo_reward.  
            wv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + go_bias + pav_bias_rew * sv[ cue[i,t,blk] ];
            wv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ];  // qv_ng is always equal to wv_ng (regardless of action)  
          } else {
            wv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + go_bias + sv[ cue[i,t,blk] ];
            wv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ];  // qv_ng is always equal to wv_ng (regardless of action)  
          }
          
          pGo[ cue[i,t,blk] ]   = inv_logit( wv_g[ cue[i,t,blk] ] - wv_ng[ cue[i,t,blk] ] ); 
          pGo[ cue[i,t,blk] ]   = pGo[ cue[i,t,blk] ] * (1 - xi[i]) + xi[i]/2;  // noise
          log_lik[i] = log_lik[i] + bernoulli_lpmf( pressed[i,t,blk] | pGo[ cue[i,t,blk] ] );
      
          // after receiving feedback, update sv[t+1]
          if (outcome[i,t,blk] >= 0) {
            sv[ cue[i,t,blk] ] = sv[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - sv[ cue[i,t,blk] ] );
          } else {
            sv[ cue[i,t,blk] ] = sv[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - sv[ cue[i,t,blk] ] );
          }
  
          // update action values
          if (pressed[i,t,blk]) { // update go value 
            if (outcome[i,t,blk] >=0) {
              qv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - qv_g[ cue[i,t,blk] ]);
            } else {
              qv_g[ cue[i,t,blk] ]  = qv_g[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - qv_g[ cue[i,t,blk] ]);
            }
          } else { // update no-go value  
            if (outcome[i,t,blk] >=0) {
              qv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ] + ep[i] * ( rhoRew[i] * outcome[i,t,blk] - qv_ng[ cue[i,t,blk] ]);  
            } else {
              qv_ng[ cue[i,t,blk] ] = qv_ng[ cue[i,t,blk] ] + ep[i] * ( rhoPun[i] * outcome[i,t,blk] - qv_ng[ cue[i,t,blk] ]);  
            }
          }
        } // end of t loop
      } // end of b loop
    } // end of i loop
  } // end of local section
}
