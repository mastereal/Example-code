// functions{

     // real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
     //   real ss;
      //  real log_s;
      //  real mus;
      //  int  miny;

     //   miny = min(r[1], r[2]);

     //   ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
      //    exp(mu3);
      //  if(miny > 0) {
       //   mus = -mu1-mu2+mu3;
        //  log_s = ss;

         // for(k in 1:miny) {
           // log_s = log_s + log(r[1] - k + 1) + mus
          //  + log(r[2] - k + 1)
          //  - log(k);
           // ss = log_sum_exp(ss, log_s);
         // }
       // }
        //return(ss);
      //}
       // real partial_sum(int[,] y_sliced,
         //               int start, int end, vector beta1, vector beta2,vector beta3){
           //             real lt=0.00001;
                      //  vector[N] beta1;
                      //  vector[N] beta2;
                      //  vector[N] beta3;
                      //  for (n in 1:N){
                      //  beta1[n]=parm_sliced[n,1];
                      //  beta2[n]=parm_sliced[n,2];
                      //  beta3[n]=parm_sliced[n,3];
                      //  }
             //           lt += poisson_lpmf(y_sliced[,1]|beta1[start:end]+beta3[start:end]);
               //         lt += poisson_lpmf(y_sliced[,2]|beta2[start:end]+beta3[start:end]);
                 //       return lt;
                   //     }
    //}
    data{
      int N;   // number of games
      int y[N,2];
      int N_prev;
      
      int nteams;
      int seasonteams;
      int team1[N];
      int team2[N];
      int ntimes;                 // dynamic periods
      int time[ntimes];
      int instants[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      int instants_prev[N_prev];
      real ranking[nteams];

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      matrix[ntimes, nteams] att_raw;        // raw attack ability
      matrix[ntimes, nteams] def_raw;        // raw defense ability
      real rho;
      real home;
      real<lower=0.000001> sigma_att;
      real<lower=0.000001> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      //cov_matrix[ntimes] Sigma_att;         // Gaussian process attack cov. funct.
      //cov_matrix[ntimes] Sigma_def;        // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                 // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;
      //real parm_sliced[N,3];              //pack parameters for reduce sum
      
      


      //Gaussian process covariance functions
      //for (i in 1:(ntimes)){
           //for (j in 1:(ntimes)){
               //Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
               //+ (i == j ? 0.1 : 0.0);
               //Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
                //           + (i == j ? 0.1 : 0.0);
            // }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1,]=att_raw[1,]-mean(att_raw[1,]);
      def[1,]=def_raw[1,]-mean(def_raw[1,]);
      for (t in 2:ntimes){
        att[t,]=att_raw[t,]-mean(att_raw[t,]);
        def[t,]=def_raw[t,]-mean(def_raw[t,]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1,]=rep_row_vector(hyper_location,nteams);
        mu_att[t,]= att[t-1,];
        //rep_row_vector(0,nteams);

        mu_def[1,]=rep_row_vector(hyper_location, nteams);
        mu_def[t,]= def[t-1,];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[team1[n]]-ranking[team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[team1[n]]-ranking[team2[n]]));
        theta_corr[n] = exp(rho);
      }
      //for (n in 1:N){
      //  parm_sliced[n,1]=theta_home[n];
      //  parm_sliced[n,2]=theta_away[n];
      //  parm_sliced[n,3]=theta_corr[n];
     // }
     
    }
    model{
      // log-priors for team-specific abilities
      profile("prior"){
      for (h in 1:(nteams)){
        if (prior_dist_num == 1 ){
          att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 2 ){
          att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
        else if (prior_dist_num == 3 ){
          att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
          def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
        }
      }

      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|0,5);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);
      }
      
        // likelihood
      profile("lik"){
      //int grainsize = 1;
      //target += reduce_sum(partial_sum,y,grainsize,theta_home,theta_away,theta_corr);
      //}
      for (n in 1:N){
        //target+=bipois_lpmf(y[n,]| theta_home[n],
        //                    theta_away[n], theta_corr[n]);
         target+=poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n]);
         target+=poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);

        }
      }
    }
    generated quantities{
      int y_rep[N,2];
      vector[N] log_lik;
      int diff_y_rep[N];
      int y_prev[N_prev,2];
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;
      vector[N_prev] theta_corr_prev;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n])+
                     poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);
       //bipois_lpmf(y[n,]| theta_home[n],
       //                          theta_away[n], theta_corr[n]);
      }
      for (n in 1:N_prev){
        theta_home_prev[n] = exp(home+att[instants_prev[n], team1_prev[n]]+
                                   def[instants_prev[n], team2_prev[n]]+
                         (gamma/2)*(ranking[team1_prev[n]]-ranking[team2_prev[n]]));
        theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                                   def[instants_prev[n], team1_prev[n]]-
                         (gamma/2)*(ranking[team1_prev[n]]-ranking[team2_prev[n]]));
        theta_corr_prev[n] = exp(rho);
        y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
        y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
      }
    }    
