# library(devtools)
# install_github("LeoEgidi/footBayes")
# library(footBayes)

weekstan <- function(data,ranking,seasonteams,prior,prior_sd,matchpred){

  ## 数据必须包含赛季，主场球队，客场球队，主场进球，客场进球，并按所列顺序排列
  ## 数据必须包含每周的完整比赛


### 相关前置变量
#### 将时间转变为周数并标序
  colnames(data) <- c("season", "home", "away","homegoals", "awaygoals") # 更改数据的列名

  nteams<- length(unique(c(data$home,data$away))) # 队伍数量
  N <- dim(data)[1] # 比赛总场数
  weak_count <- (N*2)/(seasonteams)
  weak <- rep(seq(1, weak_count), each = (seasonteams*(1/2))) # 每周踢 队伍数/2 场比赛
  data <- data %>% 
    mutate(weak) #建立指示周数的列
  ntimes <- length(unique(weak)) #总周数
  time <- c(1:length(unique(weak))) #
  instants <- weak[1:N]
  N_prev=seasonteams/2


#### 其他数据向量化数量化
  teams <- unique(c(data$home,data$away)) # 建立队伍列表
  team_home <- match( data$home, teams) # 返回 data$home 中每个元素在 teams的位置，相当于返回每个队伍的序号 --- 等于stata 里的 group()
  team_away <- match( data$away, teams)
  team1 <- team_home[1:N] # 主场队伍按比赛排列的标号
  team2 <- team_away[1:N]
  y <- matrix(NA, N, 2) #建立空白矩阵，N行2列
  y[,1] <- as.numeric(as.vector(data$homegoals)[1:N]) # 第一列为所有主场进球
  y[,2] <- as.numeric(as.vector(data$awaygoals)[1:N]) # 第二列为所有客场进球
  diff_y <- y[,1]-y[,2] ## 模型暂时不用

#### 预测相关参数
  instants_prev <- weak[(N-N_prev+1):N] #只用最后一周的参数做预测  
  
  if (missing(matchpred)){ # 如果没有输入所要预测的队伍组合，则默认用最后一周的赛事组合
    team1_prev <- team_home[(N-N_prev+1):N]
    team2_prev <- team_away[(N-N_prev+1):N]
  }else{ # 如果有输入索要预测的队伍，则对预测队伍进行编号
    colnames(matchpred) <- c("home", "away")
    team1_prev <- match(matchpred$home, teams)
    team2_prev <- match(matchpred$away, teams)
  }

### 参数相关前置变量
  hyper_df <- 1           # initialization
  if (missing(prior)){    # Normal as default weakly-inf. prior
    prior_dist_num <- 1
    prior <- normal(0,NULL)
    hyper_location<- 0    # location
    #hyper_sd_scale <- 5  # scale
  }else{
    prior_dist <- prior$dist
    #good_prior_names <- c("normal", "student_t", "cauchy", "laplace")
    #prior_dist <- match.arg(prior_dist, good_prior_names)
    if (is.null(prior$scale)==FALSE){
      warning("Group-level standard deviations cannot be fixed to
               numerical values, rather they need to be assigned
               a reasonable prior distribution. Thus, the 'scale'
               argument in the 'prior' argument will be omitted
               (by default, prior$scale=NULL).")
    }
      if (prior_dist == "normal"){
        prior_dist_num <- 1
        hyper_df <- 1
        hyper_location <- prior$location
          # if (is.null(prior_sd$scale)){
          #   hyper_sd_scale <-1
          # }else{
          #   hyper_sd_scale <- prior_sd$scale
          # }
      }else if (prior_dist=="t" && prior$df!=1){
        prior_dist_num <- 2   # student-t
        hyper_df <- prior$df
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      }else if (prior_dist=="t"&& prior$df==1){
        prior_dist_num <- 3
        hyper_df <- 1     # by default of Cauchy distribution
        hyper_location <- prior$location
        # if (is.null(prior$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      } else if (prior_dist =="laplace"){
        prior_dist_num <- 4
        hyper_df <- 1
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
        }
    }


         hyper_sd_df <- 1        # initialization
      if (missing(prior_sd)){    # Cauchy as default weakly-inf. prior
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1        # student_t with 1 df
         hyper_sd_location<- 0   # location
         hyper_sd_scale <- 5     # scale
      }else{
        prior_dist_sd <- prior_sd$dist
       if (prior_dist_sd == "normal"){
         prior_dist_sd_num <- 1
         hyper_sd_df <- 1
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t" && prior_sd$df!=1){
         prior_dist_sd_num <- 2   # student-t
         hyper_sd_df <- prior_sd$df
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t"&& prior_sd$df==1){
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1     # by default of Cauchy distribution
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      } else if (prior_dist_sd =="laplace"){
        prior_dist_sd_num <- 4
        hyper_sd_df <- 1
        hyper_sd_location <- prior_sd$location
        if (is.null(prior_sd$scale)){
          hyper_sd_scale <-1
        }else{
          hyper_sd_scale <- prior_sd$scale
        }
      }
    }


### 模型暂时不用但以后可能会用到的参数
  if (missing(ranking)){
    ranking <- matrix(0, nteams,2)
  }else if (is.matrix(ranking)==FALSE & is.data.frame(ranking)== FALSE ){
    stop("Please, ranking must be a matrix or a data frame!")
  }else{
    colnames(ranking) <- c("rank_team", "points")
    team_order <- match(teams, ranking$rank_team)
    ranking[,1] <- ranking$rank_team[team_order]
    ranking[,2] <- ranking$points[team_order]
    ranking[,2] <- (as.numeric(as.vector(ranking[,2]))-mean(as.numeric(as.vector(ranking[,2]))))/(2*sd(as.numeric(as.vector(ranking[,2]))))
    }

### 检查数据
  if ((N*2)%%(seasonteams)!=0){
    stop("The number of total matches is not
              the same for all the teams. Please,
              provide an adequate number of matches
              (hint: proportional to the number
              of matches for each match day).")
    } 
    
  if ( !is.numeric(data$homegoals) |!is.numeric(data$awaygoals)){
    stop("Goals are not numeric! Please, provide numeric values for the goals")
    }



### 拟合相关默认参数
  user_dots <- list(chains = 4, iter = 2000,
                    warmup = 800,
                    thin = 1,
                    init = "random", seed = sample.int(.Machine$integer.max, 1),
                    algorithm = c("NUTS", "HMC", "Fixed_param"),
                    control = NULL, sample_file = NULL, diagnostic_file = NULL,
                    save_dso = TRUE, verbose = FALSE, include = TRUE,
                    cores = getOption("mc.cores", 1L),
                    open_progress = interactive() && !isatty(stdout()) &&
                      !identical(Sys.getenv("RSTUDIO"), "1"),
                    boost_lib = NULL, eigen_lib = NULL)





  # Stan data
  data_stan <- list( y=y,
                spi_std = rep(0, nteams),
                diff_y = diff_y,
                N=N,
                N_prev=N_prev,
                seasonteams=seasonteams,
                nteams=nteams,
                time=time,
                ntimes=ntimes,
                instants=instants,
                team1 = team1,
                team2=team2,
                team1_prev=team1_prev,
                team2_prev=team2_prev,
                instants_prev=instants_prev,
                prior_dist_num = prior_dist_num,
                prior_dist_sd_num = prior_dist_sd_num,
                hyper_df=hyper_df,
                hyper_location=hyper_location,
                hyper_sd_df=hyper_sd_df,
                hyper_sd_location=hyper_sd_location,
                hyper_sd_scale=hyper_sd_scale,
                ranking = ranking[,2])
  
  # Stan model
 
  #f<-write_stan_file(stan_model,dir = 'stan',basename = 'biv_poisson_football')
      
  #mod_cl<-cmdstan_model(stan_file = "biv_poisson_football_orig.stan",cpp_options = list(stan_threads = TRUE,stan_opencl = TRUE))
  mod_cl<-cmdstan_model(stan_file = "biv_poisson_football_orig.stan",cpp_options = list(stan_threads = TRUE,stan_opencl = TRUE,"PRECOMPILED_HEADERS"=FALSE,paste0("LDFLAGS_OPENCL= -L\"","C:/Program Files (x86)/OCL_SDK_Light/lib/x86_64","\" -lOpenCL")))
  
  #mod_cl<-cmdstan_model(stan_file = f,cpp_options = list(stan_threads = TRUE,stan_opencl = TRUE))
  
  #mod_cl<-cmdstan_model(stan_file = f,cpp_options = list(stan_threads = TRUE,stan_opencl = TRUE,"PRECOMPILED_HEADERS"=FALSE,paste0("LDFLAGS_OPENCL= -L\"","C:/Program Files (x86)/OCL_SDK_Light/lib/x86_64","\" -lOpenCL")))
  
  fit_sample <- mod_cl$sample(data= data_stan,
                       iter_sampling = user_dots$iter-user_dots$warmup,
                       chains = user_dots$chains,
                       thin = user_dots$thin,
                       parallel_chains = user_dots$cores,
                       threads_per_chain = 10,
                       iter_warmup = user_dots$warmup,
                       init =1, seed = user_dots$seed,
                       refresh=10,max_treedepth=2^11-1,
                       opencl_ids = c(0, 0)
                       )
  fit_quantity<- mod_cl$generate_quantities(fit_sample,data= data_stan,seed = user_dots$seed,parallel_chains = user_dots$cores,
                                            threads_per_chain = 10,opencl_ids = c(0, 0))
  return(fit_quantity)

}



seasonstan <- function(data,ranking,seasonteams,prior,prior_sd,matchpred){

  ## 数据必须包含赛季，主场球队，客场球队，主场进球，客场进球，并按所列顺序排列
  ## 数据必须包含每周的完整比赛


### 相关前置变量
#### 将时间转变为周数并标序
  colnames(data) <- c("season", "home", "away","homegoals", "awaygoals") # 更改数据的列名
  
  nteams<- length(unique(c(data$home,data$away))) # 队伍数量
  N <- dim(data)[1] # 比赛总场数
  season_count <- N/(seasonteams*(seasonteams-1))
  season <- rep(seq(1, season_count ), each = seasonteams*(seasonteams-1)) # 每季踢 seasonteams*(seasonteams-1) 场比赛
  data <- data %>% 
    mutate(season) #建立指示周数的列
  ntimes <- length(unique(season)) #总周数
  time <- c(1:length(unique(season))) #
  instants <- season[1:N]
  N_prev=N_prev=seasonteams/2


#### 其他数据向量化数量化
  teams <- unique(c(data$home,data$away)) # 建立队伍列表
  team_home <- match( data$home, teams) # 返回 data$home 中每个元素在 teams的位置，相当于返回每个队伍的序号 --- 等于stata 里的 group()
  team_away <- match( data$away, teams)
  team1 <- team_home[1:N] # 主场队伍按比赛排列的标号
  team2 <- team_away[1:N]
  y <- matrix(NA, N, 2) #建立空白矩阵，N行2列
  y[,1] <- as.numeric(as.vector(data$homegoals)[1:N]) # 第一列为所有主场进球
  y[,2] <- as.numeric(as.vector(data$awaygoals)[1:N]) # 第二列为所有客场进球
  diff_y <- y[,1]-y[,2] ## 模型暂时不用

#### 预测相关参数
  instants_prev <- season[(N-N_prev+1):N] #只用最后一周的参数做预测
  
  if (missing(matchpred)){ # 如果没有输入所要预测的队伍组合，则默认用最后一周的赛事组合
    team1_prev <- team_home[(N-N_prev+1):N]
    team2_prev <- team_away[(N-N_prev+1):N]
  }else{ # 如果有输入索要预测的队伍，则对预测队伍进行编号
    colnames(matchpred) <- c("home", "away")
    team1_prev <- match(matchpred$home, teams)
    team2_prev <- match(matchpred$away, teams)
  }  

### 参数相关前置变量
  hyper_df <- 1           # initialization
  if (missing(prior)){    # Normal as default weakly-inf. prior
    prior_dist_num <- 1
    prior <- normal(0,NULL)
    hyper_location<- 0    # location
    #hyper_sd_scale <- 5  # scale
  }else{
    prior_dist <- prior$dist
    #good_prior_names <- c("normal", "student_t", "cauchy", "laplace")
    #prior_dist <- match.arg(prior_dist, good_prior_names)
    if (is.null(prior$scale)==FALSE){
      warning("Group-level standard deviations cannot be fixed to
               numerical values, rather they need to be assigned
               a reasonable prior distribution. Thus, the 'scale'
               argument in the 'prior' argument will be omitted
               (by default, prior$scale=NULL).")
    }
      if (prior_dist == "normal"){
        prior_dist_num <- 1
        hyper_df <- 1
        hyper_location <- prior$location
          # if (is.null(prior_sd$scale)){
          #   hyper_sd_scale <-1
          # }else{
          #   hyper_sd_scale <- prior_sd$scale
          # }
      }else if (prior_dist=="t" && prior$df!=1){
        prior_dist_num <- 2   # student-t
        hyper_df <- prior$df
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      }else if (prior_dist=="t"&& prior$df==1){
        prior_dist_num <- 3
        hyper_df <- 1     # by default of Cauchy distribution
        hyper_location <- prior$location
        # if (is.null(prior$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
      } else if (prior_dist =="laplace"){
        prior_dist_num <- 4
        hyper_df <- 1
        hyper_location <- prior$location
        # if (is.null(prior_sd$scale)){
        #   hyper_sd_scale <-1
        # }else{
        #   hyper_sd_scale <- prior_sd$scale
        # }
        }
    }


         hyper_sd_df <- 1        # initialization
      if (missing(prior_sd)){    # Cauchy as default weakly-inf. prior
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1        # student_t with 1 df
         hyper_sd_location<- 0   # location
         hyper_sd_scale <- 5     # scale
      }else{
        prior_dist_sd <- prior_sd$dist
       if (prior_dist_sd == "normal"){
         prior_dist_sd_num <- 1
         hyper_sd_df <- 1
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t" && prior_sd$df!=1){
         prior_dist_sd_num <- 2   # student-t
         hyper_sd_df <- prior_sd$df
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      }else if (prior_dist_sd=="t"&& prior_sd$df==1){
         prior_dist_sd_num <- 3
         hyper_sd_df <- 1     # by default of Cauchy distribution
         hyper_sd_location <- prior_sd$location
         if (is.null(prior_sd$scale)){
           hyper_sd_scale <-1
         }else{
           hyper_sd_scale <- prior_sd$scale
         }
      } else if (prior_dist_sd =="laplace"){
        prior_dist_sd_num <- 4
        hyper_sd_df <- 1
        hyper_sd_location <- prior_sd$location
        if (is.null(prior_sd$scale)){
          hyper_sd_scale <-1
        }else{
          hyper_sd_scale <- prior_sd$scale
        }
      }
    }


### 模型暂时不用但以后可能会用到的参数
  if (missing(ranking)){
    ranking <- matrix(0, nteams,2)
  }else if (is.matrix(ranking)==FALSE & is.data.frame(ranking)== FALSE ){
    stop("Please, ranking must be a matrix or a data frame!")
  }else{
    colnames(ranking) <- c("rank_team", "points")
    team_order <- match(teams, ranking$rank_team)
    ranking[,1] <- ranking$rank_team[team_order]
    ranking[,2] <- ranking$points[team_order]
    ranking[,2] <- (as.numeric(as.vector(ranking[,2]))-mean(as.numeric(as.vector(ranking[,2]))))/(2*sd(as.numeric(as.vector(ranking[,2]))))
    }

### 检查数据
  if ((N*2)%%(seasonteams)!=0){
    stop("The number of total matches is not
              the same for all the teams. Please,
              provide an adequate number of matches
              (hint: proportional to the number
              of matches for each match day).")
    } 
    
  if ( !is.numeric(data$homegoals) |!is.numeric(data$awaygoals)){
    stop("Goals are not numeric! Please, provide numeric values for the goals")
    }
  

### 拟合相关默认参数
  user_dots <- list(chains = 4, iter = 5000,
                    #warmup = floor(iter/4),
                    thin = 1,
                    init = "random", seed = sample.int(.Machine$integer.max, 1),
                    algorithm = c("NUTS", "HMC", "Fixed_param"),
                    control = NULL, sample_file = NULL, diagnostic_file = NULL,
                    save_dso = TRUE, verbose = FALSE, include = TRUE,
                    cores = getOption("mc.cores", 1L),
                    open_progress = interactive() && !isatty(stdout()) &&
                      !identical(Sys.getenv("RSTUDIO"), "1"),
                    boost_lib = NULL, eigen_lib = NULL)


  # Stan data
  data_stan <- list( y=y,
                spi_std = rep(0, nteams),
                diff_y = diff_y,
                N=N,
                nteams=nteams,
                team1 = team1,
                team2=team2,
                prior_dist_num = prior_dist_num,
                prior_dist_sd_num = prior_dist_sd_num,
                hyper_df=hyper_df,
                hyper_location=hyper_location,
                hyper_sd_df=hyper_sd_df,
                hyper_sd_location=hyper_sd_location,
                hyper_sd_scale=hyper_sd_scale,
                ranking = ranking[,2])
  
  # Stan model
  stan_model <-       
    "functions{

      real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
        real ss;
        real log_s;
        real mus;
        int  miny;

        miny = min(r[1], r[2]);

        ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
          exp(mu3);
        if(miny > 0) {
          mus = -mu1-mu2+mu3;
          log_s = ss;

          for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
            + log(r[2] - k + 1)
            - log(k);
            ss = log_sum_exp(ss, log_s);
          }
        }
        return(ss);
      }
    }
    data{
      int N;   // number of games
      int N_prev;
      int y[N,2];
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
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real gamma;
    }
    transformed parameters{
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      cov_matrix[ntimes] Sigma_att;         // Gaussian process attack cov. funct.
      cov_matrix[ntimes] Sigma_def;        // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                 // exponentiated linear pred.
      vector[N] theta_away;
      vector[N] theta_corr;

      //Gaussian process covariance functions
      for (i in 1:(ntimes)){
           for (j in 1:(ntimes)){
               Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
               + (i == j ? 0.1 : 0.0);
               Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
                           + (i == j ? 0.1 : 0.0);
             }}

      // Sum-to-zero constraint for attack/defense parameters
      att[1]=att_raw[1]-mean(att_raw[1]);
      def[1]=def_raw[1]-mean(def_raw[1]);
      for (t in 2:ntimes){
        att[t]=att_raw[t]-mean(att_raw[t]);
        def[t]=def_raw[t]-mean(def_raw[t]);
      }

      // Lagged prior mean for attack/defense parameters
      for (t in 2:(ntimes)){
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location, nteams);
        mu_def[t]= def[t-1];
        //rep_row_vector(0,nteams);

      }


      for (n in 1:N){
        theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[team1[n]]-ranking[team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[team1[n]]-ranking[team2[n]]));
        theta_corr[n] = exp(rho);
      }
    }
    model{
      // log-priors for team-specific abilities
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

      // likelihood

      for (n in 1:N){
        //target+=bipois_lpmf(y[n,]| theta_home[n],
        //                    theta_away[n], theta_corr[n]);
          target+=poisson_lpmf(y[n,1]|theta_home[n]+theta_corr[n]);
          target+=poisson_lpmf(y[n,2]|theta_away[n]+theta_corr[n]);

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
    }"    
    

  fit <- stan(model_code = stan_model,
                       data= data_stan,
                       iter = user_dots$iter,
                       chains = user_dots$chains,
                       thin = user_dots$thin,
                       cores = user_dots$cores
                       )
  return(fit)

}


foot_prob <- function(object, data,filename, home_team, away_team, matchpred,cmdstanr = T){ # 预测概率

  colnames(data) <- c("season", "home", "away",
                      "homegoals", "awaygoals")
  teams <- unique(c(data$home,data$away))
  if (cmdstanr==T){
    sims <- as_draws_matrix(object$draws("y_prev"))
    predict <- dim(sims)[2]/2
    if (missing(matchpred)){ # 如果没有输入所要预测的队伍组合，则默认用最后一周的赛事组合
      #predict <- dim(sims$y_prev)[2]
      #predict <- dim(sims)[2]/2
      data_prev <- data[(dim(data)[1]-predict +1):(dim(data)[1]),]
    }else{ # 
      colnames(matchpred) <- c("home", "away")
      data_prev <- matchpred
    }
    if (missing(home_team) & missing(away_team)){
      home_team <- data_prev$home
      away_team <- data_prev$away    
    }
    
    find_match <- which(data_prev$home==home_team & data_prev$away == away_team )
    M <- dim(sims)[1]
    previsioni1<-sims[,1:predict]
    previsioni2<-sims[, (predict+1):(dim(sims)[2])]
    previsioni1<-matrix(previsioni1,nrow=dim(previsioni1)[1],ncol=dim(previsioni1)[2])
    previsioni2<-matrix(previsioni2,nrow=dim(previsioni2)[1],ncol=dim(previsioni2)[2]) # 转化为'matrix'对象
  }else{
    sims <- rstan::extract(object)
    if (missing(matchpred)){ # 如果没有输入所要预测的队伍组合，则默认用最后一周的赛事组合
      predict <- dim(sims$y_prev)[2]
      #predict <- dim(sims_)[2]/2
      data_prev <- data[(dim(data)[1]-predict +1):(dim(data)[1]),]
    }else{ # 
      colnames(matchpred) <- c("home", "away")
      data_prev <- matchpred
    }
    if (missing(home_team) & missing(away_team)){
      home_team <- data_prev$home
      away_team <- data_prev$away    
    }
    
    find_match <- which(data_prev$home==home_team & data_prev$away == away_team )
    M <- dim(sims$y_prev)[1]
    #true_gol_home <- data$homegoals[(dim(sims$y_rep)[2]+1):(dim(sims$y_rep)[2]+predict)]
    #true_gol_away <- data$awaygoals[(dim(sims$y_rep)[2]+1):(dim(sims$y_rep)[2]+predict)]
    #true_gol_home <- true_gol_home[match(home_team, data_prev$home)]
    #true_gol_away <- true_gol_away[match(away_team, data_prev$away)]
    
    previsioni1<-sims$y_prev[, find_match ,1]
    previsioni2<-sims$y_prev[, find_match,2]
  }
  
  data_exp_tot <- rep(1,5) # 建立一个值都为1，4列1行的列表

  for (i in 1:length(find_match)){ 
    posterior_prop1<-table(subset(previsioni1[,i], previsioni1[,i]<15))
    posterior_prop2<-table(subset(previsioni2[,i], previsioni2[,i]<15)) # 在find_match里面，所有可能比分的频次

    teamaa=home_team[i]
    teamab=away_team[i] # 主场客场队伍

    x_min=y_min=min(length(posterior_prop1),
                  length(posterior_prop2))

    counts_mix<-matrix(0, min(length(posterior_prop1), length(posterior_prop2)),  # 值为0，行列维度和不同得分的可能数量匹配
                     min(length(posterior_prop1), length(posterior_prop2)))

    for (j in 1: min(length(posterior_prop1), length(posterior_prop2))){ # 填充counts_mix矩阵
      for (t in 1: min(length(posterior_prop1), length(posterior_prop2))){
        counts_mix[j,t]<-posterior_prop1[j]*posterior_prop2[t] #纵轴为主队进球数，则每列代表主队进球变化时的组合
      }}
    dim1 <- dim(counts_mix)[1]
    dim2 <- dim(counts_mix)[2] # 矩阵维度

    x <- seq(0,dim1-1, length.out=dim1)
    y <- seq(0,dim2-1, length.out=dim2) # 0，边界为0到最大得分
    data_exp <- expand.grid(Home=x, Away=y) # 所有比分组合的panel, 主队变化，客队不变
    data_exp$Prob <- as.double(counts_mix/(M*M))
    data_exp$home_team <- teamaa
    data_exp$away_team <- teamab   
  #data_exp$true_gol_home <- true_gol_home[i]
  #data_exp$true_gol_away <- true_gol_away[i]

  # overall "adjusted" probabilities
    #prob_h[i] <- sum(counts_mix[lower.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)
    #prob_d[i] <- sum(diag(counts_mix/(M*M)))/sum(data_exp$Prob)
    #prob_a[i] <- sum(counts_mix[upper.tri(counts_mix)]/(M*M))/sum(data_exp$Prob)

  # MLO (most likely outcome)

    #row_pos[i] <- row(counts_mix)[counts_mix==max(counts_mix)]
    #col_pos[i] <- col(counts_mix)[counts_mix==max(counts_mix)]


    #mlo[i] <- paste(row_pos[i]-1, "-", col_pos[i]-1, " (",
        #round(max(counts_mix/(M*M)),4), ")" , sep="")

    data_exp_tot <- rbind(data_exp_tot, data_exp) # 按列数合并数据
    #write.csv(data_exp,"matchprob.csv",row.names = FALSE)
    }
    data_exp_tot <- data_exp_tot[-c(1), ] # 不包含第一列 （因为都为1）



    #tbl <- data.frame(home_team = home_team,
                    #away_team = away_team,
                    #prob_h = round(prob_h,4),
                    #prob_d = round(prob_d,4),
                    #prob_a = round(prob_a,4),
                    #mlo = mlo)
  if (missing(filename)){
    return(data_exp_tot)
  }    
  else{  
    write.csv(data_exp_tot,filename,row.names = FALSE)
    return(data_exp_tot)
  }  
    
  }