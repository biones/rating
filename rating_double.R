library(rstan)

scode="
data{
  int N;
  int M;
  int x[N,2];
  int y[N,2];
  int one[N];
}

parameters {
  real rate[M];
  real rate_two[M,M];
}

transformed parameters {
    real p[N];
    real tmp1;
    real tmp2;
    real b=0.00575364;
    for(i in 1:N){
      tmp1=rate[x[i,1]]+rate[x[i,2]]+rate_two[x[i,1],x[i,2]];
      tmp2=rate[y[i,1]]+rate[y[i,2]]+rate_two[y[i,1],y[i,2]];
      //tmp1=rate_two[x[i,1],x[i,2]];
      //tmp2=rate_two[y[i,1],y[i,2]];
      //tmp1=rate[x[i,1]];
      //tmp2=rate[y[i,1]];
      p[i]=inv_logit(b*(tmp1-tmp2));
      //print(rate_two[1,1])
      print(p[i])
    }
}

model{
  
  rate~normal(1500,500);
  //rate~normal(0,0.0001);
  //b=0.00575364;
  for(i in 1:M){
    for(j in 1:M){
      if(i<j){
        rate_two[i,j]~normal(0,500);
      }else{
        //rate_two[i,j]~normal(1500,500);
        rate_two[i,j]~normal(0,500);
      }
    }
  }
      
  //rate_two~normal(0,100);
  //one~berounulli(p);
  
  for(i in 1:N){
    //tmp1=rate[x[i,1]]+rate[x[i,2]]+rate_two[x[i,1],x[i,2]];
    //tmp2=rate[y[i,1]]+rate[y[i,2]]+rate_two[y[i,1],y[i,2]];
    //print(tmp1);
    //print(tmp2);
    //print(tmp1-tmp2);
  //  print(b*(tmp1-tmp2))
    //print(inv_logit(b*(tmp1-tmp2)))
    //target += bernoulli_lpmf( one[i] | inv_logit(b*(tmp1-tmp2)));
    one[i]~bernoulli(inv_logit(p[i]));
  }
  
}

"

#b=Solve[1/(1 + Exp[-x*100]) == 0.64, x]

d=c(1,2,3,4,
    1,4,2,3,
    2,3,4,5,
    2,3,1,5,
    1,3,2,5,
    2,3,1,4,
    1,5,2,4
    )

d2=c(1,2,3,4,
    1,4,2,3,
    2,3,4,5,
    2,3,1,5,
    1,3,2,5,
    1,2,4,5,
    1,3,4,5
)

#12が強いが14だと弱くなるペア
d3=c(1,2,3,4,
     1,2,3,4,
     1,3,2,4,
     1,3,2,5,
     1,2,3,5,
     1,2,3,4,
     2,3,4,5,
     2,3,1,4
     )
#あえてシングル
dsingle=c(1,1,2,2,
          1,1,3,3,
          2,2,3,3,
          2,2,4,4,
          3,3,4,4,
          1,1,2,2,
          1,1,3,3,
          2,2,3,3,
          2,2,4,4,
          3,3,4,4
          )

df=matrix(d3,ncol=4,byrow = T)

x=df[,1:2]
y=df[,3:4]
M=max(df)
#rtinit=matrix(1500,M,M)

lst=list(N=nrow(x),x=x,y=y,M=max(df),one=rep(1,nrow(x)))
fit=stan(model_code = scode,data=lst,
         #init = function(){list(rate_two=rtinit,rate=rep(1500,M))},
         chain=1,iter=500,warmup = 80)

la=extract(fit)

rate=apply(la$rate,2,mean)
M=max(df)
rate_two=matrix(0,M,M)
for(i in 1:M){
  for(j in 1:M){
    rate_two[i,j]=mean(la$rate_two[,i,j])
  }
}
print(rate_two)
#rate_two=apply(la$rate_two,2,mean)

print(rate)
#[1] 2463.690  403.316 1254.647 1795.259



