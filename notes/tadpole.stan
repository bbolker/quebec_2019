    data {
       int<lower=1> N;
       int<lower=0> Initial[N];
       int<lower=0> Killed[N];
    }
parameters {
	real <lower=0>a;
	real <lower=0>h;
    }
    model {
       for (i in 1:N) {
          Killed[i] ~ binomial(Initial[i], a/(1+a*h*Initial[i]));
       }
}
