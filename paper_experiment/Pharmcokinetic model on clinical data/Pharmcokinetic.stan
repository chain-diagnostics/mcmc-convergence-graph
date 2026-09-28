
functions {
  real[] three_cpt_ode(real t, // the current time at which the ODE solver is evaluating the system.
                       real[] x, //the current state vector, meaning the current drug amounts in the three compartments at time t.
                       real[] ode_pars, //five inferred rates
                       data real[] x_r //two inputs: infusion rate, and infusion duration.
                       ) {
    real dxdt[3];

    real k10 = ode_pars[1];
    real k12 = ode_pars[2];
    real k21 = ode_pars[3];
    real k13 = ode_pars[4];
    real k31 = ode_pars[5];

    real infusion_rate = x_r[1];
    real infusion_duration = x_r[2];

    real Rin; //infusion into the centre

    if (t <= infusion_duration) {
      Rin = infusion_rate;
    } else {
      Rin = 0;
    }

    // x[1]: central amount
    // x[2]: peripheral amount 1
    // x[3]: peripheral amount 2
    dxdt[1] = Rin - (k10 + k12 + k13) * x[1] + k21 * x[2] + k31 * x[3];//
    dxdt[2] = k12 * x[1] - k21 * x[2];//VP1
    dxdt[3] = k13 * x[1] - k31 * x[3];//VP2

    return dxdt;
  }
}

data {
  int<lower=1> N_subj; //the number of participants
  int<lower=1> N_obs; //total number of concentration observations passed to Stan

  int<lower=1> n_obs[N_subj]; //how many observation subject s has
  int<lower=1> start[N_subj]; // where subjects begins

  real<lower=0> time_obs[N_obs]; //observation times, time in the data file
  real<lower=0> conc_obs[N_obs]; // observation concentrations (y)

  real<lower=0> rate[N_subj]; //infusion rate , rate in the datafile
  real<lower=0> tinf[N_subj]; // duration


  real<lower=0> x_r_ode[N_subj, 2]; // infusion rate and duration in the form of ode solver wants
}

parameters {
  real<lower=0> k10;  // elimination from central compartment
  real<lower=0> k12;  // central -> peripheral 1
  real<lower=0> k21;  // peripheral 1 -> central
  real<lower=0> k13;  // central -> peripheral 2
  real<lower=0> k31;  // peripheral 2 -> central
  real<lower=0> VC;   // central volume
  real<lower=0> sigma_add;
  real<lower=0> sigma_prop;
}

model {
  k10 ~ lognormal(log(0.3), 1);
  k12 ~ lognormal(log(0.3), 1);
  k13 ~ lognormal(log(0.3), 1);
  k21 ~ lognormal(log(0.3), 1);
  k31 ~ lognormal(log(0.3), 1);
  VC  ~ lognormal(log(5), 1);
  sigma_add  ~ normal(0, 1); // additive SD
  sigma_prop ~ normal(0, 1); // proportional SD

  {
    real x0[3];
    real ode_pars[5];
    int x_i[0];

    x0[1] = 0; //initial drug amounts in the three compartments are 0.
    x0[2] = 0;
    x0[3] = 0;

    ode_pars[1] = k10; //packges the five rate parameters into one array.
    ode_pars[2] = k12;
    ode_pars[3] = k21;
    ode_pars[4] = k13;
    ode_pars[5] = k31;

    for (s in 1:N_subj) { //outer loop
      int n_s = n_obs[s]; // how many concentration observation does subject s have.
      int st = start[s];// where does this subject's first ovservation begin in the long flattened arrays.
      real ts[n_obs[s]]; //create a temporary vector of observation times for subject s.
      real x_hat[n_obs[s], 3]; //creates a matrix-like object with one row per observation and three columns for the three states

      for (j in 1:n_s) { //inner loop fills the subject-specific time vector ts
        ts[j] = time_obs[st + j - 1];
      }

      x_hat = integrate_ode_bdf(//happens once per subject
        three_cpt_ode,
        x0,
        0.0,
        ts,
        ode_pars,
        x_r_ode[s],
        x_i
      );

      for (j in 1:n_s) {//inner loop:through all observations for that subject
        int idx = st + j - 1; //for each observation, finds the observation's position in the long global vectors
        real mu = x_hat[j, 1] / VC; //take the central-compartment amount for the j-th observation time
        real sigma = sigma_add + sigma_prop*mu;//proportional-additive model.

        conc_obs[idx] ~ normal(mu, sigma);
      }
    }
  }
}
