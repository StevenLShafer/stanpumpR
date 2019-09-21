# for RxODE solution
RxODEModel <- RxODE(
  model = 
    "
   d/dt(a1) =  k21 * a2 + k31 * a3 - k * a1; 
   d/dt(a2) =  k12 * a1 - k21 * a2;
   d/dt(a3) =  k13 * a1 - k31 * a3;
   "
)
