function MP_shock_plot = compute_MP_shock_plot(irfinfo,T,vars)

%This function returns the IRFs of length T to MP shock for vars of interest using matrix irfinfo containing IRFs

%It returns a matrix MP_shock_plot with size equal to T x num vars x 1

num_vars = numel(vars);

MP_shock_plot = NaN(T,num_vars,1);

for nvars = 1:num_vars
    
   aux = eval(['irfinfo.',vars{nvars},'_eps_nu;']);
   
   %Trim length in case not equal (and ensure response is to a monetary stimulus)
   MP_shock_plot(:,nvars,1) = -aux(1:T);
end
end