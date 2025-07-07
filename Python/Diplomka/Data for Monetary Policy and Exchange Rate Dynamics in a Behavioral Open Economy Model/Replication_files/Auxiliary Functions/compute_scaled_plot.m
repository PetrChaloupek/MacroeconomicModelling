function scaled_plot = compute_scaled_plot(irfinfo,T,vars)

%This function returns the scaled plot of variables in vars for the horizon T from matrix irfinfo containing their IRFs

num_vars = numel(vars);

scaled_plot = zeros(num_vars,T+1);

for i = 1:T+1
    for j = 1:num_vars
        scaled_plot(j,i) = eval(['irfinfo.',vars{j},'_errshk',num2str(i-1),'(1)/irfinfo.',vars{j},'_errshk0(1);']);
        %Note: if a variable doesn't respond this may result in 0/0 giving an NaN
        %      this will mean that the line isn't plotted, which is just as well, as the relative response will be ill defined
    end
end
end