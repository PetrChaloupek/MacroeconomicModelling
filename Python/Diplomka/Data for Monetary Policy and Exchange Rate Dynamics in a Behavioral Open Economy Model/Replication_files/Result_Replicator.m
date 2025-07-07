clearvars; close all; clc;

%This file replicates the post-estimation results reported in the paper
%"Monetary Policy and Exchange Rate Dynamics in Behavioral Open Economy Model"
%by Marcin Kolasa, Sahil Ravgotra and Pawel Zabczyk
%published in the "Journal of International Economics"



%Name of thes file with estimated parameters
estparam_fname_rational = 'JIE_EST_Rational_mean.mat';
estparam_fname_baseline = 'JIE_EST_Baseline_mean.mat';
estparam_fname_inflated = 'JIE_EST_Inflated_mean.mat';

%Set gen_all to 1 to generate all figures and tables overwriting specific settings below
gen_all = 1;

%Set to one to generate corresonding figure or table
%Figure numbers:  #  1 2 3 4 5
gen_figures       = [0 0 0 0 0];
%----------------------------------Figure List----------------------------------------------%
%Figure 1: Engel regression coefficients (analytical)
%Note:     Replicating the data underlying Figure 1 can take around 20 minutes depending on PC speed
%Figure 2: Effects of forward guidance: Real Exchange Rate 
%Figure 3: Comparison between complete and incomplete markets
%Figure 4: Effects of forward guidance: Closed vs Open Economy [Inflation and Output]
%Figure 5: Determinacy and indeterminacy analysis
%Note:     Replicating the data underlying Figure 5 can take around 10 minutes depending on PC speed
%-------------------------------------------------------------------------------------------%

%Table numbers: #  1
gen_tables      = [0];
%----------------------------------Table  List----------------------------------------------%
%Table  1: Fama regression coefficients
%-------------------------------------------------------------------------------------------%

%Overwrite settings above if requested
if gen_all
    gen_figures = ones(size(gen_figures));
    gen_tables  = ones(size(gen_tables));
end

%Set path to current directory
MainPath = [pwd, filesep];

%Add folder with auxiliary functions to path
addpath([MainPath,'Auxiliary Functions'])

%Export figures to file (set to 1 to export)
export_res = 1;

%Graphics export format (easiest options are 'pdf' and 'png', the latter less trimmed)
export_fmt = 'pdf';

%Directory clean-up request (tidy up the directory after all results are generated
clean_dir = 1;  % Set to zero to disable

% Line styles
%st = {'-o', '-s', '-^', '-d'};
st = {'-ob', '-sr', '-^g', '-+m', '-dc'};

%%
%%---------------------------------------------------------------------------%%
% Figure 1: Engel regression coefficients (analytical)                        %
%%---------------------------------------------------------------------------%%

if gen_figures(1)

%Choose model 
model = 'SOE_BNK';

%------------------------------------------------------%
% A. Compute Exact Engel Regression Coefficients       %
%------------------------------------------------------%

%Define default switches to use for parameterization
%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) Standard Taylor rule / monetary policy shocks
%v)   Domestic monetary policy shocks only
default_switches = [1,1,1,1,0];

%For Engel coefficients we need larger j values (this creates a large model / slows execution)
j = 500;

%Define variables of interest
vars_base = {'y','pi','i','q','e','de','rho'};
vars_aux{j+1} = [];
for n = 0:j
    vars_aux{n+1} = ['id',num2str(n)];
end
vars = union(vars_base,vars_aux);

%Preallocate space for results
Engel_coeffs = NaN(5,j);

%1. Rational model
params = estimated_parameterization(default_switches,estparam_fname_rational); params.m = 1;
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars;
%Compute Engel coefficients (using exact formulae derived in paper)
for counter2 = 1:j
   Engel_coeffs(1,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
end

%2. Benchark behavioral model
params = estimated_parameterization(default_switches,estparam_fname_baseline);
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars;
%Compute Engel coefficients (using exact formulae derived in paper)
for counter2 = 1:j
   Engel_coeffs(2,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
end

%3. Inflated priors
params = estimated_parameterization(default_switches,estparam_fname_inflated);
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars;
%Compute Engel coefficients (using exact formulae derived in paper)
for counter2 = 1:j
   Engel_coeffs(3,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
end

%4. Low interest rate inertia
params = estimated_parameterization(default_switches,estparam_fname_baseline); params.rho_i = 0.75;
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars;
%Compute Engel coefficients (using exact formulae derived in paper)
for counter2 = 1:j
   Engel_coeffs(4,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
end

%5. High interest rate inertia
params = estimated_parameterization(default_switches,estparam_fname_baseline); params.rho_i = 0.95;
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars;
%Compute Engel coefficients (using exact formulae derived in paper)
for counter2 = 1:j
   Engel_coeffs(5,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
end

save Engel500 Engel_coeffs

%------------------------------------------------------%
% B. Plot exact Engel coefficients                     %
%------------------------------------------------------%

load Engel500 Engel_coeffs

% Engel coefficients
f = figure('Name',['Analytical Engel Coefficients'],'Position',[50 50 700 700]); %f.WindowState = 'maximized';
export_name = 'Figure_1_1';
N = 50;

%Engel coeffs: rational, baseline, inflated priors
subplot(2,1,1),
for counter=1:3
    plot(1:N,Engel_coeffs(counter,1:N),st{counter},'LineWidth',2,'MarkerIndices',1:4:N); hold on, grid on
end           
plot(1:N,zeros(1,N),'-k','LineWidth',1); 
lgd = legend('Rational','Baseline','Inflated Priors');

%Engel coeffs: interest rate smoothing
subplot(2,1,2),
for counter=[2 4 5]
    plot(1:N,Engel_coeffs(counter,1:N),st{counter},'LineWidth',2,'MarkerIndices',1:4:N); hold on, grid on
end           
plot(1:N,zeros(1,N),'-k','LineWidth',1); 
lgd = legend('Baseline','Baseline, low \rho','Baseline, high \rho');

%Saving the figure
if export_res
    save_figure_1;
end


% 2. Cumulative Engel coefficients
f = figure('Name',['Analytical Engel Coefficients'],'Position',[50 50 700 700]); %f.WindowState = 'maximized';
export_name = 'Figure_1_2';
N = 500;

%Cum. Engel coeffs: rational, baseline, inflated priors
subplot(2,1,1),
for counter=1:3
    plot(1:N,cumsum(Engel_coeffs(counter,1:N)),st{counter},'LineWidth',2,'MarkerIndices',1:40:N); hold on, grid on
end           
plot(1:N,zeros(1,N),'-k','LineWidth',1); 
lgd = legend('Rational','Baseline','Inflated Priors');

%Cum. Engel coeffs: interest rate smoothing
subplot(2,1,2),
for counter=[2 4 5]
    plot(1:N,cumsum(Engel_coeffs(counter,1:N)),st{counter},'LineWidth',2,'MarkerIndices',1:40:N); hold on, grid on
end           
plot(1:N,zeros(1,N),'-k','LineWidth',1); 
lgd = legend('Baseline','Baseline, low \rho','Baseline, high \rho');

%Saving the figure
if export_res
    save_figure_1;
end

%Tidy up
clear_redundant_variables;
end

%%
%%---------------------------------------------------------------------------%%
% Figure 2: Effects of Forward Guidance: RER          %
%%---------------------------------------------------------------------------%%

if gen_figures(2)

%Choose model 
model = 'SOE_BNK';

%Define T (the final horizon)
T = 40;

%Define variables of interest
vars = {'y','pi','q'};

%Convert variables to a string
var_list = conv2string(vars);

%Preallocate space for scaled IRFs
output_premium    = zeros(3,numel(vars),T+1);

%------------------------------------------------------%
% A. Run loop computing results                        %
%------------------------------------------------------%

%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) News shocks to real interest rates,
%v)   Domestic monetary policy shocks only
default_switches = [1,1,1,0,0];

%1. Rational model 
params = estimated_parameterization(default_switches,estparam_fname_rational); params.m = 0.99999; params.m_star=params.m;
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_premium(1,:,:) = compute_scaled_plot(irfinfo,T,vars);

%2. Baseline model 
params = estimated_parameterization(default_switches,estparam_fname_baseline); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_premium(2,:,:) = compute_scaled_plot(irfinfo,T,vars);

%2. Inflated priors model 
params = estimated_parameterization(default_switches,estparam_fname_inflated); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_premium(3,:,:) = compute_scaled_plot(irfinfo,T,vars);


%------------------------------------------------------%
% B. Plot results                                      %
%------------------------------------------------------%

f = figure('Name',['Effects of Forward Guidance on RER']); %f.WindowState = 'maximized';

y_lim = {[0 1],[0 16],[0 1]};

for counter = 1:3
    i=3;
    plot(0:T,squeeze(output_premium(counter,i,:)),st{counter},'LineWidth',2), 
    ylabel('Real Exchange Rate','FontSize',14), grid on, xlim([0 T]), hold on, ylim(y_lim{i})
end
lgd = legend('Rational','Baseline','Inflated Priors'); lgd.FontSize = 14;


%Export results to file if requested
if export_res
    export_name = 'Figure_2';
    save_figure_2a;
end

%Tidy up
clear_redundant_variables;
end



%%
%%---------------------------------------------------------------------------%%
% Figure 3: Engel regression - complete vs incomplete markets                %
%%---------------------------------------------------------------------------%%

if gen_figures(3)

%------------------------------------------------------%
% A. Compute Exact Engel Regression Coefficients       %
%------------------------------------------------------%

%Define default switches to use for parameterization
%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) Standard Taylor rule / monetary policy shocks
%v)   Domestic monetary policy shocks only
default_switches = [1,1,1,1,0];

%For Engel coefficients we need larger j values (this creates a large model / slows execution)
j = 100;

%Define variables of interest
vars_base = {'y','pi','i','q','e','de','rho'};

%Preallocate for auxiliary variables
vars_aux{j+1} = [];

%Generate variable names
for n = 0:j
    vars_aux{n+1} = ['id',num2str(n)];
end

%Compute list of all variables
vars = union(vars_base,vars_aux);

%Preallocate space for results
Engel_coeffs_exact = NaN(2,j);

%Run simulations for incomplete (baseline) and complete markets models
for counter = 1:2
    
    if counter == 1; model = 'SOE_BNK'; end
    if counter == 2; model = 'SOE_BNK_comp'; end
    
    %Ensure we have the default parameterization
    params = estimated_parameterization(default_switches,estparam_fname_baseline);
    
    %Compute model solution 
    eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
    %Find numbers of variables of interest
    find_vars;
    %Compute Engel coefficients (using exact formulae derived in paper)
    for counter2 = 1:j
       Engel_coeffs_exact(counter,counter2) = -oo_.var(where_rho,where_id(counter2+1))/oo_.var(where_id(counter2+1),where_id(counter2+1));
    end

end

save Engel100mkts Engel_coeffs_exact


%------------------------------------------------------%
% B. Compute RER IRF                                   %
%------------------------------------------------------%

%This focuses on contemporaneous easing only
LFL_vect = [0];
%Define T (the final horizon)
T = j;
%Define variables of interest
vars = {'q'};
%Convert variables to a string
var_list = conv2string(vars);
%Preallocate space for IRFs
output_open   = zeros(2,T,numel(vars));

%------------------------------------------------------%
% Run loop computing results                           %
%------------------------------------------------------%

for counter = 1:2

if counter == 1; model = 'SOE_BNK'; end
if counter == 2; model = 'SOE_BNK_comp'; end
    
%1. Open economy
    
%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) Shocks to the nominal interest rate,
%v)   Domestic monetary policy shocks only 
default_switches = [1,1,1,1,0];
    
%Obtain corresponding parameterization
params = estimated_parameterization(default_switches,estparam_fname_baseline);
    
%Run dynare reporting IRFs for variables of interest only
eval(['dynare ',model,' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Save IRFs to monetary policy shock
output_open(counter,:,:)= compute_MP_shock_plot(irfinfo,T,vars);   
    
end


%------------------------------------------------------%
% Plot exact Engel coefficients and RER IRF            %
%------------------------------------------------------%

tit   = {'RER Response to Monetary Easing','Engel Coefficients'};

N = 50;

f = figure('Name',['Engel Coefficients - complete vs incomplete mkts.'],'Position',[50 50 700 700]); %f.WindowState = 'maximized';

%Engel Coefficients
panel = 2;
subplot(1,2,panel),
    for counter = 1:2
        title(tit{panel},'FontSize',14), set(gca,'fontsize',12),
        plot(1:N,Engel_coeffs_exact(counter,1:N),st{counter},'LineWidth',2,'MarkerIndices',1:4:N), 
        grid on, hold on
    end
    %Add zero line and fix x and y axis limits
    plot(1:N,zeros(1,N),'-k','LineWidth',1); xlim([1,N]); 
    lgd = legend('Incomplete markets','Complete markets'); lgd.FontSize = 14;

%RER IRF
panel = 1;
subplot(1,2,panel),
    for counter = 1:2
        title(tit{panel},'FontSize',14), set(gca,'fontsize',12),
        plot(1:N,output_open(counter,1:N),st{counter},'LineWidth',2,'MarkerIndices',1:4:N), 
        grid on, hold on
    end
    %Add zero line and fix x and y axis limits
    plot(1:N,zeros(1,N),'-k','LineWidth',1); xlim([1,N]);     
    
%Export results to file if requested
export_name = 'Figure_8';
if export_res
    save_figure_2;
end

%Tidy up
clear_redundant_variables;

end


%%
%%---------------------------------------------------------------------------%%
% Figure 4: Effects of Forward Guidance (Closed vs Open Economy)              %
%%---------------------------------------------------------------------------%%

if gen_figures(4)

%Choose model 
model = 'SOE_BNK';
    
%Define T (the final horizon)
T = 20;

%Define variables of interest
vars = {'y','pi','q'};

%Convert variables to a string
var_list = conv2string(vars);

%Preallocate space for scaled IRFs
output_open   = zeros(3,numel(vars),T+1);
output_closed = zeros(3,numel(vars),T+1);

%------------------------------------------------------%
% A. Run loop computing results                        %
%------------------------------------------------------%

%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) News shocks to real interest rates,
%v)   Domestic monetary policy shocks only
default_switches = [1,1,1,0,0];
switch_closed =  [1,1,0,0,0]; %As above, but the closed economy specification

%1. Rational model 
%***Open economy specification
default_switches = [1,1,1,0,0];
params = estimated_parameterization(default_switches,estparam_fname_rational); params.m = 0.99999; params.m_star=params.m;
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_open(1,:,:) = compute_scaled_plot(irfinfo,T,vars);
%***Now compute the same for the closed economy specification
params = estimated_parameterization(switch_closed,estparam_fname_rational); params.m = 0.99999; params.m_star=params.m;
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
output_closed(1,:,:)= compute_scaled_plot(irfinfo,T,vars);

%2. Baseline behavioral model 
%***Open economy specification
default_switches = [1,1,1,0,0];
params = estimated_parameterization(default_switches,estparam_fname_baseline); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_open(2,:,:) = compute_scaled_plot(irfinfo,T,vars);
%***Now compute the same for the closed economy specification
params = estimated_parameterization(switch_closed,estparam_fname_baseline); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
output_closed(2,:,:)= compute_scaled_plot(irfinfo,T,vars);

%3. Behavioral model with inflated priors
%***Open economy specification
default_switches = [1,1,1,0,0];
params = estimated_parameterization(default_switches,estparam_fname_inflated); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
%Use the irfinfo matrix to compute scaled IRFs of variables of interest
output_open(3,:,:) = compute_scaled_plot(irfinfo,T,vars);
%***Now compute the same for the closed economy specification
params = estimated_parameterization(switch_closed,estparam_fname_inflated); 
%Run dynare with the extra shocks required and reporting IRFs for variables of interest only
eval(['dynare ',model,' -DT=',num2str(T+1),' ''-DOptions="order=1, irf=',num2str(T+1),', noprint, nograph"'' ''-DVar_List="',var_list,'"'';']);
output_closed(3,:,:)= compute_scaled_plot(irfinfo,T,vars);


%------------------------------------------------------%
% B. Plot results                                      %
%------------------------------------------------------%

f = figure('Name',['Effects of Forward Guidance (Closed vs Open Economy)']); f.WindowState = 'maximized';

tit   = {'Open','Closed'};
fig_labels = {'Output', 'Inflation', 'Nom. Int. Rate', 'Real Ex. Rate', 'Nominal Exchange Rate'};
y_lim = {[0 1],[0 16],[0 1]};

for counter = 1:3
for j = 1:2
    for i = 2:(-1):1
        subplot(2,2,(j-1)+(i-1)*2+1),
        if j == 1
            plot(0:T,squeeze(output_open(counter,i,:)),st{counter},'LineWidth',2), set(gca,'fontsize',12), 
        else
            plot(0:T,squeeze(output_closed(counter,i,:)),st{counter},'LineWidth',2), set(gca,'fontsize',12),
        end
        ylabel(fig_labels{i},'FontSize',14), grid on, xlim([0 T]), hold on
        if i == 1
            title(tit{j},'FontSize',14), ylim(y_lim{i}),
        end
    end    
end
end
lgd = legend('Rational','Baseline','Inflated Priors'); lgd.FontSize = 14;

%Export results to file if requested
if export_res
    export_name = 'Figure_3';
    save_figure;
end

%Tidy up
clear_redundant_variables;
end


%%
%%---------------------------------------------------------------------------%%
% Figure 5: Determinacy and indeterminacy analysis                            %
%%---------------------------------------------------------------------------%%

if gen_figures(5)

%Choose model 
model = 'SOE_BNK';
    
%Overwrite m so that we use an exact value of m=1
m_vect = [0.57, 0.69, 0.95, 1];
    
%Define grids for phi_pi and delta
phi_pi_min = 0; phi_pi_max = 2;    step_phi_pi = 0.01;
delta_min = 0;  delta_max = 0.005;  step_delta = 0.00001;

phi_pi_vals     = phi_pi_min:step_phi_pi:phi_pi_max;
delta_vals      = delta_min:step_delta:delta_max;

%Preallocate space for determinacy results
info_matrix     = NaN(numel(m_vect),numel(phi_pi_vals),numel(delta_vals));

%------------------------------------------------------%
% A. Run loop computing results                        %
%------------------------------------------------------%

for acc = 1e-12

for counter = 1:numel(m_vect)

%Standard switches
%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) Shocks to the real interest rate (also keeps real interest rate constant domestically)
%v)   All shocks enabled (cannot disable foreign MP shock here)
default_switches = [1,1,1,0,1];

%Auxiliary switches
%i)  Set to 1 to have identical values of m home and abroad (0 to have RE abroad)
%ii) Set to 1 to change values of both Taylor rule coefficients (0 to have foreign Taylor rule coefficients of 1.5 and 0.125)
aux_switches = [0,0];

%Obtain corresponding parameterization
params = estimated_parameterization(default_switches,estparam_fname_baseline);
%Normalize by setting phi_y to zero
params.phi_y      = 0;
%Update m and M_f (and their foreign equivalents)
update_m_mstar;

%We will be using RE abroad
if ~aux_switches(1)
    params.m_star   = 1; %RE abroad
end

%Initialize dynare matrices by running dynare once per new m
eval(['dynare ',model]);

%Now run loop over phi_pi and delta
for counter_1 = 1:numel(phi_pi_vals)
    disp(counter_1/numel(phi_pi_vals))
    for counter_2 = 1:numel(delta_vals)
        set_param_value('phi_pi',       phi_pi_vals(counter_1));
        set_param_value('phi_pi_star',  phi_pi_vals(counter_1));
        set_param_value('delta',        delta_vals(counter_2));
        
        if ~aux_switches(2)
            %Reset foreign Taylor rule parameters back to default
            set_param_value('phi_pi_star',  1.5);
            set_param_value('phi_y_star',   0.125);
        end
        
        [eigv,stab,info_out] = check(M_, options_, oo_);
       
        info_matrix(counter,counter_1,counter_2) = info_out(1);
        
        %If stable solution, check random walk
        if stab
            eigv=abs(sort(abs(eigv))); 
            if sum(eigv > 1 - acc) > 10 %9 f-l variables + ER definition
                info_matrix(counter,counter_1,counter_2) = -2;
            end
        end

    end
end
end
    
%Move to a matrix of -2, -1, 0, 1 only (with indeterminacy negative, and explosiveness positive)
%0 will correspond to a determinate solution
info_matrix(info_matrix==3) =  1; %Blanchard & Kahn conditions are not satisfied: no stable equilibrium.
info_matrix(info_matrix==4) = -1; %Blanchard & Kahn conditions are not satisfied: indeterminacy.
info_matrix(info_matrix==5) = -1; %Blanchard & Kahn conditions are not satisfied: indeterminacy due to rank failure

save Determinacy_Results info_matrix;

%------------------------------------------------------%
% B. Plot results                                      %
%------------------------------------------------------%


switch 10*aux_switches(1)+aux_switches(2)
    case 0
        fname = 'RE and Standard Taylor Rule in Foreign Economy';
        export_name = ['Figure_0'];
    case 1
        fname = 'RE Abroad and Symmetrical Taylor Rules';
        export_name = ['Figure_7_b_',num2str(acc)];
    case 10
        fname = 'Behavioral Discounting and Standard Taylor Rule in Foreign economy';
        export_name = ['Figure_7_c_',num2str(acc)];
    case 11
        fname = 'Behavioral Discounting and Symmetrical Taylor Rules';
        export_name = ['Figure_7_d_',num2str(acc)];
    otherwise
        error('Something is wrong with your switches');
end

%Create meshgrid
[X,Y]       = meshgrid(100*delta_vals,phi_pi_vals);

f = figure('Name',fname); f.WindowState = 'maximized';

modn{1} = ' (Inflated Priors)'; modn{2} = ' (Baseline)'; modn{3} = []; modn{4} = ' (Rational)';
%sgtitle(fname);
for counter = 1:numel(m_vect)
    subplot(2,2,counter),
    Z=squeeze(info_matrix(counter,:,:));
    X=X(:); Y=Y(:); Z=Z(:);
    scatter3(X(Z==-1),Y(Z==-1),Z(Z==-1),10,'y','filled'); view ([0 0 90]); hold on;
    scatter3(X(Z== 0),Y(Z== 0),Z(Z== 0),10,'b','filled'); view ([0 0 90]); hold on;
    scatter3(X(Z== 1),Y(Z== 1),Z(Z== 1),10,[.5 0.5 .5],'filled'); view ([0 0 90]); hold on;
    scatter3(X(Z==-2),Y(Z==-2),Z(Z==-2)+2,10,'r','filled'); view ([0 0 90]); 
    zlim([-1,1]);
    xlabel('100\phi'); ylabel('\phi_{\pi}'); title(['m = ',num2str(m_vect(counter),2),modn{counter}]);
end

leg_labels ={'Indeterminate','Determinate','Explosive','Random Walk'};
lgd = legend(leg_labels,'Orientation', 'horizontal','Box', 'on', 'Location',[0.47 0.04 0.1 0.0]); lgd.FontSize = 14;


%Export results to file if requested
if export_res
    save_figure;
end
end

%Tidy up
clear_redundant_variables;
end



%%
%%---------------------------------------------------------------------------%%
% Table 1:  Fama regression coefficients                                      %
%%---------------------------------------------------------------------------%%

if gen_tables(1)

%------------------------------------------------------%
% A. Compute Exact Fama Regression Coefficients        %
%------------------------------------------------------%

%Choose model 
model = 'SOE_BNK';

%Define default switches to use for parameterization
%i)   Full IS,                  ii) Discounting pi in real rate,
%iii) Open economy,             iv) Shocks to nominal interest rates (Taylor rule)
%v)   Domestic monetary policy shocks only
default_switches = [1,1,1,1,0];

%List of variables
vars_base = {'y','pi','i','q','e','de','rho'};
j = 1; %For Fama we do not need large j values (we'll only require id0 and id1)
vars_aux{j+1} = [];
for n = 0:j
    vars_aux{n+1} = ['id',num2str(n)];
end
vars = union(vars_base,vars_aux);

%Compute Fama coefficient for selected models
%Rational model
params = estimated_parameterization(default_switches,estparam_fname_rational); params.m = 1; 
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars; par_m(1) = params.m; par_rho_i(1) = params.rho_i;
Fama_coeffs(1) = oo_.var(where_de,where_id1)/oo_.var(where_id1,where_id1);
%Behavioral model
params = estimated_parameterization(default_switches,estparam_fname_baseline); 
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars; par_m(2) = params.m; par_rho_i(2) = params.rho_i;
Fama_coeffs(2) = oo_.var(where_de,where_id1)/oo_.var(where_id1,where_id1);
%Inflated priors model
params = estimated_parameterization(default_switches,estparam_fname_inflated); 
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars; par_m(3) = params.m; par_rho_i(3) = params.rho_i;
Fama_coeffs(3) = oo_.var(where_de,where_id1)/oo_.var(where_id1,where_id1);
%Behavioral model, low interest rate smoothing
params = estimated_parameterization(default_switches,estparam_fname_baseline); params.rho_i = 0.75;
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars; par_m(4) = params.m; par_rho_i(4) = params.rho_i;
Fama_coeffs(4) = oo_.var(where_de,where_id1)/oo_.var(where_id1,where_id1);
%Behavioral model, high interest rate smoothing
params = estimated_parameterization(default_switches,estparam_fname_baseline); params.rho_i = 0.95;
eval(['dynare ',model,' ''-DOptions="order = 1, irf=15, nocorr, nodecomposition, nofunctions, noprint, nograph"'' ''-DVar_List="',conv2string(vars),'"'' ''-Dj=',num2str(j),''' noclearall;']);
find_vars; par_m(5) = params.m; par_rho_i(5) = params.rho_i;
Fama_coeffs(5) = oo_.var(where_de,where_id1)/oo_.var(where_id1,where_id1);

%------------------------------------------------------%
% B. Display Exact Fama Regression Coefficients        %
%------------------------------------------------------%

clc;

fprintf('|--------------------------------------------------------------|\n');
fprintf('|     Exact values of Fama regression coefficients             |\n');
fprintf('|--------------------------------------------------------------|\n');
fprintf('|Rational model      (m=%4.2f, rho_i=%4.2f):   %+4.2f             |\n',[par_m(1) par_rho_i(1)  Fama_coeffs(1)]);
fprintf('|Baseline model      (m=%4.2f, rho_i=%4.2f):   %+4.2f             |\n',[par_m(2) par_rho_i(2) Fama_coeffs(2)]);
fprintf('|Inflated priors     (m=%4.2f, rho_i=%4.2f):   %+4.2f             |\n',[par_m(3) par_rho_i(3)  Fama_coeffs(3)]);
fprintf('|Baseline, low rho   (m=%4.2f, rho_i=%4.2f):   %+4.2f             |\n',[par_m(4) par_rho_i(4) Fama_coeffs(4)]);
fprintf('|Baseline, high rho  (m=%4.2f, rho_i=%4.2f):   %+4.2f             |\n',[par_m(5) par_rho_i(5) Fama_coeffs(5)]);
fprintf('|--------------------------------------------------------------|\n');  


end

if clean_dir
    clean_up
end
