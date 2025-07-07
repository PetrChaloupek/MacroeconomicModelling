function params = estimated_parameterization(inputs,fname)

%This is the estimated parameterization of the OE_BNK model of Kolasa, Ravgotra and Zabczyk

%This version 9/22/2022

%Load default parametrization
params = default_parameterization(inputs);

load(fname);
nparams = length(xparam1);
for i=1:nparams
    parname = string(parameter_names(i));
    if sum(strcmp(parameter_names(i),fieldnames(params)))
        params.(parname) = xparam1(i);
    end
end

%Composite or dependent parameters
params.m_star           = params.m;
params.alppha_star      = params.zetta/(1-params.zetta)*params.YC_star_ss*params.Y_Y_star_ss*(1-(1-params.alppha)/params.YC_ss);
params.lambda           = (1-params.theta)*(1-params.betta*params.theta)/params.theta;
