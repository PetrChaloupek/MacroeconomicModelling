close all; clear all; clc; 
%Inputs
 params.s_1 = 1; %Takes on values 0 or 1:   Set to zero to switch off extra terms in IS curve
 params.s_2 = 1; %Takes on values 0 or 1:   Set to zero to switch off discounting of inflation in real rate definition
 params.s_3 = 1; %Takes on values 0 or 1:   Set to zero to impose closed economy version of the model
 params.s_5 = 1; %Takes on values 0 or 1:   Set to zero to disable all shocks other than domestic monetary policy shocks


%This is the default parameterization of the OE_BNK model of Kolasa, Ravgotra and Zabczyk

%Parameter values
params.alppha           = 0.4;                  % Openness                                                                       
params.betta            = 0.99;                 % Discount Factor                                                                
params.delta            = 0.01;                 % Sensitivity of intermediation costs to the level of foreign debt               
params.epsilon          = 6;                    % IES among goods produced in a same country                                     
params.eta              = 1;                    % Elasticity of substitution between domestic and foreign goods                  
params.m                = 1;                 % Cognitive Discounting  (Gabaix, 2020; Table 1)
params.mu               = 1.2;                  % Gross product Markup                                                           
params.phi_pi           = 1.5;                  % Inflation Feedback Taylor Rule        
params.phi_pi_star      = 1.5;                  % Inflation Feedback Taylor Rule (foreign)  
params.phi_y            = 0.125;                % Output Feedback Taylor Rule
params.phi_y_star       = 0.125;                % Output Feedback Taylor Rule (foreign)
params.rho_rp           = 0.95;                 % Autocorrelation Technology Shock   
params.rho_g            = 0.95;                 % Autocorrelation Preference Shock                                                      
params.rho_g_star       = params.rho_g;         % Autocorrelation Preference Shock                                               
params.rho_i            = 0.9;                  % Interest rate smoothing parameter 
params.rho_i_star       = params.rho_i;         % Interest rate smoothing parameter 
params.rho_nu           = 0;                    % Autocorrelation Monetary Shock                                                    
params.rho_nu_star      = params.rho_nu;        % Autocorrelation Monetary Shock                                                 
params.rho_xi           = 0.95;                 % Autocorrelation Cost-push Shock                                                
params.rho_xi_star      = params.rho_xi;        % Autocorrelation Cost-push Shock                                                
params.siggma           = 1;                    % Effective Intertemporal elasticity of substitution                             
params.theta            = 0.875;                % Calvo Probability of not changing price (changed from 0.75 previously to flatten Phillips curve)                                     
params.varphi           = 3;                    % Inverse Frisch Elasticity of Labor Supply                                      
params.zetta            = 1/100;                % Country Size  

%Constants for observables (Average of the data series from 1990-2020)
params.yA               = 0.5;
params.rA               = 0.5;
params.piA              = 1;
params.yA_star          = 0.5;
params.rA_star          = 0.5;
params.piA_star         = 1;
params.deA              = 0;

%Steady State Values
params.B_star_Y_ss      = 0;
params.Y_Y_star_ss      = 1;
params.YC_ss            = 1;
params.YC_star_ss       = 1;


% Estimate the Model
dynare JIE_EST_Rational.mod;



