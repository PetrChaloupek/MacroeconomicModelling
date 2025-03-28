% Initialize environment
close all    % Close all open figure windows
clear        % Clear all variables from the workspace
clc          % Clear the command window
addpath(genpath('.\functions')); % Add all functions from the 'functions' directory to the path

period = 2;  % 1 = Whole period, 2 = 1990-2017  

if period==1
% Load data from Excel (sheet 'Quarterly', columns B:H contain macroeconomic data)
[DATA, VARI, raw] = xlsread('MAMO_DATA_Sem01.xlsx','Quarterly', 'B:H');
% Define the time axis with quarterly frequency (from 1954 to 2017)
time = 1954.25:0.25:2017.25;
else 
% Load data from Excel (sheet 'Quarterly', columns B:H contain macroeconomic data)
[~, VARI, ~] = xlsread('MAMO_DATA_Sem01.xlsx', 'Quarterly', 'B1:H1');
[DATA, ~, ~] = xlsread('MAMO_DATA_Sem01.xlsx','Quarterly', 'B146:H254');
% Define the time axis with quarterly frequency (from 1954 to 2017)
time = 1990.25:0.25:2017.25;
end

%% Plot raw data - Task 1a



% Open a new figure for plots
figure
% For each of the 7 variables in 'DATA'
for i = 1:7
    subplot(3,3,i)  % Create a subplot (3x3 grid, each subplot is created sequentially)
    plot(time(1:end), DATA(:,i), 'k', LineWidth=2);  % Plot data in black with line width 2
    title(VARI(i));  % Set the title of the plot based on the variable name
end

%% Plot output gap using HP filter and growth rate using first difference - Task 1b

% Transform the output to a logged time series (logarithms are necessary for percentage changes)
Output_log = log(DATA(:,1));

% Set the lambda parameter for the HP filter (1600 is standard for quarterly data)
lambda = 1600;

% Apply HP filter to the logged time series (returns the estimated trend)
Y_t = hpfilter(Output_log, lambda);

% Open a new figure for plots
figure
% First plot: Plot the original time series and the trend obtained from the HP filter
subplot(2,1,1)
plot(time, Output_log, 'k--', 'linewidth', 2)  % Plot the original time series (black dashed line)
hold on  % Keep the current plot for additional lines
plot(time, Y_t, 'k', 'linewidth', 1.5)  % Plot the trend obtained from the HP filter (black solid line)
legend('Original time series', 'Estimated Trend')  % Add a legend

% Second plot: Plot the output gap and growth rate
subplot(2,1,2)
plot(time, Output_log - Y_t, 'k', 'linewidth', 2)  % Plot the output gap as the difference between the original series and the trend
hold on  % Keep the plot for additional lines
plot(time(2:end), diff(Output_log) - mean(diff(Output_log)), 'r', 'linewidth', 2);  % Plot the growth rate (demeaned first differences of the log of GDP)
legend('Output gap', 'Output growth (demeaned)')  % Add a legend

%% Plot and save output gaps obtained by HP filter - Task 1c

% Apply HP filter to each variable in 'DATA'
for i = 1:7
    if i == 4  % If the variable is indexed as 4 (interest rate), do not apply logarithm
        DATA2(:,i) = DATA(:,i) - hpfilter(DATA(:,i), lambda);
    else  % For other variables, apply logarithm
        DATA2(:,i) = (log(DATA(:,i)) - hpfilter(log(DATA(:,i)), lambda)) * 100;    
    end
end

% Plot the output gaps for each variable
figure
for i = 1:7
    subplot(3,3,i)  % Create a subplot (3x3 grid)
    plot(time(1:end), DATA2(:,i), 'k', LineWidth=2);  % Plot the output gap in black
    title(VARI(i));  % Set the title of the plot based on the variable name
end

% Plot the output gaps and GDP for comparison
figure
for i = 1:7
    subplot(3,3,i)  % Create a subplot (3x3 grid)
    plot(time(1:end), DATA2(:,i), 'k', LineWidth=2);  % Plot the output gap in black
    hold on 
    plot(time(1:end), DATA2(:,1), 'r:', LineWidth=1.5);  % Plot GDP (the first variable) in red dashed line
    title(VARI(i));  % Set the title of the plot based on the variable name
end

%% Task 2: Calculate basic statistics

MIN_DATA = min(DATA2);  % Calculate the minimum value for each variable
MAX_DATA = max(DATA2);  % Calculate the maximum value for each variable
RANGE = MAX_DATA - MIN_DATA;  % Calculate the range (max - min) for each variable
STD_DATA = std(DATA2);  % Calculate the standard deviation for each variable
STD_DATA_vs_output = std(DATA2) / std(DATA2(:,1));  % Calculate relative volatility compared to GDP (output gap)

% Create a table with the results
T1 = table(VARI', RANGE', STD_DATA', STD_DATA_vs_output');
T1.Properties.VariableNames = {'Variable', 'Range', 'Std. Var', 'Std. Var vs. Std. Output'};
disp(T1);  % Display the table

% Plot autocorrelation functions (ACF) for each variable
figure
for i = 1:7
    subplot(3,3,i)
    autcf(DATA2(:,i), 20);  % Calculate and plot ACF up to 20 lags
    title(VARI(i));  % Set the title of the plot based on the variable name
end

% Calculate the correlation matrix between variables
CORR_DATA = corr(DATA2);

% Create a table with the correlation matrix
T2 = table(VARI', CORR_DATA);
disp(T2);  % Display the table

%% Task 3: Analyze time lagging relative to GDP

k = 4;  % Number of lags forward and backward
alpha = 0.05;  % Set significance level
figure
KOR = zeros(7, 2*k + 1);  % Initialize the matrix for correlations

% Calculate cross-correlation functions for each variable relative to GDP
for i = 1:7
    KOR(i, :) = corrfn(DATA2(:,i), DATA2(:,1), k);  % Cross-correlation between the i-th variable and GDP
    display(VARI{i});  % Display the variable name in the command window
    P1=corrtest(KOR(i,:), length(time));  % Perform a correlation test
    CONINT(KOR(i,:), length(time), alpha, i);  % Calculate and plot confidence intervals for correlations
    T=table((-k:1:k)',P1(:,3));
    T.Properties.VariableNames = {'Shift', 'Significant (1=yes)'};
    display(T)
    title(VARI{i});  % Set the title of the plot based on the variable name
end
