% test_knapsack_opt
clear all; close all;
addpath('C:\gurobi952\win64\matlab');

STR_INFILE = './202106_sim_knapsack_data.mat';
STR_OUTFILE = './202210_opt_knapsack_result.mat';
%STR_OUTFILE = './202106_opt_knapsack_result.mat';
load(STR_INFILE);

% allocate results vector
x_opt = NaN(N_SIM, N);
val_opt = NaN(N_SIM, 1);
slack_opt = NaN(N_SIM, 1);
status_opt = cell(N_SIM, 1);

x_opt_min = NaN(N_SIM, N);
val_opt_min = NaN(N_SIM, 1);
slack_opt_min = NaN(N_SIM, 1);
orig_opt_gap_min = NaN(N_SIM, 1);
status_opt_min = cell(N_SIM, 1);

x_opt_max = NaN(N_SIM, N);
val_opt_max = NaN(N_SIM, 1);
slack_opt_max = NaN(N_SIM, 1);
orig_opt_gap_max = NaN(N_SIM, 1);
status_opt_max = cell(N_SIM, 1);

for ii = 1:N_SIM
    W = weight_mat(ii, :);
    V = value_mat(ii, :); 
    
    %
    % Knapsack model
    % --------------
    % max c'x
    % s.t. sum(w'x <= K); x binary
    % 

    model.obj = V; 
    model.A = sparse(W);
    model.rhs = K;
    model.sense = '<';
    model.vtype = 'B';
    model.modelsense = 'max';
    
    % set model
    params.outputflag = 0;    
    
    % extract output
    result = gurobi(model, params);
    
    x_opt(ii, :) = result.x(:)';
    val_opt(ii) = result.objval;
    slack_opt(ii) = result.slack;
    status_opt{ii} = result.status;
end

% second optimization
for ii = 1:N_SIM
    W = weight_mat(ii, :);
    V = value_mat(ii, :); 
    
    %
    % Knapsack model
    % --------------
    % min sum(x)
    % s.t. sum(w'x <= K); x binary
    %      c'x >= OPT

    model.obj = ones(size(V)); 
    model.A = [sparse(W); sparse(V)];
    model.rhs = [K; val_opt(ii)];
    model.sense = ['<'; '>'];
    model.vtype = 'B';
    model.modelsense = 'min';
    
    % set model
    params.outputflag = 0;    
    
    % extract output
    result = gurobi(model, params);
    
    x_opt_min(ii, :) = result.x(:)';
    val_opt_min(ii) = result.objval;
    slack_opt_min(ii) = result.slack(1);
    orig_opt_gap_min(ii) = result.slack(2); % should be zero
    status_opt_min{ii} = result.status;
end
   

% third optimization (max instread of min)
for ii = 1:N_SIM
    W = weight_mat(ii, :);
    V = value_mat(ii, :); 
    
    %
    % Knapsack model
    % --------------
    % max sum(x)
    % s.t. sum(w'x <= K); x binary
    %      c'x >= OPT

    model.obj = ones(size(V)); 
    model.A = [sparse(W); sparse(V)];
    model.rhs = [K; val_opt(ii)];
    model.sense = ['<'; '>'];
    model.vtype = 'B';
    model.modelsense = 'max';
    
    % set model
    params.outputflag = 0;    
    
    % extract output
    result = gurobi(model, params);
    
    x_opt_max(ii, :) = result.x(:)';
    val_opt_max(ii) = result.objval;
    slack_opt_max(ii) = result.slack(1);
    orig_opt_gap_max(ii) = result.slack(2);
    status_opt_max{ii} = result.status;
end
   

save(STR_OUTFILE, 'x_opt', 'val_opt', 'slack_opt', 'status_opt', ... 
    'x_opt_min', 'val_opt_min', 'slack_opt_min', 'status_opt_min', 'orig_opt_gap_min', ... 
    'x_opt_max', 'val_opt_max', 'slack_opt_max', 'status_opt_max', 'orig_opt_gap_max');
