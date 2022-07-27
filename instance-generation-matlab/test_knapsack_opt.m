% test_knapsack_opt
clear all; close all;

STR_INFILE = './202106_sim_knapsack_data.mat';
STR_OUTFILE = './202106_opt_knapsack_result.mat';
load(STR_INFILE);

% allocate results vector
x_opt = NaN(N_SIM, N);
val_opt = NaN(N_SIM, 1);
slack_opt = NaN(N_SIM, 1);
status_opt = cell(N_SIM, 1);

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
   
save(STR_OUTFILE, 'x_opt', 'val_opt', 'slack_opt', 'status_opt');
