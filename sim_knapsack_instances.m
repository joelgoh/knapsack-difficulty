    % sim_knapsack_instances

clear all; close all;

s = RandStream('mt19937ar','Seed',0);
RandStream.setGlobalStream(s);

N_SIM = 100000;
K = 100; % Capacity of knapsack
N = 12 ; % number of items to choose from 
MIN_WEIGHT = 10;
MAX_WEIGHT = 40;
MIN_VALUES = 1:8;
MAX_VALUES = 21:30;

% output structures 
weight_mat = NaN(N_SIM, N);
value_mat  = NaN(N_SIM, N);
raw_value_mat = NaN(N_SIM, N);
corr_mat   = NaN(N_SIM, 1);
raw_corr_mat = NaN(N_SIM, 1);

% RHO for simulation
MIN_RHO = 0.9;
MAX_RHO = 0.99;
N_DISTINCT_RHO = 10; % ensure that this is a divisor of N_SIM
rho_vec = repmat(linspace(MIN_RHO, MAX_RHO, N_DISTINCT_RHO), N_SIM/N_DISTINCT_RHO, 1); % vector of all rhos
rho_vec = rho_vec(:);
r_vec = rho_vec ./ (rho_vec + sqrt(1 - rho_vec.^2)); % derived by formula in sheet


% iterate
all_weights = (MIN_WEIGHT:MAX_WEIGHT)'; % for easy permutation
min_value_mat = MIN_VALUES(randi(length(MIN_VALUES), N_SIM, 1))'; % smallest and largest values per simulation
max_value_mat = MAX_VALUES(randi(length(MAX_VALUES), N_SIM, 1))';
for ii = 1:N_SIM
    % randomly create vector of all weights
    cur_weight_vec = all_weights(randperm(length(all_weights), N));
    
    % first value (perfect correlation with weights)
    val1 = (cur_weight_vec - MIN_WEIGHT) / (MAX_WEIGHT - MIN_WEIGHT + 1); % component that is perfectly correlated with weight vec
    val1 = (val1 - mean(val1)) ./ std(val1);  % normalize
    
    % second value (orthogonal to weights)
    normalized_weight_vec = cur_weight_vec - mean(cur_weight_vec);
    proj_mat = eye(N) - normalized_weight_vec * normalized_weight_vec' / (normalized_weight_vec' * normalized_weight_vec); % projection matrix of component that is uncorrelated
    val2 = proj_mat * rand(N, 1);
    val2 = (val2 - mean(val2)) ./ std(val2);  % normalize
    
    % value is the weighted sum of terms, using r_vec 
    val = r_vec(ii) * val1 + (1 - r_vec(ii)) * val2; 
    
    % normalize
    val = val - min(val);
    val = val ./ max(val);
    
    % write to output
    weight_mat(ii, :) = cur_weight_vec(:)';     % weight vector  
    raw_value_mat(ii, :) = val(:)'; % raw value vector for reporting
    value_mat(ii, :)  = round(min_value_mat(ii) + val(:)' * (max_value_mat(ii) - min_value_mat(ii))); % rounded value vector
    raw_corr_mat(ii, :) = corr(weight_mat(ii, :)', raw_value_mat(ii, :)');
    corr_mat(ii, :) = corr(weight_mat(ii, :)', value_mat(ii, :)');
end 

% save output
save('./202106_sim_knapsack_data.mat', 'weight_mat', 'value_mat', 'corr_mat', 'rho_vec', ...
     'N_SIM', 'K', 'N', 'MIN_WEIGHT', 'MAX_WEIGHT', 'min_value_mat', 'max_value_mat', ... 
     'MIN_VALUES', 'MAX_VALUES', 'N_DISTINCT_RHO', 'MIN_RHO', 'MAX_RHO');

% save('./REPORTING_knapsack_data.mat', 'weight_mat', 'value_mat', 'raw_value_mat', 'corr_mat', 'raw_corr_mat', 'rho_vec', ...
%     'N_SIM', 'K', 'N', 'MIN_WEIGHT', 'MAX_WEIGHT', 'min_value_mat', 'max_value_mat', ... 
%     'MIN_VALUES', 'MAX_VALUES', 'N_DISTINCT_RHO', 'MIN_RHO', 'MAX_RHO');


