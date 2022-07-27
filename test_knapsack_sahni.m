% test_knapsack_sahni
clear all; close all;

STR_INFILE = './202106_sim_knapsack_data.mat';
STR_OUTFILE = './202106_sahni_knapsack_result.mat';
load(STR_INFILE);

% allocate results vector
N_SAHNI_ORDERS = 5;
x_sahni_all = NaN(N_SIM, N, N_SAHNI_ORDERS);
val_sahni_all = NaN(N_SIM, N_SAHNI_ORDERS);
slack_sahni_all = NaN(N_SIM, N_SAHNI_ORDERS);

val_density_mat = value_mat ./ weight_mat;

for sahni_order = 0:(N_SAHNI_ORDERS-1)
    for ii = 1:N_SIM
        W = weight_mat(ii, :);
        V = value_mat(ii, :);
        D = val_density_mat(ii, :);
        
        
        % Sahni-k Algorithm
        % STEP 1: choose "optimal" knapsack with at most k items.
        
        %
        % Knapsack model
        % --------------
        % max c'x
        % s.t. sum(w'x <= K);
        %      sum(x) <= k;
        %      x binary
        %
        
        model.obj = V;
        model.A = sparse([W; ones(1, N)]);
        model.rhs = [K; sahni_order];
        model.sense = '<<';
        model.vtype = 'B';
        model.modelsense = 'max';
        
        % set model
        params.outputflag = 0;
        
        % extract output
        result = gurobi(model, params);
        
        % output vector
        x_sahni = result.x(:)';
        
        % STEP 2: fill remainder of backback with greedy algorithm
        % --------------------------------------------------------
        
        % sort by value density
        D(logical(x_sahni)) = 0; % remove those already chosen
        [sorted_D, sorted_ind] = sort(D, 'descend');
        sorted_W = W(sorted_ind);
        sorted_V = V(sorted_ind);
        
        % get indices of chosen values
        ind_accept = sorted_ind(cumsum(sorted_W) <= K - W * x_sahni(:));
        
        % check that accepted inds should not be those that had been accepted
        % before
        if(any(x_sahni(ind_accept)))
            error('Logical Error');
        end
        
        % now add
        x_sahni(ind_accept) = 1;
        
        x_sahni_all(ii, :, sahni_order+1) = x_sahni;
        val_sahni_all(ii, sahni_order+1) = V * x_sahni(:);
        slack_sahni_all(ii, sahni_order+1) = K - W * x_sahni(:);
    end
end
   
save(STR_OUTFILE, 'x_sahni_all', 'val_sahni_all', 'slack_sahni_all', 'N_SAHNI_ORDERS');
