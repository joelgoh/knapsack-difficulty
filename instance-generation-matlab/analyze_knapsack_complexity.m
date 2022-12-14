% analyze_knapsack_complexity

clear all; close all;
STR_INFILE_SIM = './202106_sim_knapsack_data.mat';
%STR_INFILE_OPT = './202106_opt_knapsack_result.mat';
STR_INFILE_OPT = './202210_opt_knapsack_result.mat';
STR_INFILE_SAHNI = './202106_sahni_knapsack_result.mat';

load(STR_INFILE_SIM); 
load(STR_INFILE_OPT);
load(STR_INFILE_SAHNI);

% % write output to csv file
% STR_CSV_OUT = './knapsack_instances.csv';
% % fid = fopen(STR_CSV_OUT);
% % fprintf(fid, '%s,', 'InstanceID');
% % fprintf(fid, '%s,%s,', 'InstanceID');
% permute_mat = [1:N;(N+1):2*N];
% out_mat = [weight_mat, value_mat];
% out_mat = [(1:N_SIM)', out_mat(:, permute_mat(:)')];
% dlmwrite(STR_CSV_OUT, out_mat, '-append', 'delimiter', ',');

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % write output with solutiosn to csv file
% STR_CSV_OUT = './knapsack_instances_with_sols.csv';
% permute_mat = [1:N;(N+1):2*N];
% out_mat = [weight_mat, value_mat];
% out_mat = [(1:N_SIM)', out_mat(:, permute_mat(:)'), x_opt, val_opt];
% dlmwrite(STR_CSV_OUT, out_mat, '-append', 'delimiter', ',');
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calculate sahni complexity
sahni_complexity = NaN(N_SIM, 1);
[maxA, indmaxA] = max(val_sahni_all == repmat(val_opt, 1, N_SAHNI_ORDERS), [], 2);
sahni_complexity(~maxA) =  N_SAHNI_ORDERS;
sahni_complexity(maxA) = indmaxA(maxA)-1;

% discretize corr mat
bins = [0, unique(rho_vec)', 1];
discr_corr_mat = discretize(corr_mat, bins);

% crosstabulate discrete correlation matrix with Sahni complexity
ct_corr_sahni = crosstab(discr_corr_mat, sahni_complexity)

% crosstabulate optimal bag size with Sahni complexity
ct_itemcount_sahni = crosstab(sum(x_opt, 2), sahni_complexity)

% scatter plot of complexity vs correlation
figure;
plot(corr_mat, sahni_complexity, 'o'); 
title('Scatter plot of Sahni complexity vs Weight/Value correlation');
xlabel('Correlation');
ylabel('Sahni complexity');

% scatter plot of correlation vs optimal # items 
figure;
plot(corr_mat, sum(x_opt, 2), 'o'); 
title('Scatter plot of optimal # items vs in bag vs Weight/Value correlation');
xlabel('# items at optimality');
ylabel('Correlation');

% CDF of correlation
figure;
plot(sort(corr_mat), (1:N_SIM)./N_SIM, 'o'); 
title('Cumulative frequency of correlation distribution');
xlabel('Correlation between value and weight vectors');
ylabel('Cumulative frequency');

% calculate optimality gap from sahni
abs_sahni_opt_gap = bsxfun(@minus, val_opt, val_sahni_all);
rel_sahni_opt_gap = bsxfun(@rdivide, abs_sahni_opt_gap, val_opt);

% Added 30 Dec 2021: Find greedyval gap (take all highest value items until
% bust)
greedyval0_vec = NaN(N_SIM, 1);
for ii = 1:N_SIM
    [sorted_curval, ind] = sort(value_mat(ii, :), 'descend');
    sorted_curwt = weight_mat(ii, ind);
    greedyval0_vec(ii) = sum(sorted_curval(cumsum(sorted_curwt) <= K));
end
greedyval0_abs_opt_gap = val_opt - greedyval0_vec;
greedyval0_rel_opt_gap = greedyval0_abs_opt_gap ./ val_opt;

% WRITE ALL OUTPUT
str_header = ['InstanceID,', ...
                sprintf('weight%d,', 1:N), ...
                sprintf('value%d,', 1:N), ...
                'wtval_corr_designed,wtval_corr_actual,sahni_complexity,', ...
                sprintf('sahni_order_%d_abs_opt_gap,', 0:N_SAHNI_ORDERS-1), ...
                sprintf('sahni_order_%d_rel_opt_gap,', 0:N_SAHNI_ORDERS-1), ...
                sprintf('greedyval_0_abs_opt_gap,') ...
                sprintf('greedyval_0_rel_opt_gap,') ...
                sprintf('xsol%d,', 1:N), sprintf('xsol%d_max,', 1:N), ...
                'opt_numitems,opt_numitems_max,opt_totalweight,optval\n'];
out_mat = [(1:N_SIM)', weight_mat, value_mat/100, ...
            rho_vec, corr_mat, sahni_complexity, ...
            abs_sahni_opt_gap/100, ...
            rel_sahni_opt_gap, ...
            greedyval0_abs_opt_gap/100, ...
            greedyval0_rel_opt_gap, ...
            x_opt_min, x_opt_max, ...
            sum(x_opt_min, 2), sum(x_opt_max, 2), sum(weight_mat .* x_opt, 2), val_opt/100]; 
STR_CSV_OUT_2 = './knapsack_instances_with_complexity.csv';
fid = fopen(STR_CSV_OUT_2, 'w');
fprintf(fid, str_header);

fmt_str = ['%d,', repmat('%d,', [1, 2*N]), ...          % InstanceID, weight and value matrix
            repmat('%0.15f,', [1, 2]), '%d,', ...       % 2 correlations and sahni complexity
            repmat('%d,', [1, N_SAHNI_ORDERS]), ...     % absolute sahni gap
            repmat('%0.15f,', [1, N_SAHNI_ORDERS]), ... % relative sahni gap
            '%d,', ...                                  % absolute greedy gap
            '%0.15f,', ...                              % relative greedy gap
            repmat('%d,', [1, 2*N+3]), ...              % xsolution (min+max), optimal num items (min, max), optimal weight
            '%d\n'];                                    % optimal value
            
fprintf(fid, fmt_str, out_mat');
fclose(fid)