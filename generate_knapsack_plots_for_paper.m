% generate_knapsack_plots_for_paper

clear all; close all;
STR_INFILE_SIM = './REPORTING_knapsack_data.mat';
STR_INFILE_OPT = './202106_opt_knapsack_result.mat';
STR_INFILE_SAHNI = './202106_sahni_knapsack_result.mat';

load(STR_INFILE_SIM); 
load(STR_INFILE_OPT);
load(STR_INFILE_SAHNI);

% calculate sahni complexity
sahni_complexity = NaN(N_SIM, 1);
[maxA, indmaxA] = max(val_sahni_all == repmat(val_opt, 1, N_SAHNI_ORDERS), [], 2);
sahni_complexity(~maxA) =  N_SAHNI_ORDERS;
sahni_complexity(maxA) = indmaxA(maxA)-1;

% discretize corr mat
bins = [0, unique(rho_vec)', 1];
discr_corr_mat = discretize(corr_mat, bins);

% CDF of actual correlation
sorted_corr_mat = sort(corr_mat);
rho_for_plot = linspace(0.87, 1, N_SIM);
cdf_actual_corr = NaN(N_SIM, 1);
for ii = 1:N_SIM
    insert_ind = find(sorted_corr_mat <= rho_for_plot(ii), 1, "last");
    if(isempty(insert_ind))
        cdf_actual_corr(ii) = 0;
    else
        cdf_actual_corr(ii) = insert_ind./N_SIM;
    end
end
h = figure;
plot(rho_for_plot, cdf_actual_corr, 'bo', 'MarkerFaceColor', 'blue'); 
xlabel('Correlation between weight vectors $w$ and value vectors $v$','Interpreter','latex');
ylabel('Cumulative frequency', 'Interpreter','latex');
set(h,'PaperUnits','inches','PaperPosition',[0 0 4 3])
print(h, './pearson_plot_actual.png', '-dpng', '-r100');

% scatter plot of complexity vs correlation
h = figure;
plot(corr_mat, sahni_complexity, 'bx'); 
%title('Scatter plot of Sahni complexity vs weight/value correlation');
xlabel('Pearson correlation beween weight and value vectors', 'Interpreter','latex');
ylabel('Sahni complexity', 'Interpreter','latex');
h2 = gca;
set(h2, 'YTick', 0:5, 'YLim', [0,5]);
set(h,'PaperUnits','inches','PaperPosition',[0 0 4 3])
print(h, './scatter_pearson_sahni.png', '-dpng', '-r100');

% scatter plot of correlation vs optimal # items 
h = figure;
plot(corr_mat, sum(x_opt, 2), 'bx'); 
%title('Scatter plot of optimal # items vs in bag vs Weight/Value correlation');
xlabel('Pearson correlation beween weight and value vectors', 'Interpreter','latex');
ylabel('Num. items at optimality', 'Interpreter','latex');
h2 = gca;
set(h2, 'YTick', 2:7, 'YLim', [2,7]);
set(h,'PaperUnits','inches','PaperPosition',[0 0 4 3])
print(h, './scatter_pearson_numopt.png', '-dpng', '-r100');

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


% generate correlation heatmap of all features
feature_corr = corr([corr_mat, sahni_complexity, sum(x_opt, 2), rel_sahni_opt_gap,greedyval0_rel_opt_gap]);
h = figure;
h2 = heatmap(feature_corr, 'CellLabelFormat','%0.2f', 'Colormap',bone);
str_labels = {'(a)', '(b)', '(c)', '(d)', '(e)', '(f)', '(g)', '(h)', '(i)'};
set(h2, 'XDisplayLabels', str_labels, 'YDisplayLabels', str_labels);
set(h,'PaperUnits','inches','PaperPosition',[0 0 4 3])
print(h, './feature1_corr.png', '-dpng', '-r100');

