% check_knapsack_opt_sols.m
% 
% Oct 20, 2022 
% 
% Script to check output of test_knapsack_opt.m

clear all; close all;

STR_INFILE_1 = './202106_sim_knapsack_data.mat';
STR_INFILE_2 = './202210_opt_knapsack_result.mat';
load(STR_INFILE_1);
load(STR_INFILE_2);

% Check 1: val_test should be zero
val_test = norm(val_opt - sum(x_opt_min .* value_mat, 2)) + norm(val_opt - sum(x_opt_max .* value_mat, 2));
fprintf('Val test result = %0.5f, expecting zero.\n', val_test);


% Check 2: check solutions
num_items = sum(x_opt, 2);
num_items_min = sum(round(x_opt_min), 2);
num_items_max = sum(round(x_opt_max), 2);

if(all(num_items_min <= num_items))
    sol_test_1 = 'PASSED';
else
    sol_test_1 = 'FAILED';
end
if(all(num_items_max >= num_items))
    sol_test_2 = 'PASSED';
else
    sol_test_2 = 'FAILED';
end

fprintf('Sol test 1 ... %s, Sol test 2 ... %s.\n', sol_test_1, sol_test_2);
