function [theta, J_history] = gradientDescentMulti(X, y, theta, alpha, num_iters)
%GRADIENTDESCENTMULTI Performs gradient descent to learn theta
%   theta = GRADIENTDESCENTMULTI(x, y, theta, alpha, num_iters) updates theta by
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCostMulti) and gradient here.
    %


    computeCost(X, y, theta); % for debugging

    mysum = zeros(size(X, 2), 1)                % start with column vector of zeros for sum; row count should match nFeatures
    for i = 1:m                                 % for each sample

        %x_sample_i = X(i, :)'                   % ith sample of x, represented as a column vector of n x 1
        %h_theta_of_x = theta' * x_sample_i      % compute h_theta(x)
        %y_sample_i = y(i)                       % ith sample of y, 1x1

        mysum = mysum + ((theta' * X(i, :)') - y(i)) * X(i, :)' 
    end

    delta = 1/m * mysum
    theta = theta - alpha*delta

    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCostMulti(X, y, theta);

end

end
