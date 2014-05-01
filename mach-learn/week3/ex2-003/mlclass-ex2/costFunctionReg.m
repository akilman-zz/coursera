function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta

% ====================== Cost ======================
%
% left = -y .* log(sigmoid(X*theta))
% right = (1 - y) .* (log(1 - sigmoid(X*theta)))
% J_left = (1 / m) .* sum(left - right)

% % Note, theta(0) should not be regularized. Instead of not computing, will just set theta(0) to zero
% reg_theta = theta
% reg_theta(1) = 0
% J_right = (lambda / (2 * m)) * sum(reg_theta .^ 2)

% J = J_left + J_right

% Note, theta(0) should not be regularized. Instead of not computing, will just 
% set theta(0) to zero
reg_theta = theta
reg_theta(1) = 0

left = -y .* log(sigmoid(X*reg_theta))
right = (1 - y) .* (log(1 - sigmoid(X*reg_theta)))
J_left = (1 / m) .* sum(left - right)
J_right = (lambda / (2 * m)) * sum(reg_theta .^ 2)

J = J_left + J_right

% ====================== Gradient ======================
%

% fprintf('Original flavor.\n');
% 0th/1st entry
% mySum = 0
% for i = 1:m % for each sample%
% 	mySum = mySum + (sigmoid(X(i,:)*theta) - y(i)) * X(i,1)
% end
% grad(1) = (1 / m) .* mySum

% remaining
% for j = 2:size(theta)
% 	mySum = 0
% 	for i = 1:m % for each sample
% 		mySum = mySum + (sigmoid(X(i,:)*theta) - y(i)) * X(i,j)
% 	end
% 	grad(j) = (1 / m) .* mySum + ((lambda / m) * theta(j))
% end

fprintf('New flavor.\n');
temp_theta = theta
temp_theta(1) = 0

z = X*temp_theta
h_theta = sigmoid(X*temp_theta) % also g(z)
beta_matrix = h_theta - y
regularizationParam = (lambda / m) .* reg_theta

grad = ((1 / m) .* (X' * beta_matrix)) + regularizationParam
% 

% =============================================================

end
