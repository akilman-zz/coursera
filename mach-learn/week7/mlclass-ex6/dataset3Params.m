function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

C_vals = [0.01 0.03 0.1 0.3 1 3 10 30];
sigma_vals = [0.01 0.03 0.1 0.3 1 3 10 30];
minError = 999999999;
bestC = 1;
bestSigma = 0.3;


for i = 1:length(C_vals)

  C = C_vals(i);

  for j = 1:length(sigma_vals)

    sigma = sigma_vals(j);

    % Train models with C & sigma input parameters
    model = svmTrain(X, y, C, @(x1, x2) gaussianKernel(x1, x2, sigma));

    % Determine the error
    predictions = svmPredict(model, Xval);
    modelError = mean(double(predictions ~= yval));

    % If this instances error is less than the current minimum, update C & sigma
    if (modelError < minError)
      bestC = C;
      bestSigma = sigma;
      minError = modelError;
      fprintf('C=%f, sigma=%f, minError=%f\n', C, sigma, minError);
    end

  end
end

% And finally, set optimal params to be returned 
C = bestC;
sigma = bestSigma;

fprintf('Found optimal values C=%f, sigma=%f', C, sigma)


% =========================================================================

end
