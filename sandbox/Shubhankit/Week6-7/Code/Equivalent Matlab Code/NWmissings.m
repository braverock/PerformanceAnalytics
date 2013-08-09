function [B,V,q] = NWmissings(Y,X,es_am,bw_opt)

% NWMISSINGS Heteroskedasticity and autocorrelation robust standard errors
% for time series regressions with missing observations. 
%
%   [B,V,q] = NWmissings(Y,X,es_am,bw_opt) returns regression coefficients,
%   a Newey-West style variance-covariance matrix, and the fixed or
%   automatically chosen bandwidth for a multiple linear regression. It can
%   implement either the Equal Spacing or Amplitude Modulated estimator in
%   Datta and Du (2012).
%
%   INPUTS:
%       Y:      Tx1 vector of response observations
%       X:      Txk matrix of regressors. The LAST column of X should be a
%               column of ones so that the model contains a constant term.
%       es_am:  0 for Equal Spacing
%               1 for Amplitude Modulated
%       bw_opt: default value is 0
%               0 for fixed bandwidth [q=floor(4*((T/100)^(2/9)))]
%               1 for automatic bandwidth selection (Newey and West, 1987)
%
%   OUTPUTS:
%       B:      the vector of coefficients in the linear model Y = X*B
%       V:      kxk Newey-West variance-covariance matrix for B
%       q:      bandwidth used for V (fixed or automatic)
%
%   References:
%   [1] Datta, D. and Wenxin Du (2012) "Nonparametric HAC Estimation for
%       Time Series Data with Missing Observations".
%
%   [2] Newey, W. K., and K. D. West (1987) "A Simple, Positive
%       Semi-Definite Heteroskedasticity and Autocorrelation Consistent
%       Covariance Matrix," Econometrica, 55(3), 703–708.
%
%   [3] Newey, W. K., and K. D. West (1994) "Automatic Lag Selection in
%       Covariance Matrix Estimation," Review of Economic Studies, 61(4),
%       631–653.
%
%   See also REGRESS.
%
%   Version 1.0, updated 10/1/12 by D. Datta.
%
%   Note: These programs are provided only to facilitate further research.
%   These programs come with no guarantee (implied or otherwise) that they
%   work as intended or described. This material is solely the
%   responsibility of the author(s) and not the responsibility of the Board
%   of Governors of the Federal Reserve System or other members of its
%   staff.

    % Count number of input arguments

    if  nargin < 3
        error('NWmissings requires at least three input arguments.');
    elseif nargin >4
        error('NWmissings accepts at most four input arguments.');
    elseif nargin ==3
        bw_opt=0;
    end

    % Check that third and fourth input arguments are scalar 0 or 1. 
    
    if es_am~=0 && es_am~=1
        error('Third input argument (es_am) must be set to 0 or 1.');
    end
    if  bw_opt~=0 && bw_opt~=1
        error('Fourth input argument (bw_opt) must be set to 0 or 1.');
    end

    % Set the fixed bandwidth q. 
    % Note 1: For automatic bandwidth selection, this sets the initial
    % value for q.
    % Note 2: When some observations are missing, the Bandwidth depends on
    % full sample, not number of observed.
    
    [T,k] = size(X);
    q = floor(4*((T/100)^(2/9)));   
        
    % Use Matlab built-in function REGRESS for coefficients and residuals:
    
    [B,~,R]=regress(Y,X);  % beta is calculated ignoring NaNs.
    rr=repmat(R,1,k);
    z=X.*rr;
    
    % Remove missing values, if any:
    
    wasnan = (isnan(Y) | any(isnan(X),2));
    havenans = any(wasnan);

    if havenans
        if es_am==0             % ES case
            X(wasnan,:) = [];
            z(wasnan,:) = [];
    
        elseif es_am==1         % AM case
            X(wasnan,:) = 0;
            z(wasnan,:) = 0;
        end
    end
    
    M= T-sum(wasnan);   % number of nonmissing observations
    TT= size(z,1);      % used for indexing z. TT=T for AM, TT=M for ES.
    
    % Code used for automatic bandwidth selection only, purpose is to choose bandwidth:

    if bw_opt==1
    
        % weighting matrix has all ones, except 0 corresponding to the
        % column of x that is the constant term. When k=1, w=1.

        if k>1
            w=[ones(k-1,1);0];
        else
            w=1;
        end
        
        % use weighting vector to get scalar sigmas
        
        sigma0= (w'*z')*(w'*z')'/(M-k);
        ss_hat0=sigma0;
        ss_hat1=0;

        for j = 1:q
            sigma = ((w'*z(1:TT-j,:)')*(w'*z(j+1:TT,:)')')/(M-k);
            ss_hat1= ss_hat1+2*j*sigma;
            ss_hat0= ss_hat0+2*sigma;
        end

        gamma=1.1447*(((ss_hat1/ss_hat0)^2)^(1/3));
        q=floor((gamma*(T^(1/3))));
        q=min(q,TT-1); 

    end

    % Calculates V for for automatic and fixed bandwidth selection:

    gamma0 = z'*z/(M-k);
    S = gamma0;

    for v = 1:q
        gamma = z(1:TT-v,:)'*z(v+1:TT,:)/(M-k);
        gamma = 2*gamma;
        wt = 1 - v/(q+1);
        S = S + wt*gamma;
    end

    V=M*((X'*X)\eye(k))*S*((X'*X)\eye(k));

end
