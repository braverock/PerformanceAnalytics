function [V,S]=nwse(e,X,nlags)
% PURPOSE: computes Newey-West adjusted heteroscedastic-serial
%          consistent standard errors (only se's)
%---------------------------------------------------
% USAGE: [V,S] = nwse(e,X,nlag)
% where: e = T x n vector of model residuls
%        X = T x k matrix of independ vars
%    nlags = lag length to use
%---------------------------------------------------
% RETURNS:
%    V   is the Newey-West Var-Cov matrix
%    S   is the spectral density of u = e.*X
% --------------------------------------------------

% written by:  Mike Cliff, Purdue Finance,  mcliff@mgmt.purdue.edu
% CREATED 11/17/00
% MODIFIED 1/23/01 Input e, X separtely; return V, S; df adjustment
%          2/20/01 Allow for system of eqs (multiple e vectors)
  
if (nargin ~= 3); error('Wrong # of arguments to nwse'); end;

[T,k] = size(X);
n = cols(e);
S = zeros(n*k,n*k);
if k == 1 & X == ones(T,1)
  u = e;
else
  u = [];
  for i = 1:cols(e)
    u = [u repmat(e(:,i),1,k).*X];
  end
end

for lag = 0:nlags
  rho = u(1:T-lag,:)'*u(1+lag:T,:)/(T-k);  
  if lag >= 1, rho = rho + rho'; end
  wt = 1 - lag/(nlags+1);
  S = S + wt*rho;    
end

V = kron(eye(n),(X'*X/T)\eye(k));
V = V*S*V/T;