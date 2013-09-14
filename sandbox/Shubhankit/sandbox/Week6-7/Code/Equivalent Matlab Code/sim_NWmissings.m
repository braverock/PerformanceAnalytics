% SIM_NWMISSINGS: 
%  This program applies NWMissings.m to simulated data to assess the
%  relative performance of the Equal Spacing, Amplitude Modulated, and
%  Imputation methods for calculating heteroskedasticity and
%  autocorrelation robust standard errors for time series regressions with
%  missing observations.
%  
%  Since researchers have a choice of consistent estimators, the purpose of
%  this program is to allow researchers to simulate the empirical bias and
%  variance of the AM and ES estimators for a parameter set of their
%  choosing, before deciding which estimator to implement. 
%
%  This simulation program relies on NWmissings.m, which returns regression
%  coefficients, a Newey-West style variance-covariance matrix, and the
%  fixed or automatically chosen bandwidth for a multiple linear
%  regression. NWmissings.m can implement either the Equal Spacing or
%  Amplitude Modulated estimator, detailed in Datta and Du (2012).
%
%   INPUTS:
%       N:       number of iterations (scalar)
%       T:       sample size (scalar)
%       phi:     vector of autocorrelation parameters
%       crit:    critical value for t-tests (scalar)
%       bw_opt:  0 for fixed bandwidth, 1 for automatic bandwidth selection
%       mstruc:  0 for Bernoulli missing, 1 for deterministic cycles
%       a:       fraction of Bernoulli missings
%       mm:      cycle length for deterministic missings
%       missing: vector of deterministically missing observations
%
%   OUTPUTS: Outputs are all matrices. Each results matrix has one column
%   for each phi parameter, and one row per estimator for the ES, AM, IM,
%   and NW (full sample) results.
%       Rejection_rates:        empirical rejection rate
%       Power:                  size-adjusted power
%       Selected_Bandwidths:    mean of the bandwidths selected
%       Bias_Squared:           empirical bias of the estimator, squared
%       Variance:               empirical variance of the estimator
%       MSE:                    mean-squared error
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
%   See also NWMISSINGS.
%
%   Version 1.0, updated 10/1/12 by D. Datta.
%
%   Note: These programs are provided only to facilitate further research.
%   These programs come with no guarantee (implied or otherwise) that they
%   work as intended or described. This material is solely the
%   responsibility of the author(s) and not the responsibility of the Board
%   of Governors of the Federal Reserve System or other members of its
%   staff.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameters To Be Selected
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N=1000;             % number of iterations
T=1200;             % sample size
phi=[0 0.5 0.9];    % autocorrelation of error term
crit=1.96;          % critical value for 0.05 level
bw_opt=0;           % 0 for fixed bandwidth, 1 for automatic bandwidth selection

% missing structure parameters:

mstruc=0;           % mstruc=0 for Bernoulli missing, 1 for deterministic cycles

if mstruc==0
    a=0.5;              % a= fraction of Bernoulli missings

elseif mstruc==1
    mm=12;              % mm = cycle length
      
    % "missing" vector sets which observations in the cycle are missing.
    % length of vector should be less than mm above, and elements of vector
    % should be integers between 1 and mm. 
    missing=[1 2 6 9]'; 
    
    m=size(missing,1);
    a=m/mm;

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simulation Setup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% preallocating matrices
k=size(phi',1);

% for t-stats
T_NW=zeros(N,k);
T_ES=T_NW;          
T_AM=T_NW;          
T_IM=T_NW;          

% for alternative t-stats
Z_NW=T_NW;    
Z_ES=T_NW;    
Z_AM=T_NW;    
Z_IM=T_NW;    

% for selected bandwidths
Q_NW=T_NW;    
Q_ES=T_NW;    
Q_AM=T_NW;    
Q_IM=T_NW;    

% for variances
SS_NW=T_NW;   
SS_ES=T_NW;   
SS_AM=T_NW;   
SS_IM=T_NW;   

% for number of nonmissing observations
M=T_NW;                 

% The size-adjusted power is computed using a one-sided test. 
% We test H0:mu=(+/-)4/sqrt(T(1-phi^2)) versus Ha:mu=0.
% The value of mu under H0 is the same for all estimators.
H0=4./sqrt(T'*(1-phi.^2)); 

% set the random number generator to a clock based function:
s = RandStream.create('mt19937ar','seed',sum(100*clock));
RandStream.setGlobalStream(s);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data Structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set up missing index for deterministic missings

if mstruc==1
    A=repmat(missing,1,T/mm)+repmat([0:1:(T/mm)-1]*mm,m,1);
    A=reshape(A,T*a,1);          % index of missings for 1:T
    AA=repmat(A,1,k)+repmat([0:1:k-1]*T,T*a,1);
    AA=reshape(AA,k*T*a,1); 
end


for iter=1:N
     
% generate w(t), the underlying complete data series
    e=randn(T,1);
    w=zeros(T,k);
    w(1,:)=ones(1,k)*e(1,1);
    for t=2:T;
        w(t,:)=phi.*w(t-1,:)+e(t,1);
    end
    
% generate data series:
y=w;
if mstruc==1
    y(AA)=NaN;          % AA indexes missings for deterministic cycles  
elseif mstruc==0
    x = rand(T,1);
    g1 = (x >= a);
    g=g1*ones(1,k);     % amplitude modulating series
    y(g == 0) = NaN;    % amplitude modulated series (has NaNs)
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Estimators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
   
%--------------------------------------------------------------------------
% (1) Newey West estimator for full (nonmissing) series.
%--------------------------------------------------------------------------
 
        y1=w(1:T,:);        % y1 is the full series, for a benchmark
        rhs=ones(size(y1(:,1)));
        b_NW=zeros(1,k);    % matrix for coefficients 
        
        for i=1:k;
            [b_NW(i),SS_NW(iter,i),Q_NW(iter,i)] = NWmissings(y1(:,i),rhs,0,bw_opt);
        end 
        
        % Hypothesis testing:
        se_NW = sqrt(SS_NW(iter,:));
        T_NW(iter,:)=abs(b_NW)./se_NW;
        Z_NW(iter,:)=abs(b_NW-H0)./se_NW;
        
        
%--------------------------------------------------------------------------
% (2) Imputation - applies Newey West estimator to linearly imputed data
%--------------------------------------------------------------------------
        
        % Construct y2, the series with linearly interpolated values

        yz=y;                           % series with missing values
        y2=y;
        xi=(1:T)';                      % index for full data
        x2=xi(any(yz,2),:);             % index of observed data
        yz(any(isnan(yz),2),:) = [];    % series without missings
    
        for i=1:k
            y2(:,i) = interp1(x2,yz(:,i),xi,'linear','extrap');
        end
        
        % Calculate betahat and variance matrix:       
        b_IM=zeros(1,k);                % matrix for coefficients 
        for i=1:k;
            [b_IM(i),SS_IM(iter,i),Q_IM(iter,i)] = NWmissings(y2(:,i),rhs,0,bw_opt);
        end 
        
        % Hypothesis testing:
        se_IM = sqrt(SS_IM(iter,:));
        T_IM(iter,:)=abs(b_IM)./se_IM;
        Z_IM(iter,:)=abs(b_IM-H0)./se_IM;
        
        
%--------------------------------------------------------------------------
% (3) Equal Spacing Estimator
%--------------------------------------------------------------------------
  
        T_nm=sum(g(:,1));
        M(iter,:)=T_nm;                 % store the number of nonmissings
                
        % Calculate betahat and variance matrix:       
        b_ES=zeros(1,k);                % matrix for coefficients 
        for i=1:k;
            [b_ES(i),SS_ES(iter,i),Q_ES(iter,i)] = NWmissings(y(:,i),rhs,0,bw_opt);
        end 
        
        % Hypothesis testing:
        se_ES = sqrt(SS_ES(iter,:));
        T_ES(iter,:)=abs(b_ES)./se_ES;
        Z_ES(iter,:)=abs(b_ES-H0)./se_ES;
        
        
%--------------------------------------------------------------------------
% (4) Amplitude Modulated Estimator
%--------------------------------------------------------------------------
               
        % Calculate betahat and variance matrix:       
        b_AM=zeros(1,k);                % matrix for coefficients 
        for i=1:k;
            [b_AM(i),SS_AM(iter,i),Q_AM(iter,i)] = NWmissings(y(:,i),rhs,1,bw_opt);
        end 
        
        % Hypothesis testing:
        se_AM = sqrt(SS_AM(iter,:));
        T_AM(iter,:)=abs(b_AM)./se_AM;
        Z_AM(iter,:)=abs(b_AM-H0)./se_AM;
           
end
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculating Result Statistics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rejection rates

I_ES=T_ES>crit;            % ES, bandwidth based on T
I_AM=T_AM>crit;            % Stata-Parzen, bandwidth based on T
I_IM=T_IM>crit;            % Imputation, bandwidth based on T
I_NW=T_NW>crit;            % NW no missings), bandwidth based on T

R_ES=sum(I_ES)/N;
R_AM=sum(I_AM)/N;
R_IM=sum(I_IM)/N;
R_NW=sum(I_NW)/N;

Rejection_rates=[R_ES;R_AM;R_IM;R_NW]; 


% Size-adjusted power

powercrit=repmat(quantile(T_ES,.95), [N 1 1]);
J_ES=Z_ES>powercrit;          
powercrit=repmat(quantile(T_AM,.95), [N 1 1]);
J_AM=Z_AM>powercrit;          
powercrit=repmat(quantile(T_IM,.95), [N 1 1]);
J_IM=Z_IM>powercrit;   
powercrit=repmat(quantile(T_NW,.95), [N 1 1]);
J_NW=Z_NW>powercrit;       

P_ES=sum(J_ES)/N;
P_AM=sum(J_AM)/N;
P_IM=sum(J_IM)/N;
P_NW=sum(J_NW)/N;

Power=[P_ES;P_AM;P_IM;P_NW]; 


% Selected bandwidths

Selected_Bandwidths=([mean(Q_ES);mean(Q_AM);mean(Q_IM);mean(Q_NW)]);


% Population parameters for calculating bias and variance

SS=zeros(1,k);
SSNW=zeros(1,k);

for i=1:k
SS(1,i)=(1+(2*a)*((phi(i))/(1-phi(i))))*(1/(1-(phi(i)^2)));
SSNW(1,i)=(1+(2)*((phi(i))/(1-phi(i))))*(1/(1-(phi(i)^2)));
end


% Bias (reported as squared bias):

S_ES=mean(SS_ES.*M);
S_AM=mean(SS_AM.*M);
S_IM=mean(SS_IM.*M);
S_NW=mean(SS_NW.*repmat(T,[N k]));

B_ES=(S_ES-SS).^2;
B_AM=(S_AM-SS).^2;
B_IM=(S_IM-SS).^2;
B_NW=(S_NW-SSNW).^2;

Bias_Squared=[B_ES;B_AM;B_IM;B_NW];


% Variance:

V_ES=(1/N)*sum(((SS_ES.*M)-repmat(S_ES,[N 1])).^2);
V_AM=(1/N)*sum(((SS_AM.*M)-repmat(S_AM,[N 1])).^2);
V_IM=(1/N)*sum(((SS_IM.*M)-repmat(S_IM,[N 1])).^2);
V_NW=(1/N)*sum(((SS_NW.*repmat(T,[N k]))-repmat(S_NW,[N 1])).^2);

Variance=[V_ES;V_AM;V_IM;V_NW];


% Mean Squared Error:

M_ES=(1/N)*sum(((SS_ES.*M)-repmat(SS_star,[N 1])).^2);
M_AM=(1/N)*sum(((SS_AM.*M)-repmat(SS_star,[N 1])).^2);
M_IM=(1/N)*sum(((SS_IM.*M)-repmat(SS_star,[N 1])).^2);
M_NW=(1/N)*sum(((SS_NW.*repmat(T,[N k]))-repmat(SS,[N 1])).^2);

MSE=[M_ES;M_AM;M_IM;M_NW];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Display Results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Each results matrix has one column for each phi parameter, and rows
% represent ES, AM, IM, and NW (full sample) results.

Rejection_rates
Power
Selected_Bandwidths
Bias_Squared
Variance
MSE






















