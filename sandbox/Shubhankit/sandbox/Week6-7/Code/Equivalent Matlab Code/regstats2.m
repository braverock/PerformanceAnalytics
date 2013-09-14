function stats=regstats2(responses,data,model,whichstats)
%REGSTATS Regression diagnostics for linear models.
%   REGSTATS(RESPONSES,DATA,MODEL) regresses measurements in the n by m 
%   matrix RESPONSES on values in the matrix DATA using a multiple linear model.
%   If the RESPONSES is a matrix, the regstats2 iterates the regression 
%   analysis for each column in RESPONSES creating a multiple structure.
%   The function creates a UI that displays a group of checkboxes that 
%   save diagnostic statistics to the base workspace using specified
%   variable names. MODEL controls the order of the regression model.  By
%   default, REGSTATS uses a linear additive model with a constant term.
%
%   The optional input MODEL specifies how the design matrix is created 
%   from DATA.  The design matrix is the matrix of term values for each
%   observation.  MODEL can be any of the following strings:
%
%     'linear'        Constant and linear terms (the default)
%     'interaction'   Constant, linear, and interaction terms
%     'quadratic'     Constant, linear, interaction, and squared terms
%     'purequadratic' Constant, linear, and squared terms
%     'onlydata'      Leaves data as it is
%
%   Alternatively, MODEL can be a matrix of model terms accepted by the 
%   X2FX function.  See X2FX for a description of this matrix and for
%   a description of the order in which terms appear.  You can use this
%   matrix to specify other models including ones without a constant term.
%
%   STATS = REGSTATS(RESPONSES,DATA,MODEL,WHICHSTATS) creates an output
%   structure STATS containing the statistics listed in WHICHSTATS.
%   WHICHSTATS can be a single string such as 'leverage' or a cell array of
%   strings such as {'leverage' 'standres' 'studres'}.  By default, 
%   REGSTATS returns all statistics. Valid statistic strings are:
%
%      Name          Meaning
%      'Q'           Q from the QR Decomposition of the design matrix
%      'R'           R from the QR Decomposition of the design matrix
%      'beta'        Regression coefficients
%      'covb'        Covariance of regression coefficients
%      'yhat'        Fitted values of the response data
%      'r'           Residuals
%      'mse'         Mean squared error
%      'rsquare'     R-square statistic
%      'adjrsquare'  Adjusted R-square statistic
%      'leverage'    Leverage
%      'hatmat'      Hat (projection) matrix
%      's2_i'        Delete-1 variance
%      'beta_i'      Delete-1 coefficients
%      'standres'    Standardized residuals
%      'studres'     Studentized residuals
%      'dfbetas'     Scaled change in regression coefficients
%      'dffit'       Change in fitted values
%      'dffits'      Scaled change in fitted values
%      'covratio'    Change in covariance
%      'cookd'       Cook's distance
%      'tstat'       t statistics for coefficients
%      'fstat'       F statistic
%      'dwstat'      Durbin Watson statistic
%      'hc0'         White robust t statistics (Eicker, 1963, 1967; Huber, 1967; White, 1980)
%      'hc1'         White with dfe correction (Hinkley, 1977)
%      'hc2'         White weighted by 1-h (MacKinnon & White, 1985)
%      'hc3'         White weighted by (1-h)^2 (Davidson & MacKinnon, 1993)
%      'hc4'         White weighted by (1-h)^delta (Cribari & Neto, 2004)
%      'hac'         Newey West t statistics (Newey & West, 1987)
%      'qlike'       Quasi-likelyhood loss function (Patton, 2011b)
%      'all'         Create all of the above statistics
%      'empty'       True if the series is all NaN
%      'rankdef'     True if the series is rank deficient
%
%   NOTE: When the RESPONSES is an n by m matrix, the regression is performed
%   for each column of the RESPONSES on the design matrix. When there are no 
%   valid observations at all, the 'empty' field will be true. When the valid
%   observations are less than the number of the regressors in DATA, then both
%   'rankdef' is true. For both the cases the regression for that column of 
%   RESPONSES is skipped. 
%   An observation is valid when all(~isnan([responses(obs,j) data(obs,:)])).
%
%   NOTE: The F statistic and its p-value are computed under the assumption
%   that the model contains a constant term, and they are not correct for
%   models without a constant.  The R-square value is one minus the ratio
%   of the error sum of squares to the total sum of squares.  This value 
%   can be negative for models without a constant, which indicates that the
%   model is not appropriate for the data.
%
%   Example:  Plot residuals vs. fitted values for Hald data.
%      load hald
%      s = regstats2(heat,ingredients,'linear',{'yhat','r'});
%      scatter(s.yhat,s.r)
%      xlabel('Fitted Values'); ylabel('Residuals');
%
% Additional features:
% - <a href="matlab: web('http://www.mathworks.com/matlabcentral/fileexchange/26169-regstats2','-browser')">FEX regstats2 page</a>
% - <a href="matlab: web('http://www.mathworks.com/matlabcentral/fileexchange/24722-heteroskedasticity-test','-browser')">FEX TestHet page</a>
%   See also X2FX, REGRESS, STEPWISE, LEVERAGE.

%   References:
%   Belsley, D.A., E. Kuh, and R.E. Welsch (1980), Regression
%      Diagnostics, New York: Wiley.
%   Cook, R.D., and S. Weisberg (1982), Residuals and Influence
%      in Regression, New York: Wiley.
%   Goodall, C.R. (1993), Computation using the QR decomposition.
%      Handbook in Statistics, Volume 9,  Statistical Computing
%      (C. R. Rao, ed.), Amsterdam, NL: Elsevier/North-Holland.

%   Copyright 1993-2006 The MathWorks, Inc.
%   $Revision: 2.27.4.10 $  $Date: 2009/05/07 18:32:03 $
%   $Modified by Oleg K. $  $Date: 2009/12/22 $ Added additional stats and looping functionality
%   $Modified by Oleg K. $  $Date: 2010/01/05 $ Added link to FEX submission
%   $Modified by Oleg K. $  $Date: 2010/01/15 $ Skips all NaN series and rankdeficient 
%   $Modified by Oleg K. $  $Date: 2010/02/05/$ Per Dajun Tuo suggestion: fixed assignment between dissimilar structures
%   $Modified by Oleg K. $  $Date: 2010/02/10/$ Per Sebastian Eisenbach suggestion: empty and rankdef fields are now 'visible'. Bug in hc3 and hc4 if called together or with hc2. Edited help.
%   $Modified by Oleg K. $  $Date: 2010/02/15/$ Per Nick Baltas suggestion: added 'onlydata' model which leaves data as it is. Added warning in case R^2 or F stat is requested under this model.
%   $Modified by Oleg K. $  $Date: 2011/09/15/$ Added QLIKE loss function


if (nargout>0 || nargin>=4)
    action = 'batch';
else
    action = 'start';
end

varnames = {'Q','R','beta','covb','yhat','r','mse','rsquare','adjrsquare', ...
            'leverage','hatmat','s2_i','beta_i','standres','studres', ...
            'dfbetas','dffit','dffits','covratio','cookd','tstat','fstat','dwstat',...
...         % Oleg K. 2009 11 24: Added fields
...         %------------------------------------------------------
            'hc0','hc1','hc2','hc3','hc4','hac','qlike','empty','rankdef'};
            %------------------------------------------------------
        
if (nargin<2)
   error('stats:regstats:TooFewInputs','At least two arguments are required.');
end
if nargin < 3 || isempty(model)
   model = 'linear';
end
if nargin < 4 || isempty(whichstats)
   whichstats = 'all';
end

% Check that the arguments are as expected, remove NaN.
[xr,xc] = size(data);
[yr,yc] = size(responses);
if (yr == 1), responses = responses'; yr = yc; yc = 1; end
if (xr == 1), data = data';           xr = xc; end
if (yr ~= xr)
   error('stats:regstats:InputSizeMismatch',...
         'RESPONSES and DATA must have the same number of rows.');
end

% Oleg K. 2009 11 24: to allow loops for each column of responses
% if (yc > 1)
%    error('stats:regstats:InvalidData','RESPONSES must have a single column.');
% end
% wasnan = isnan(responses) | any(isnan(data),2);
% havenans = any(wasnan);
% if havenans
%    responses(wasnan) = [];
%    data(wasnan,:) = [];
% end

% Oleg K. 2010 02 13: allow regression on the raw predictors 
% ----------------------------------------------------------
if strcmp(model,'onlydata')
    X = data;
else
    X = x2fx(data,model);
end
% ----------------------------------------------------------
y = responses;   
if size(X,1) < size(X,2)
    error('stats:regstats:RankDefDesignMat', ...
          'The design matrix has more predictor variables than observations.');
end

% Bring up "Export to Workspace" Dialog
if strcmp(action,'start')
    labels = {'Q from QR Decomposition', 'R from QR Decomposition', ...
              'Coefficients', 'Coefficient Covariance', 'Fitted Values', ...
              'Residuals', 'Mean Square Error', 'R-square Statistic', ...
              'Adjusted R-square Statistic', 'Leverage', 'Hat Matrix', ...
              'Delete-1 Variance', 'Delete-1 Coefficients', ...
              'Standardized Residuals', 'Studentized Residuals', ...
              'Change in Beta', 'Change in Fitted Value', ...
              'Scaled Change in Fit', 'Change in Covariance', ...
              'Cook''s Distance', 't Statistics', 'F Statistic','DW Statistic',...
...           % Oleg K. 2009 11 24: Added fields
...           %------------------------------------------------------------------------------------------
              'White robust t Statistics','White with dfe correction','White weighted 1-h',...
              'White weighted (1-h)^2', 'White weighted (1-h)^delta','Newey West robust t Statistics',...
              'Quasi-likelihood','All NaNs series','Rank deficient series'};
              %------------------------------------------------------------------------------------------
              
    % Calculate all statistics
    s = regstats2(responses,data,model, 'all');
   
    % Oleg K. 2009 11 24: items grouping changes a bit if more than one series
    %----------------------------------------------------------------------  
    if yc > 1
        % Note that substructures size can be < yc because of empty | rankdef
        idx = ~([s.empty] | [s.rankdef]);
        items = {{s.Q},{s.R},{s.beta},{s.covb},{s.yhat},{s.r},{s.mse},{s.rsquare},...
                 {s.adjrsquare},{s.leverage},{s.hatmat},{s.s2_i},{s.beta_i},{s.standres},...
                 {s.studres},{s.dfbetas},{s.dffit},{s.dffits},{s.covratio},{s.cookd},...
                 [s(idx).tstat],[s(idx).fstat],[s(idx).dwstat],[s(idx).hc0],...
                 [s(idx).hc1],[s(idx).hc2],[s(idx).hc3],[s(idx).hc4],[s(idx).hac],...
                 {s.qlike},[s.empty],[s.rankdef]};
    else
        items = {s.Q,s.R,s.beta,s.covb,s.yhat,s.r,s.mse,s.rsquare,...
                 s.adjrsquare,s.leverage,s.hatmat,s.s2_i,s.beta_i,s.standres,...
                 s.studres,s.dfbetas,s.dffit,s.dffits,s.covratio,s.cookd,s.tstat,...
                 s.fstat,s.dwstat,s.hc0,s.hc1,s.hc2,s.hc3,s.hc4,s.hac,s.qlike,s.empty,s.rankdef};
    end
    %----------------------------------------------------------------------
    
    fh = @helpCallback;
    wintitle = 'Regstats Export to Workspace';
    hdialog = export2wsdlg(labels, varnames, items, wintitle, ...
                           false(1, length(varnames)), {fh});
    set(hdialog,'WindowStyle','normal');
else %  action = 'batch'
  if ~iscell(whichstats), whichstats = {whichstats}; end
  if ~iscellstr(whichstats)
     error('stats:regstats:BadStats',...
           'WHICHSTATS argument must be one or more statistic names.');
  end
  idx = false(length(varnames),1);
  for j=1:length(whichstats)
     snj = whichstats{j};
     if isequal(snj,'all')
        idx(:) = true;
        break;
     else
        k = find(strcmp(snj,varnames));
        if isempty(k)
           error('stats:regstats:BadStats',...
                 'Invalid statistic name ''%s''.',snj);
        else
           idx(k) = true;
        end
     end
  end
  if ~any(idx), return, end
  
% Oleg K. 2010 02 13: warn if F statistics and R^2 in case of 'onlydata' model
% ----------------------------------------------------------------------------
if strcmp(model,'onlydata') && any(idx([22, 8]))
    warning('stats:regstats2:NoConst',...
            ['R-square and the F statistic are not well-defined under "onlydata" MODEL\n'...
             'unless DATA contains a column of ones (intercept), in which case is\n'...
             'recommended to use the "linear" MODEL.']);
end
% ----------------------------------------------------------------------------

% Oleg K. 2009 11 24: allowing loop with NaN removal
%----------------------------------------------------------------  
Y = y; x = X; 
allSeriesStats(1:yc) = cell2struct(num2cell(false(nnz(idx),1)),varnames(idx)); 
% [1] LOOP for series
for ser = 1:yc
 y = Y(:,ser);
 X = x;
 wasnan = isnan(y) | any(isnan(X),2);
 % [2] CONDITION of non NaN and not Rank deficient
 if all(wasnan)
     if idx(30)
        allSeriesStats(ser).empty = true;
     end
 elseif nnz(~wasnan) < xc + 1 % Skip if rank deficient 
     if idx(31)
         allSeriesStats(ser).rankdef = true;
     end
 else %...continue with computations
    havenans = any(wasnan);
 if havenans
    y(wasnan) = [];
    X(wasnan,:) = [];
 end
%----------------------------------------------------------------  
  
  % This is not a rank-revealing QR.  p>n has already been weeded out, but X
  % may still be rank deficient, and that is not explicitly detected.  In such
  % cases, the computation of beta and inv(R) will warn.
  [Q,R] = qr(X,0);
  beta = R\(Q'*y);
  yhat = X*beta;
  residuals = y - yhat;  
  nobs = length(y);
  p = length(beta);
  dfe = nobs-p;
  dft = nobs-1;
  ybar = mean(y);
  sse = norm(residuals)^2;    % sum of squared errors
  ssr = norm(yhat - ybar)^2;  % regression sum of squares
  sst = norm(y - ybar)^2;     % total sum of squares;
  mse = sse./dfe;
  h = sum(abs(Q).^2,2);
  s_sqr_i = (dfe*mse - abs(residuals).^2./(1-h))./(dfe-1);
  e_i = residuals./sqrt(s_sqr_i.*(1-h));
  ri = R\eye(p);
  xtxi = ri*ri';
  covb = xtxi*mse;

  % Do one preliminary calculation
  if idx(13) || idx(16)
     % Delete 1 coefficients. BETA_I
     stde = residuals./(1-h);
     stde = stde(:,ones(p,1));
     b_i = beta(:,ones(nobs,1)) - ri*(Q.*stde)';
  end

  % Store each requested statistic into the structure
  % stats.source = 'regstats';
  if idx(1)  % Q from the QR decomposition of the X matrix.
	 stats.(varnames{1}) = Q;
  end
  if idx(2)  % R from the QR decomposition of the X matrix.
     stats.(varnames{2}) = R;
  end
  if idx(3)  % Coefficients.
     stats.(varnames{3}) = beta;
  end
  if idx(4)   % Covariance of the parameters.
     stats.(varnames{4}) = covb;
  end
  if idx(5)  % Fitted values.
     tmp = yhat;
     if havenans, tmp = fixrows(tmp, wasnan); end
     stats.(varnames{5}) = tmp;
  end
  if idx(6)  % Residuals.
     tmp = residuals;
     if havenans, tmp = fixrows(tmp, wasnan); end
     stats.(varnames{6}) = tmp;
  end
  if idx(7)  % Mean squared error.
     stats.(varnames{7}) = mse;
  end
  if idx(8)  % R-square.
     % There are several ways to compute R^2, all equivalent for a linear
     % model where X includes a constant term, but not equivalent
     % otherwise.  R^2 can be negative for models without an intercept.
     % This indicates that the model is inappropriate.
     stats.(varnames{8}) = 1 - sse ./ sst;
  end
  if idx(9)  % Adjusted R-square.
     stats.(varnames{9}) = 1 - (sse./sst)*(dft./dfe);
  end
  if idx(10)  % Leverage.
     tmp = h;
     if havenans, tmp = fixrows(tmp, wasnan); end
     stats.(varnames{10}) = tmp;
  end
  if idx(11)  % Hat Matrix.
     H = Q*Q';
     if havenans
        tmp = zeros(length(wasnan));
        tmp(~wasnan,~wasnan) = H;
        tmp(wasnan,wasnan) = diag(NaN(sum(wasnan),1));
        H = tmp;
     end
     stats.(varnames{11}) = H;
  end
  if idx(12) % Delete 1 variance. S_I
     tmp = s_sqr_i;
     if havenans, tmp = fixrows(tmp, wasnan); end
     stats.(varnames{12}) = tmp;
  end
  if idx(13) % Delete 1 coefficients. BETA_I
     tmp = b_i;
     if havenans
        % Estimates would be same if missing observations left out
        tmp = zeros(p,length(wasnan));
        tmp(:,~wasnan) = b_i;
        tmp(:,wasnan) = beta(:,ones(sum(wasnan),1));
     end
     stats.(varnames{13}) = tmp;
  end
  if idx(14) % Standardized residuals.
     standr = residuals./(sqrt(mse*(1-h)));
     if havenans, standr = fixrows(standr, wasnan); end
     stats.(varnames{14}) = standr;
  end
  if idx(15) % Studentized residuals.
     tmp = e_i;
     if havenans, tmp = fixrows(tmp, wasnan); end
     stats.(varnames{15}) = tmp;
  end
  if idx(16) % Scaled change in beta. DFBETAS
     b = beta(:,ones(nobs,1));
     s = sqrt(s_sqr_i(:,ones(p,1))');
     rtri = sqrt(diag(xtxi));
     rtri = rtri(:,ones(nobs,1));
     dfbeta = (b - b_i)./(s.*rtri);
     if havenans
        % Zero change in estimates if missing observations left out
        tmp = zeros(p,length(wasnan));
        tmp(:,~wasnan) = dfbeta;
        dfbeta = tmp;
     end
     stats.(varnames{16}) = dfbeta;
  end
  if idx(17) % Change in fitted values. DFFIT
     dffit = h.*residuals./(1-h);
     if havenans, dffit = fixrows(dffit, wasnan); end
     stats.(varnames{17}) = dffit;
  end
  if idx(18) % Scaled change in fitted values. DFFITS
     dffits = sqrt(h./(1-h)).*e_i;
     if havenans, dffits = fixrows(dffits, wasnan); end
     stats.(varnames{18}) = dffits;
  end
  if idx(19) %  Change in covariance. COVRATIO
     covr = 1 ./((((nobs-p-1+abs(e_i).^2)./(nobs-p)).^p).*(1-h));
     if havenans, covr = fixrows(covr, wasnan); end
     stats.(varnames{19}) = covr;
  end
  if idx(20) %  Cook's Distance.
     d = abs(residuals).^2 .* (h./(1-h).^2)./(p*mse);
     if havenans, d = fixrows(d, wasnan); end
     stats.(varnames{20}) = d;
  end
  if idx(21) %  t Statistics.
     d = struct;
     d.beta = beta;
     d.se = sqrt(diag(covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     d.dfe = dfe;
     stats.(varnames{21}) = d;
  end
  if idx(22) %  F Statistic.
     d = struct;
     d.sse = sse;
     d.dfe = dfe;
     d.dfr = p-1;
     d.ssr = ssr;
     d.f = (d.ssr/d.dfr)/(d.sse/d.dfe);
     d.pval = 1 - fcdf(d.f, d.dfr, d.dfe);
     stats.(varnames{22}) = d;
  end
  if idx(23) % Durbin Watson Statistic.
     [pvalue, dw]=dwtest(residuals,X);
     d = struct;
     d.dw = dw;
     d.pval = pvalue;
     stats.(varnames{23}) = d;
  end
  % Oleg K. (Added HC0, HC1, HC2, HC3, HC4 and HAC statistics, QLIKE)
  % -----------------------------------------------------------------
  if idx(24) % HC0
     hhat = repmat(residuals',p,1).*X';
     xuux = hhat*hhat';
     d = struct;
     d.covb = xtxi*xuux*xtxi;
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{24}) = d;
  end
  if idx(25) % HC1
     d = struct;
     if idx(24)
        d.covb = stats.hc0.covb*nobs/dfe;
     else
        hhat = repmat(residuals',p,1).*X';
        xuux = hhat*hhat';
        d.covb = xtxi*xuux*xtxi*nobs/dfe;
     end
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{25}) = d;
  end
  if idx(26) % HC2
     tmp = 1-h;
     xuux = zeros(p,p);
     for ii = 1:nobs;
        xuux = xuux + residuals(ii)^2/tmp(ii)*X(ii,:)'*X(ii,:);
     end
     d = struct;
     d.covb = xtxi*xuux*xtxi;
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{26}) = d;
  end
  if idx(27) % HC3
     tmp = (1-h).^2;
     d = struct;
     xuux = zeros(p,p);
     for ii = 1:nobs;
        xuux = xuux + residuals(ii)^2/tmp(ii)*X(ii,:)'*X(ii,:);
     end
     d.covb = xtxi*xuux*xtxi;
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{27}) = d;
  end
   if idx(28) % HC4
     if havenans
         if idx(10); h = stats.leverage;
         else h = fixrows(h, wasnan); 
         end
     end
     tmp = (1-h).^(min(4,h/mean(h)));
     xuux = zeros(p,p);
     for ii = 1:nobs;
        xuux = xuux + residuals(ii)^2/tmp(ii)*X(ii,:)'*X(ii,:);
     end
     d = struct;
     d.covb = xtxi*xuux*xtxi;
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{28}) = d;
   end
   if idx(29) % HAC (Newey West)
     L = round(4*(nobs/100)^(2/9));
     % L = nobs^.25; % as an alternative
     hhat = repmat(residuals',p,1).*X';
     xuux = hhat*hhat';
     for l = 1:L;
        za = hhat(:,(l+1):nobs)*hhat(:,1:nobs-l)';
        w = 1 - l/(L+1);
        xuux = xuux + w*(za+za');
     end
     d = struct;
     d.covb = xtxi*xuux*xtxi;
     d.se = sqrt(diag(d.covb));
     d.t = beta./d.se;
     d.pval = 2*(tcdf(-abs(d.t), dfe));
     stats.(varnames{29}) = d;
   end
   if idx(30)  % QLIKE Quasi-likelihood
     stats.(varnames{30}) = sum(log(yhat)+y./yhat)/dfe;
   end

   if idx(31) % All NaNs series
      stats.empty = false;
   end
   if idx(32) % Rank deficient series
      stats.rankdef = false;
   end
   allSeriesStats(ser) = stats;  
end % [2] CONDITION non NaN

end % [1] LOOP for series
stats = allSeriesStats;
end 
%----------------------------------------------------------------------------

function helpCallback
% display help
doc regstats

%----------------------------------------------------------------------------
function vv = fixrows(v, b)
% helper to extend v to original length, NaNs are given by b

vv = NaN(size(b));
vv(~b) = v;
