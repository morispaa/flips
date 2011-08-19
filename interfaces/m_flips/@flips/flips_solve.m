function flips_solve(h,mode)
%FLIPS_SOLVE Solve the problem fed into FLIPS object
%
%    FLIPS_SOLVE(H,MODE) solves the linear inverse problem associated with
%    FLIPS object H. The MODE is one of the following character strings:
%        ''    Calculate only the solution
%        'r'   Calculate the solution and the residual
%        'rc'  Calculate the solution, the residual and the diagonal of the
%              posteriori covariance matrix
%        'rfc' Calculate the solution, the residual and the full posteriori
%              covariance matrix
%        'c'   Calculate the solution and the diagonal of the posteriori
%              covariance matrix
%        'fc'  Calculate the solution and the full posteriori covariance
%              matrix
%
%    The solution (and possibly the residual and the posteriori covariance
%    matrix) can be fetched by using <a href="matlab: help
%    flips_get">FLIPS_GET</a>.
%
%    Note that currently there is no checking that enough data is added to
%    the FLIPS object. 


hname=inputname(1);

switch lower(mode)
    
    case ''
        res = 0;
        cov = 0;
        fcov = 0;
    
    case 'r'
        res = 1;
        cov = 0;
        fcov = 0;
        
    case 'rc'
        res = 1;
        cov = 1;
        fcov = 0;
        
    case 'rfc'
        res = 1;
        cov = 1;
        fcov = 1;
        
    case 'c'
        res = 0;
        cov = 1;
        fcov = 0;
        
    case 'fc'
        res = 0;
        cov = 1;
        fcov = 1;
        
    otherwise
        error('Unknown mode!')
end

% Write namelist
wnamelist(h,1,res,cov,fcov,h.buffersize);

% Call flipseng
system(h.flipseng);

% Remove namelist
%delete('rflips.nml');

% Update variables
h.crows = h.crows + h.newrows;
h.newrows = 0;
if (h.crows > h.ncols) h.crows = h.ncols; end

h.append = 0;

h.r_written = 1;
h.sol_written = 1;

if (res==1) h.residual_written = 1; end
if (cov==1) h.cov_written = 1; end
if (fcov==1) h.cov_full = 1; end

% Read current band and common variable count
tmp = readbw(h);
h.band = tmp(1);
h.common = tmp(2);

% Read solution
h.solution = rbinfile(h.Sfile,h.ncols*h.nrhs,h.cplx,h.prec);
h.solution = reshape(h.solution,h.nrhs,h.ncols).';

% Read residual
if (res==1)
    h.residual = rbinfile(h.residfile,h.nrhs,0,h.prec);
end

% Read covariance
if (cov==1)
    if (fcov == 1)
        h.covariance = rbinfile(h.covfile,h.ncols*h.ncols,h.cplx,h.prec);
        h.covariance = reshape(h.covariance,h.ncols,h.ncols);
    else
        h.covariance = rbinfile(h.covfile,h.ncols,h.cplx,h.prec);
    end
end

assignin('caller',hname,h);


    
    



