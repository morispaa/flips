function flips_init(h,varargin)
%FLIPS_INIT Initialize FLIPS object
%
%    FLIPS_INIT(H,NCOLS,NRHS,TYPE) initializes a FLIPS object H to have
%    NCOLS unknowns (the number of columns in the theory matrix) and NRHS
%    columns (number of right hand sides) in the measurement matrix. The
%    TYPE must be one of the following:
%        's' for single precision real problem
%        'd' for double precision real problem
%        'c' for single precision complex problem
%        'z' for double precision complex problem
%
%    FLIPS_INIT(H,NCOLS,NRHS,TYPE,COMMON) initializes a FLIPS object H with
%    COMMON common variables. Common variables must be the last ones
%    (indexwise).
%
%    FLIPS_INIT(H,NCOLS,NRHS,TYPE,COMMON,BAND) initializes a FLIPS object H
%    with band storege system disabled, if BAND is equal 0. This is 
%    normally not needed and is here mainly for bebugging and testing
%    purposes.
%    
%
%    Example: Initialize a single precision real problem with 1000 unknowns
%        and one right hand side.
%
%    % New FLIPS object
%    h = flips;
%
%    flips_init(h,1000,1,'s');
%



hname=inputname(1);

% Check arguments

nvarg = size(varargin,2);

if (nvarg<3 || nvarg>5)
    error('Wrong number of arguments!')
elseif (nvarg==3)
    ncols = varargin{1};
    nrhs = varargin{2};
    type = varargin{3};
    common = 0;
    band = 1;
elseif (nvarg==4)
    ncols = varargin{1};
    nrhs = varargin{2};
    type = varargin{3};
    common = varargin{4};
    band = 1;
elseif (nvarg==5)
    ncols = varargin{1};
    nrhs = varargin{2};
    type = varargin{3};
    common = varargin{4};
    band = varargin{5};
end

% ncols and nrhs must be integers
 
 if(ncols ~= uint32(ncols))
     error('ncols must be an integer!')
 end
 
 if(nrhs ~= uint32(nrhs))
     error('nrhs must be an integer!')
 end

% type must be a character string
if(~ischar(type))
    error('type must be either ''s'', ''d'', ''c'' or ''z''!')
end

% common and band must be integers
if(common~=uint32(common))
    error('common must be an integer!')
end

if (band~=uint32(band))
    error('band must be an integer')
end

h.name = hname;
h.ncols = ncols;
h.nrhs = nrhs;
h.band = band;
h.common = common;

 switch lower(type)
     
     case 's'
         h.dbl=0;
         h.cplx=0;
         h.prec='float32';
     case 'd'
         h.dbl=1;
         h.cplx=0;
         h.prec='float64';
     case 'c'
         h.dbl=0;
         h.cplx=1;
         h.prec='float32';
     case 'z'
         h.dbl=1;
         h.cplx=1;
         h.prec='float64';
     otherwise
         error('Unknown type! Must be either ''s'', ''d'', ''c'' or ''z''!')
 end

h.type = type;

% Get the path to flipseng
h.flipseng = flipspath();


% Initialize internal variables

h.crows = 0;
h.newrows = 0;
h.append = 0;

h.r_written = 0;
h.sol_written =0;
h.rinv_written = 0;
h.residual_written = 0;
h.cov_written = 0;
h.cov_full = 0;
h.n_addvar = 0;
h.n_delvar = 0;
h.solution = [];
h.residual = [];
h.covariance = [];
h.buffersize = 100;

% Filenames
dirname = sprintf('FLIPS_%s',hname);
[succ,msg,mid] = mkdir(dirname);
if (succ == 0)
    error(['FLIPS_INIT: Unable to create directory ' dirname '!'])
end

fpath = fullfile(pwd,dirname,'');

h.Rfile = fullfile(fpath,'R.dat');
h.Yfile = fullfile(fpath,'Y.dat');
h.Sfile = fullfile(fpath,'sol.dat');
h.residfile = fullfile(fpath,'resid.dat');
h.covfile = fullfile(fpath,'cov.dat');
h.rinvfile = fullfile(fpath,'rinv.dat');
h.Afile = fullfile(fpath,'A.dat');
h.Mfile = fullfile(fpath,'M.dat');
h.Efile = fullfile(fpath,'E.dat');
h.rmaskfile = fullfile(fpath,'rmask.dat');
h.bwfile = fullfile(fpath,'cbw.dat');
h.cvfile = fullfile(fpath,'cv.dat');
    
assignin('caller',hname,h);