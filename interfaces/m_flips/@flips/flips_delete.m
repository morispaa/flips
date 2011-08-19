function flips_delete(h,varargin)
%FLIPS_DELETE Delete data rows
%
%    FLIPS_DELETE(H,ADATA,MDATA,EDATA) deletes the given data from
%    the FLIPS object H. The vectors/matrices ADATA, MDATA and EDATA
%    are as in FLIPS_ADD.
%
%    Nota bene: This is an experimental feature. Known issues and 
%    limitations:
%
%       1) Works only for real problems;
%       2) Data can only be deleted from an over determined problems and
%          the problem must not become under determined after the deletion
%          of data rows.
%       3) It is only safe to delete the exactly same data that has
%          previously put in the FLIPS object. Any other data may (or may
%          not) blow up the problem. There is currently  no way of 
%          checking this.
%
%    See also FLIPS_ADD.
%
%    Example:
%    
%    % Create and initialize FLIPS object (double precision real)
%    h = flips;
%    flips_init(h,100,1,'d')
%
%    % Create random data
%    % Over determined theory matrix
%    A = randn(200,100);
%
%    % Measurement
%    m = randn(200,1);
%
%    % Add the data 
%    flips_add(h,A,m);
%
%    % Delete first 50 rows 
%    flips_delete(h,A(1:50,1:100),m(1:50));
%    
%    % Solve problem and get solution, residual and posteriori covariance
%    flips_solve(h,'rfc');
%    [sol,res,cov] = flips_get(h,'sol','res','cov');

hname=inputname(1);

% Check number of arguments

narg=size(varargin,2);

if (narg == 2)
    adata = varargin{1};
    mdata = varargin{2};
    edata = 1;
elseif (narg==3)
    adata = varargin{1};
    mdata = varargin{2};
    edata = varargin{3};
else
    error('Wrong number of input arguments!');
end

% First of all, if there are rows that are not rotated in, rotate them now!
if (h.newrows > 0)
    flips_rotate(h);
end


% Send the data to FLIPS_ADD, so they will be written into files 
flips_add(h,adata,mdata,edata);
h.append = 0;

% Write namelist
wnamelist(h,4,0,0,0,h.buffersize);

% Call flipseng
system(h.flipseng);

% Remove old data files
D=ls;
if (strfind(D,h.Afile) ~= [])
    delete(h.Afile);
end

if (strfind(D,h.Mfile) ~= [])
    delete(h.Mfile);
end

if (strfind(D,h.Efile) ~= [])
    delete(h.Efile);
end


% Update internal variables
h.append = 0;
h.crows = h.crows - h.newrows;
if (h.crows > h.ncols) h.crows = h.ncols; end

h.newrows = 0;

h.sol_written = 0;
h.r_written = 1;
h.residual_written = 0;
h.cov_written = 0;
h.cov_full = 0;
h.rinv_written = 0;

% Read current band and common variable count
tmp = readbw(h);
h.band = tmp(1);
h.common = tmp(2);



assignin('caller',hname,h);


    
    

