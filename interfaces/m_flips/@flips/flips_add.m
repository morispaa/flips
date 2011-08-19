function flips_add(h,varargin)
%FLIPS_ADD Add datarows into FLIPS object
%
%    FLIPS_ADD(H,ADATA,MDATA,EDATA) adds new data into FLIPS object. ADATA
%    contains the theory matrix rows, MDATA the measurements and EDATA
%    errors. Data can be given as a matrix or as a vector. The error EDATA
%    can either be omitted, given as a vector (diagonal values of the error
%    covariance matrix) or as a full error covariance matrix. The sizes of
%    the vectors/matrices must agree with the FLIPS object H.
%
%    If the error is given as a full error covariance matrix, all the data
%    must be fed to FLIPS at one time, i.e. feeding FLIPS row-by-row is not
%    allowed. Also, the error covariance matrix must be positive definite
%    and symmetric. 
%
%    Example 1: Solve a random (over determined) problem with 100 unknowns, 
%    200 measurements and a known standard deviation (error) of the 
%    measurements:
%
%    % Create and initialize FLIPS object (double precision real)
%    h = flips;
%    flips_init(h,100,1,'d')
%
%    % Create random data
%    % Theory matrix
%    A = randn(200,100);
%
%    % Measurement
%    m = randn(200,1);
%
%    % Error (stand. deviation 0.5...1.5)
%    E = 0.5 + rand(200,1);
%
%    % Add the first 100 data rows as a matrix
%    flips_add(h,A(1:100,:),m(1:100),E(1:100));
%
%    % Add the rest 100 rows row-by-row
%    for i = 101:200,
%        flips_add(h,A(i,:),m(i),E(i));
%    end
%
%    % Solve problem and get solution, residual and posteriori covariance
%    flips_solve(h,'rfc');
%    [sol,res,cov] = flips_get(h,'sol','res','cov');




hname=inputname(1);

% Check number of arguments

narg=size(varargin,2);

if (narg == 2)
    Adata = varargin{1};
    Mdata = varargin{2};
    Edata = 1;
elseif (narg==3)
    Adata = varargin{1};
    Mdata = varargin{2};
    Edata = varargin{3};
else
    error('Wrong number of input arguments!');
end

% Check input arquments

% is error given as a matrix?
if (~isvector(Edata))
    Emat = 1;
else
    Emat = 0;
end


% theory matrix
if (isvector(Adata))
    if( length(Adata)/h.ncols ~= uint32(length(Adata)/h.ncols))
        error('theory matrix has wrong size!')
    else
        nrows = length(Adata)/h.ncols;
    end
    
    % reshape into matrix
    Adata = reshape(Adata,h.ncols,nrows).';
    
else
    if (size(Adata,2) ~= h.ncols)
        error('theory matrix has wrong number of columns!')
    else
        nrows = size(Adata,1);
    end
    
end

% measurement
if (isvector(Mdata))
    if (length(Mdata) ~= h.nrhs * nrows)
        error('measurement has wrong size!')
    else
        % reshape as column vector
        Mdata = reshape(Mdata,h.nrhs,nrows).';
    end
else
    if (~all([nrows h.nrhs]==size(Mdata)))
        error('measurement matrix has wrong shape!')
    end
    
end

% error
if (Emat)
    if ( ~all([nrows nrows]==size(Edata)))
        error('error covariance matrix has wrong size!')
    else
        C=chol(Edata)';
        Adata = C\Adata;
        Mdata = C\Mdata;
        Edata = ones(nrows,1);
    end
else
    if (prod(size(Edata))==1)
        tmp = Edata;
        Edata = tmp*ones(nrows,1);
    else
        if (length(Edata) ~= nrows)
            error('error vector has wrong length!')
        else
            Edata = reshape(Edata,nrows,1);
        end
    end
end
        

% All OK. Write data into binary files and update h.


if (h.append==0)
    % New data. Overwrite old files
    % Solution is no longer valid.
    h.append = 1;
    h.sol_written = 0;
    h.newrows = h.newrows + nrows;
    
    wbinfile(h.Afile,Adata.',h.cplx,0,h.prec);
    wbinfile(h.Mfile,Mdata.',h.cplx,0,h.prec);
    wbinfile(h.Efile,Edata.',0,0,h.prec);
else
    % Append to old data.
    
    h.newrows = h.newrows + nrows;
    
    wbinfile(h.Afile,Adata.',h.cplx,1,h.prec);
    wbinfile(h.Mfile,Mdata.',h.cplx,1,h.prec);
    wbinfile(h.Efile,Edata.',0,1,h.prec);
end

assignin('caller',hname,h);


    
    

