function flips_add_fc(h,nrows,Adata,Mdata,EC)
%FLIPS_ADD_FC Add data rows and full error covariance matrix into FLIPS object 

% Adds data to FLIPS with full error covariance matrix
% We embed the errors into the data and call the usual flips_add.

% Check the size of EC
ECsize = size(EC);

if (ECsize(1) ~= ECsize(2))
    error('Error covariance must be square matrix!');
end

if (nrows ~= ECsize(1))
    error('Error covariance matrix is of wrong size!');
end

Asize = size(Adata);
Msize = size(Mdata);

if(prod(Asize) ~= nrows*h.ncols)
    error('Adata is of wrong size!');
end

if (prod(Msize) ~= nrows*h.nrhs)
    error('Mdata is of wrong size!');
end


if (isvector(Adata))
    Adata = reshape(Adata,h.ncols,nrows).';
end

if (isvector(Mdata))
    Mdata = reshape(Mdata,h.nrhs,nrows).';
end

C = chol(EC);
C = inv(C);

A = C * Adata;
M = C * Mdata;

flips_add(h,nrows,A,M);