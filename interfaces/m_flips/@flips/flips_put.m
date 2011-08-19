function flips_put(h,name,data)
%FLIPS_PUT Manually insert target matrix or target vector into FLIPS
%
%    FLIPS_PUT(H,NAME,DATA) inserts target matrix or target vector into
%    a existing FLIPS problem.
%    H must be a initialized FLIPS object.
%    NAME is either 'Rmat' for target matrix or 'Ymat' for target vector.
%    DATA is the numerical values of target matrix or target vector. It
%    can be given as vector (target vector in row-major order, for target 
%    matrix R only the upper triagonal part in row-major order) or as
%    matrix. 
%    

hname = inputname(1);

name = lower(name(isletter(name)));

switch name(1:3)
    
    case 'rma'
        
    %    % Check arguments
    %    s = size(data);
    %    n = h.ncols;
    %    
    %    % R given as vector
    %    if (isvector(data))
    %        if (s ~= n*(n+1)/2)
    %            error('Target matrix R has wrong size!');
    %        end
    %        R = data;
    %    else
    %        if (~all([n n] == s))
    %            error('Target matrix R has wrong size!');
    %        end
    %        
    %        % Reshape as a vector
    %        R = zeros(1,n*(n+1)/2);
    %        for i = j:n,
    %            rst = j + (j-1)*(2*h.ncols -j)/2;
    %            ren = h.ncols + (j-1)*(2*h.ncols-j)/2;
    %            
    %            R(rst:ren) = data(j,j:n);
    %        end
    %    end
    %    
    %    wbinfile(h.Rfile,R,h.cplx,0,h.prec);
    %    h.r_written = 1;
    error('R matrix insertion is no longer supported!')

           
        
        
    case 'yma'
        
        % Check arguments
        s = size(data);
        n = h.ncols;
        m = h.nrhs;
        
        % Y given as a vector
        if (isvector(data))
            if (s ~= n*m)
                error('Target matrix R has wrong size!');
            end
            
            Y = data;
            
        else
            if (~all([n m]==s))
                error('Target matrix R has wrong size!');
            end
            
            Y = reshape(data',1,n*m);
            
        end
        
        wbinfile(h.Yfile,Y,h.cplx,0,h.prec);
                
    otherwise
        warning(['Unknown or unimplemented parameter name: ' name])
        
end

