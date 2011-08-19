function varargout = flips_get(h,varargin)
%FLIPS_GET Get FLIPS object parameters.
%
%    [A,B,C,...] = FLIPS_GET(H,'par1','par2','par3',...) fetches parameters
%    and variables from FLIPS object H. 
%
%    Currently, the following parameters are supported:
%
%        'sol<ution>' returns the solution to the problem
%        'res<idual>' returns the residual of the problem
%        'cov<variance> ' returns the posteriori covariance matrix (full
%            or diagonal depending how <a href="matlab: help flips_solve">flips_solve</a> was called)
%
%        'ncol<s>' returns the number of columns in the theory matrix
%        'nrh<s>' returns the number of right hand sides in the problem
%        'crow<s>' returns the number of data rows feeded into the problem
%        'new<rows>' returns the number of unrotated data rows in the
%            problem
%
%    S = FLIPS_GET(H,'all') returns the whole FLIPS object H as a
%    structure S. Note that structure S is not a valid FLIPS object.


if (nargout+(nargout==0)) ~= nargin-1
    error('No. of outputs must equal the no. of inputs.')
end

for i = 1:nargin-1
    
    name = varargin{i};
    
    if (~ischar(name))
        error('Parameters must be strings.')
    end
    
    name = lower(name(isletter(name)));
    
    switch name(1:3)
        
        case 'sol'
            varargout{i} = h.solution;
            
        case 'cov'
            varargout{i} = h.covariance;
            
        case 'res'
            varargout{i} = h.residual;
            
        case 'ncol'
            varargout{i} = h.ncols;
            
        case 'nrh'
            varargout{i} = h.nrhs;
            
        case 'cro'
            varargout{i} = h.crows;
            
        case 'new'
            varargout{i} = h.newrows;
            
        case 'all'
            varargout{i} = struct(h);
            
        % Additions 080121 start here -----
        
        case 'rma'
          %  R = rbinfile(h.Rfile,h.ncols*(h.ncols+1)/2,h.cplx,h.prec);
          %  RR = zeros(h.ncols);
          %  
          %  for j = 1:h.ncols,
          %      rst = j + (j-1)*(2*h.ncols -j)/2;
          %      ren = h.ncols + (j-1)*(2*h.ncols-j)/2;
          %      RR(j,j:h.ncols) = R(rst:ren);
          %  end
          
          bw = readbw(h);
          n = h.ncols;
          cv = h.common;
          nn = n - cv;
          r_len = (nn-bw)*bw + bw*(bw+1)/2;
          R = rbinfile(h.Rfile,r_len,h.cplx,h.prec);
          RR1 = reshape(R(1:((nn-bw)*bw)),bw,nn-bw)';
          RR2 = R((nn-bw)*bw+1:end);
          
          RR = zeros(n,nn);
          
          for j=1:nn-bw
              RR(j,j:j+bw-1) = RR1(j,:);
          end
          
          k=0;
          for j = nn-bw+1:nn
              k = k + 1;
              st = k + (k-1)*(2*bw-k)/2;
              en = bw + (k-1)*(2*bw-k)/2;
              RR(j,j:nn) = RR2(st:en);
          end
          
         
          % Read R data
          if (h.common>0)
              cv_len = nn*cv + cv*(cv+1)/2;
              R_cv = rbinfile(h.cvfile,cv_len,h.cplx,h.prec);
              R_cv1 = R_cv(1:(nn*cv));
              R_cv2 = R_cv((nn*cv+1):end);
              
              R12 = reshape(R_cv1,cv,nn)';
              
              RRR=zeros(n,cv);
              
              RRR(1:nn,1:cv) = R12;
              
              k=0;
              for j = nn+1:n
                  k=k+1;
                  st = k + (k-1)*(2*cv-k)/2;
                  en = cv + (k-1)*(2*cv-k)/2;
                  RRR(j,k:cv) = R_cv2(st:en);
                  
                  RR = [RR RRR];
              end
              
          end
         
          varargout{i} = RR;
                
        case 'yma'
            Y = rbinfile(h.Yfile,h.ncols*h.nrhs,h.cplx,h.prec);
            varargout{i} = reshape(Y,h.nrhs,h.ncols)';
            
        
        % Additions 080121 end here -----
            
           
        otherwise
            warning(['Unknown or unimplemented parameter name: ' name])
            varargout{i} = [];
    end
    
end
