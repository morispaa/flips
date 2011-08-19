function display( h )

var = inputname(1);

disp(' ')

if isempty(var)
    disp('ans =')
else
    disp([var ' ='])
end

disp('          FLIPS Object:')
disp(' ')

if (h.ncols==0)
    disp('          Uninitialized')
else
    disp(['          Number of unknowns: ' sprintf('%d',h.ncols) ])
    disp(['               Number of rhs: ' sprintf('%d',h.nrhs) ])
    disp(['                        Type: ' h.type])
    disp(' ')
    disp(['         Number of data rows: ' sprintf('%d',h.crows+h.newrows) ])
    disp(['        Rotation buffer size: ' sprintf('%d',h.buffersize) ])
    disp(' ')
    disp(['                    Solution: ' exists(h.sol_written) ])
    disp(['                    Residual: ' exists(h.residual_written) ])
    disp(['                  Covariance: ' cexists(h.cov_written,h.cov_full) ]) 
    disp(' ')
    disp(['            Common variables: ' sprintf('%d',h.common) ])
    disp(['                   Bandwidth: ' sprintf('%d',readbw(h)) ])
    
    disp(' ')
end

disp(' ')


function oo = exists( inq )

if (inq==0)
    oo = 'No';
else
    oo = 'Yes';
end

end


function oo = cexists( inq, full )

if (inq==0)
    oo = 'No';
else
    if (full==0)
        oo = 'Yes (diagonal)';
    else
        oo = 'Yes (full)';
    end
end

end


end

