function flips_resize(h,remove,add)
%FLIPS_RESIZE Resize FLIPS structure by marginalizing and/or adding unknowns.
%              
%
%    FLIPS_RESIZE(H,REMOVE,ADD) resizes the FLIPS object H by marginalizing
%    unknowns given by vector REMOVE and adding ADD number of new unknowns.
%    If no marginalization of unknowns is desirable, an empty vector []
%    must be given as REMOVE. If no adding of variables is going to take
%    place, number zero (0) must be given as ADD.
%
%    Marginalizing unknowns do not affect the solution or the posteriori
%    covariance of the remaining unknowns.
%
%    New unknowns will be inserted in the end, i.e. if, say, 10 unknowns
%    are added to a problem which had 50 unknowns before, the new unknowns
%    will be indexded as numbers 51...60. Note that a problem needs
%    additional data to be solvable.
%
% Example 1: Marginalize first 10 unknowns from a 100 unknowns problem.
%
%    %Create FLIPS problem with random data
%    h=flips;
%    flips_init(h,100,1,'d');
%    flips_add(h,randn(100),randn(100,1));
%
%    % Now marginalize first 10 unknowns (and add 0)
%    flips_resize(h,1:10,0);
%
% Example 2: Expand problem by adding 10 unknowns.
%
%    flips_resize(h,[],10);
%
% Example 3: Marginalize unknowns number 1,3,5,...,19 and add 5 new
%        unknowns.
%
%    flips_resize(h,1:2:19,5);
%


hname = inputname(1);

% Check arguments
if (add ~= uint32(add))
    error('add must be an integer!')
end

rlen = length(remove);
h.n_addvar = add;
h.n_delvar = 0;

o_common = h.common;

% Write mask vector
if (rlen > 0)
    rmask = zeros(h.ncols,1);
    rmask(remove) = 1;
    h.n_delvar = sum(rmask);
    
    nremcv = sum(rmask((h.ncols-h.common+1):h.ncols));
    
    wbinfile(h.rmaskfile,rmask,0,0,'uint32');
else
    nremcv = 0;
end

% Write namelist
wnamelist(h,2,0,0,0,h.buffersize);

% Run flipseng
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
h.crows = h.crows + h.newrows;
if (h.crows > h.ncols) h.crows = h.ncols; end

h.newrows = 0;
h.ncols = h.ncols - h.n_delvar + h.n_addvar;
h.crows = h.crows - h.n_delvar;
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

h.common = h.common - nremcv + add;

% If the number of common variables is zero, delete the file
if (h.common == 0 & o_common)
    delete(h.cvfile);
end

    


assignin('caller',hname,h);


