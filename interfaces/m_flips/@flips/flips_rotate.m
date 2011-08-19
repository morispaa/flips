function flips_rotate(h)
%FLIPS_ROTATE Rotate all unrotated data rows
%
%    FLIPS_ROTATE(H) forces the Givens rotations on given FLIPS object H.
%    This command rotates the current data rows into FLIPS. It can be used 
%    at any point when there exists unrotated data rows. For large problems
%    giving FLIPS_ROTATE from time to time can remarkably reduce the solution
%    time of FLIPS problem. However, overusing this command can also reduce the
%    performance of FLIPS. The best interval of using this depends on computer
%    platform (CPU, memory, disk speed). A decent rule-of-thumb would be using
%    this command after every couple of hundred data rows fed into FLIPS for 
%    larger problems (with thousands of unknowns).

hname=inputname(1);

% Write namelist
wnamelist(h,3,0,0,0,h.buffersize);

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
h.crows = h.crows + h.newrows;
h.newrows = 0;
if (h.crows > h.ncols) h.crows = h.ncols; end

h.sol_written = 0;
h.r_written = 1;
h.residual_written = 0;
h.cov_written = 0;
h.cov_full = 0;
h.rinv_written = 0;

% read new bandwidth
% h.band=readbw(h);

% Read current band and common variable count
tmp = readbw(h);
h.band = tmp(1);
h.common = tmp(2);


assignin('caller',hname,h);


    
    

