function flips_copy(h_old,h_new)
%FLIPS_COPY Copy FLIPS objects.
%
%    FLIPS_COPY(H_OLD,H_NEW) copies the FLIPS object H_OLD and the files
%    associated with it into FLIPS object H_NEW. The FLIPS object H_NEW
%    should be in uninitialized state before calling FLIPS_COPY.

%
% Copies FLIPS structure and FLIPS files into a new FLIPS structure.

outname = inputname(2);



% Create a new FLIPS structure
flips_init(h_new,h_old.ncols,h_old.nrhs,h_old.type);
rmdir(sprintf('FLIPS_%s',h_new.name));


% Copy variables
h_new.name = outname;
h_new.covariance = h_old.covariance;
h_new.residual = h_old.residual;
h_new.solution = h_old.solution;
h_new.cov_full = h_old.cov_full;
h_new.cov_written = h_old.cov_written;
h_new.residual_written = h_old.residual_written;
h_new.rinv_written = h_old.rinv_written;
h_new.sol_written = h_old.sol_written;
h_new.r_written = h_old.r_written;
h_new.newrows = h_old.newrows;
h_new.crows = h_old.crows;
h_new.append = h_old.append;
h_new.common = h_old.common;
h_new.band = h_old.band;

% Fix paths (this broke up in MATLAB 2008a)
h_new.Rfile = regexprep(h_new.Rfile,'h_new',h_new.name);
h_new.Yfile = regexprep(h_new.Yfile,'h_new',h_new.name);
h_new.Sfile = regexprep(h_new.Sfile,'h_new',h_new.name);
h_new.residfile = regexprep(h_new.residfile,'h_new',h_new.name);
h_new.covfile = regexprep(h_new.covfile,'h_new',h_new.name);
h_new.rinvfile = regexprep(h_new.rinvfile,'h_new',h_new.name);
h_new.Afile = regexprep(h_new.Afile,'h_new',h_new.name);
h_new.Mfile = regexprep(h_new.Mfile,'h_new',h_new.name);
h_new.Efile = regexprep(h_new.Efile,'h_new',h_new.name);
h_new.rmaskfile = regexprep(h_new.rmaskfile,'h_new',h_new.name);
h_new.bwfile = regexprep(h_new.bwfile,'h_new',h_new.name);
h_new.cvfile = regexprep(h_new.cvfile,'h_new',h_new.name);


% Copy files
olddir=sprintf('FLIPS_%s',h_old.name);
newdir = sprintf('FLIPS_%s',outname);

copyfile(olddir,newdir);

assignin('caller',outname,h_new);

