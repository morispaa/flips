function h = flips()
%FLIPS Create FLIPS object.
%   H = FLIPS() creates a new empty FLIPS object.



h.name = '';
h.ncols = 0;
h.nrhs = 0;
h.dbl=0;
h.cplx=0;
h.band=0;
h.common=0;

h.prec = '';
h.type = '';

h.flipseng = '';

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

h.Rfile = '';
h.Yfile = '';
h.Sfile = '';
h.residfile = '';
h.covfile = '';
h.rinvfile = '';
h.Afile = '';
h.Mfile = '';
h.Efile = '';
h.rmaskfile = '';
h.bwfile = '';
h.cvfile = '';

h.buffersize = 100;
    
h=class(h,'flips');
