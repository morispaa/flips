% FLIPS Toolbox 
% Version 2.0.3 8-Apr-2010
% Author: Mikko Orispaa <mikko.orispaa@oulu.fi>
%
% M_FLIPS is a MATLAB interface to FLIPS, the Fortran Linear Inverse Problem
% Solver. FLIPS is copyrighted by the University of Oulu, Finland, and it is
% licensed under GNU General Public License version 2.
%
% FLIPS is a Fortran95 module for solving large scale statistical linear inverse problems. 
% For more information about FLIPS, visit http://mep.fi/mediawiki/index.php/Flips
%
% FLIPS object class
%   flips           - Create a FLIPS object
%
% FLIPS object methods
%   <a href="matlab: help flips_init">flips_init</a>            - Initialize a FLIPS object
%   <a href="matlab: help flips_add">flips_add</a>             - Add data into FLIPS object
%   <a href="matlab: help flips_solve">flips_solve</a>           - Solve FLIPS object
%   <a href="matlab: help flips_resize">flips_resize</a>          - Marginalize/add unknowns into FLIPS object
%   <a href="matlab: help flips_copy">flips_copy</a>            - Copy FLIPS objects
%   <a href="matlab: help flips_dispose">flips_dispose</a>         - Reset FLIPS object and delete all files associated with it
%   <a href="matlab: help flips_get">flips_get</a>             - Fetch parameters and data from FLIPS object
%   <a href="matlab: help flips_rotate">flips_rotate</a>          - Force Givens rotations on FLIPS object
%   <a href="matlab: help flips_put">flips_put</a>             - Manually insert target matrix or vector
%   <a href="matlab: help flips_delete">flips_delete</a>          - Delete datarows fron FLIPS object
%   <a href="matlab: help flips_set_buffersize">flips_set_buffersize</a>  - Set rotation buffersize
%
% Example:
%   Solve an overdetermined linear problem
%        M = A*X + E,
%   where M is called the measurement, A is called the direct theory matrix, 
%   X is unknown and E is a known error (standard deviation of the measurement).
%
%
%   
%   % theory matrix
%   A = [1 2 3; 2 3 1; 3 2 1; 1 3 2];
%
%   % measurement
%   M = [0 1 2 3];
%
%   % errors
%   E = [0.1 0.2 0.3 0.4];
%
%   % new FLIPS object
%   h = flips;
%
%   % initialize h with 3 unknowns and 1 right hand side (double precision real)
%   flips_init(h,3,1,'d');
%
%   % add data (4 rows) into h
%   flips_add(h,A,M,E);
%
%   % solve problem (with residual and full posteriori covariance matrix
%   flips_solve(h,'rfc')
%
%   % get solution, residual and covariance
%   [sol, res, cov] = flips_get(h,'sol','res','cov');
%
%   % reset h and delete files/folders
%   flips_dispose(h);
%



