function flips_set_buffersize(h,bsize)
%FLIPS_SET_BUFFERSIZE Sets new rotation buffer size for FLIPS object
%
%    FLIPS_SOLVE(H,BSIZE) Sets the rotation buffer size of object H
%    to BSIZE.
% 
%    Default buffer size is 100. Increasing the buffer size can have
%    a dramatic effect in performance for large low bandwidth problems.
%    However, use with caution since it can also decrease the performance
%    as dramatically.

hname = inputname(1);

h.buffersize = bsize;

assignin('caller',hname,h);

end