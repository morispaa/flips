function flips_dispose(h)
%FLIPS_DISPOSE Reset FLIPS object and delete all files associated with it.
%
%    FLIPS_DISPOSE(H) resets the FLIPS object H and deletes the directory
%    named FLIPS_H that contains all the files used by FLIPS. The FLIPS
%    object H will be in uninitialized state afterwards.



hname = inputname(1);

% Delete directory
dirname = sprintf('FLIPS_%s',h.name);
[s,m,mid] = rmdir(dirname,'s');

if (s == 0)
    error(['FLIPS_DISPOSE: Could not delete directory ' dirname])
end



% Reset FLIPS object
h=flips();

assignin('caller',hname,h);



