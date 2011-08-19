function wnamelist(h,mode,resid,cov,fcov,bsize)

% Write namelist for flipseng.
fname = sprintf('rflips.nml',h.name);


if (h.band>0)
    bbb = 1;
else
    bbb = 0;
end

fid = fopen(fname,'wt');

fprintf(fid,'&PARAMS\n');

fprintf(fid,'mode=%d\n',mode);
fprintf(fid,'cplx=%d\n',h.cplx);
fprintf(fid,'id="%s"\n',h.name);
fprintf(fid,'ncols=%d\n',h.ncols);
fprintf(fid,'nrhs=%d\n',h.nrhs);
fprintf(fid,'crows=%d\n',h.crows);
fprintf(fid,'newrows=%d\n',h.newrows);
fprintf(fid,'naddvar=%d\n',h.n_addvar);
fprintf(fid,'ndelvar=%d\n',h.n_delvar);
fprintf(fid,'dbl=%d\n',h.dbl);
fprintf(fid,'rexists=%d\n',h.r_written);
fprintf(fid,'calc_resid=%d\n',resid);
fprintf(fid,'calc_cov=%d\n',cov);
fprintf(fid,'full_cov=%d\n',fcov);
fprintf(fid,'bsize=%d\n',bsize);
fprintf(fid,'common=%d\n',h.common);
fprintf(fid,'band=%d\n',bbb);






fprintf(fid,'/');

fclose(fid);