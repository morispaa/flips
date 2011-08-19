function data = rbinfile(fname,dlen,cplx,prec)

% Writes/appends to a binary file



fid = fopen(fname,'rb');




if (cplx == 1)
    ddlen = 2*dlen;
    ddata = fread(fid,ddlen,prec);
    datar = ddata(1:2:ddlen);
    datai = ddata(2:2:ddlen);
    
    data = complex(datar,datai).';
    
else
    
    data = fread(fid,dlen,prec)';
end

fclose(fid);