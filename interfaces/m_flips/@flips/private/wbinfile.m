function wbinfile(fname,data,cplx,append,prec)

% Writes/appends to a binary file


if (append==1)
    fid = fopen(fname,'ab');
else
    fid = fopen(fname,'wb');
end

datalen = prod(size(data));
%datalen = datalen(2);

if (cplx == 1)
    datar = real(data);
    datai = imag(data);
    
    ddata=zeros(1,2*datalen);
    ddata(1:2:2*datalen) = datar;
    ddata(2:2:2*datalen) = datai;
%    for n = 1:datalen
%        ddata(2*n-1) = datar(n);
%        ddata(2*n) = datai(n);
%    end
    
    fwrite(fid,ddata,prec);
    
else
    
    fwrite(fid,data,prec);
end

fclose(fid);