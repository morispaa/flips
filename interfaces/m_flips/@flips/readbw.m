function data=readbw(h)

% Reads the current bandwidth from the file

fid = fopen(h.bwfile,'rb');

data = fread(fid,2,'uint32');

fclose(fid);

end