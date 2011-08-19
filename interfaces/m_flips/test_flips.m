function [time,acc] = test_flips(fsize,type,mode)

nn = length(fsize);

time = [];
acc = [];

for n = 1:nn,
    
    ss = fsize(n);
    
    h = flips;
    
    flips_init(h,ss,1,type);
    
    sol = ones(ss,1);
    A = randn(ss);
    m = A*sol;
    
    %tc = cputime;
    tic;
    flips_add(h,A,m);
    flips_solve(h,mode);
    time(1,n) = toc;
    %time(2,n) = cputime-tc;
    
    ssol = flips_get(h,'sol');
    acc(n) = mean(sol-ssol);
    
    flips_dispose(h);
    
end