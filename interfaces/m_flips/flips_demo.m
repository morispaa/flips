function flips_demo()

% Construct solution, theory matrix and measurement

% Solution
x=linspace(0,1,500);
sol=abs(100*x.*(x-0.2).*(x-0.6).*(x-1));

% Plot solution
subplot(3,2,1)
plot(x,sol)
title('Solution')

% Construct measurement by convolving the solution with a Hamming window of
% length 100.
meas = conv(sol,hamming(100));

% plot measurement
subplot(3,2,2)
plot(meas)
title('Measurement (no error)')

% Add noise to the measurement
noisy_meas = 0.1 * mean(meas) * randn(1,length(meas)) + meas;

% plot noisy data
subplot(3,2,3)
plot(noisy_meas)
title('Noisy measurement')

% Construct theory matrix
A = zeros(length(meas),500);
for (i=1:500)
    A(i:(i+99),i) = hamming(100);
end

% Initialize FLIPS object, add data and solve
h = flips;
flips_init(h,500,1,'d');
flips_add(h,A,noisy_meas);
flips_solve(h,'');
fsol1 = flips_get(h,'sol');

subplot(3,2,4)
plot(x,fsol1)
title('Dummy boy solution (no regularization)')

% Add regularization and solve again
reg_mat = 2*eye(500) + -1*diag(ones(499,1),1) + -1*diag(ones(499,1),-1);
reg_meas = zeros(500,1);
reg_err = 0.001^2;

flips_add(h,reg_mat,reg_meas,reg_err);
flips_solve(h,'');
fsol2 = flips_get(h,'sol');

subplot(3,2,5)
plot(x,fsol2)
hold on
plot(x,sol,'r')
hold off
title('Regularized and true solutions')

% Initialize a new problem, add the same data and regularization, and then
% marginalize away the first and last 100 unknowns
h2=flips;
flips_init(h2,500,1,'d');
flips_add(h2,A,noisy_meas);
flips_add(h2,reg_mat,reg_meas,reg_err);
flips_resize(h2,[1:100 401:500],0);
flips_solve(h2,'');
fsol3 = flips_get(h2,'sol');

subplot(3,2,6)
plot(x,fsol2)
hold on
plot(x(101:400),fsol3,'ro')
hold off
title('Original and resized solutions')

%readbw(h)
%readbw(h2)

% Delete FLIPS files and reset FLIPS objects
flips_dispose(h);
flips_dispose(h2);

% Test band matrix system and common variables


% 
% %plot((1:10)/10,btimes)
% %hold;
% 
% 
% for i = 1:10
%     
%     band = n*i/10;
%     
%     B = zeros(n,n+2*band);
%     
%     for b = 1:n
%         B(b,b:b+2*band) = randn(band*2+1,1);
%     end
%     
%     A = B(:,band+1:band+n);
%     A(:,n-cv+1:n) = randn(n,cv);
%     
%     spy(A)
%     
%     
%     %size(A)
%     
% 
%     flips_init(bb,n,1,'d',cv,1);
%     
%     m = sum(A,2);
%     
%     st=tic;
%     flips_add(bb,A,m);
%     flips_solve(bb,'')
%     en=toc(st);
%     
%     ctimes(i) = en;
%     %ctimes(i)
%     ss=flips_get(bb,'sol');
%     
%     mean(ss-ones(n,1))
%     
%     flips_dispose(bb)
%     
% end
% 
% %plot((1:10)/10,ctimes,'g')
% 
% 
% 
% for i = 1:10
%     
%     band = n*i/10;
%     
%     B = zeros(n,n+2*band);
%     
%     for b = 1:n
%         B(b,b:b+2*band) = randn(band*2+1,1);
%     end
%     
%     A = B(:,band+1:band+n);
%     A(:,n-cv+1:n) = randn(n,cv);
%     
%     %size(A)
%     spy(A)
% 
%     flips_init(bb,n,1,'d',0,0);
%     
%     m = sum(A,2);
%     
%     st=tic;
%     flips_add(bb,A,m);
%     flips_solve(bb,'')
%     en=toc(st);
%     
%     ctimes(i) = en;
%     ss=flips_get(bb,'sol');
%     
%     mean(ss-ones(n,1))
%     
%     flips_dispose(bb)
%     
% end
% 
% %plot((1:10)/10,ctimes,'r')

    
    
    
    
