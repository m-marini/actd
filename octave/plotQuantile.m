function plotQuantile(filename, Q = [0.25,0.5,0.75])
   X = (csvread(filename)(:,2)-1/18);
   n = size(X,1);
   m = size(Q,2);
   Y = zeros(n, m);
   for i = 1 : n
     Y(i,:) = quantile(X(1:i), Q)';
   endfor
   plot([Y movingAvg(X, n)]);
   grid minor on;
   L = cell(1,m+1);
   for i = 1 : m
    L(1,i) = sprintf("%g", Q(i));
   endfor
   L(1,m+1)= "Mean";
   legend(L);
endfunction