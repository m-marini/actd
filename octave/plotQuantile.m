function plotQuantile(filename, Q = [0.25,0.75])
   X = ((csvread(filename)(:,2)-1)/18);
   #X = (csvread(filename)(:,2));
   n = size(X,1);
   m = size(Q,2);
   Y = zeros(n, m + 3);
   for i = 1 : n
     Y(i,1:m) = quantile(X(1:i), Q)';
     Y(i,m+1) = median(X(1:i));
     Y(i,m+2) = mean(X(1:i));
     Y(i,m+3) = mode(X(1:i));
   endfor
   plot(Y);
   grid minor on;
   L = cell(1,m+3);
   for i = 1 : m
    L(1,i) = sprintf("%g", Q(i));
   endfor
   L(1,m+1)= "Median";
   L(1,m+2)= "Mean";
   L(1,m+3)= "Mode";
   legend(L);
   title("Wall Game bouncing")
endfunction