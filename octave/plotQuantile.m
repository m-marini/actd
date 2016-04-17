function plotQuantile(filename, l=1e6, Q = [0.25,0.75])
   X = ((csvread(filename)(:,2)-1)/18);
   #X = (csvread(filename)(:,2));
   n = size(X,1);
   m = size(Q,2);
   Y = zeros(n, m + 3);
   for i = 1 : n
     from = i - min(l,i)+1;
     Y(i,1:m) = quantile(X(from:i), Q)';
     Y(i,m+1) = median(X(from:i));
     Y(i,m+2) = mean(X(from:i));
     Y(i,m+3) = mode(X(from:i));
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