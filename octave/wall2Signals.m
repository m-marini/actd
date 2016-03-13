function Y = wall2Signals(X)
  [n m] = size(X);
  Width = 13;
  Height = 10;
  PadSize = 3;
  BallDim = Width * Height;
  PadDim = Width - PadSize + 1;
  DirectionDim = 4;
  SignalDim = BallDim * PadDim * DirectionDim + 1;
  
  Y = zeros(n, SignalDim);

  if X(1) == 0
    Y(SignalDim) = 1;
  else
    I = zeros(n, 1);
    I = 1 + X(:, 2) + (X(:, 1) - 1) * Width + X(:, 3) * BallDim + X(:, 4) * BallDim * DirectionDim;

    for i = 1 : n
      Y(i,I(i)) = 1;
    endfor
  endif
endfunction