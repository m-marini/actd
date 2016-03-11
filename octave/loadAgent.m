function A = loadAgent(file)
  p = csvread([file "-parms.csv"]);
  A.parms.alpha = p(1);
  A.parms.beta = p(2);
  A.parms.gamma = p(3);
  A.parms.epsilon = p(4);
  A.parms.lambda = p(5);
  A.parms.eta = p(6);
  A.parms.maxSamples = p(7);
  A.critic = loadNet([file "-critic"]);
  A.actor = loadNet([file "-actor"]);
endfunction