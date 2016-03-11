function A = loadAgent(file)
  A.parms = csvread([file "-parms.csv"]);
  A.critic = loadNet([file "-critic"]);
  A.actor = loadNet([file "-actor"]);
endfunction