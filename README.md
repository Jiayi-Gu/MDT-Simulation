# MDT-Simulation
## Assumptions & modelling decisions
1) Model only to be used on MDT version 2 projects because version 2 records whether a recommendation in a query has led to an invitation to the recommended user
2) To not allow invitations made to enjoy.work connections because for some classes, quite a number of users knew each other.  (can’t forbid invitations made to know connections as that would eliminate too many options and ende of creating near completely randomized teams.) 
Possibility to change this in the code
4) For each project, the simulation creates the same number of as the number of teams it has in reality. The simulation caps the size of the teams to be the maximum team size of the teams formed in reality.
5) Consider invitations in the order of time replied. Only consider invitations that were accepted.
6) Remove invitations that were sent directly to a user (without query). Only project 231 has no direct invitations. 
7) If exhaust all options from a single query, send an invite to the first result of the query, regardless of their relationship. And continues down the list until one accepts. 
8) Probability of accepting an invitation is currently fixed - 50%.
9) If the invite was sent to a non enjoy.work person in the first place, I assigned a probability decision (in the simulation this user might not accept, however we know this person accepted the invite). This is to make future changes to the model easier.
10) If we have created enough teams to hit the team number hit limit (see C), add individuals without teams to the team with fewest members
