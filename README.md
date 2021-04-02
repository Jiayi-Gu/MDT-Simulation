# Assumptions & modelling decisions
1) Model only to be used on MDT version 2 projects because version 2 records whether a recommendation in a query has led to an invitation to the recommended user
2) To not allow invitations made to enjoy.work connections because for some classes, quite a number of users knew each other.  (can’t forbid invitations made to know connections as that would eliminate too many options and ende of creating near completely randomized teams.) 
Possibility to change this in the code
4) For each project, the simulation creates the same number of as the number of teams it has in reality. The simulation caps the size of the teams to be the maximum team size of the teams formed in reality.
5) Consider invitations in the order of time replied. Only consider invitations that were accepted.
6) Remove invitations that were sent directly to a user (without query). Only project 231 has no direct invitations. 
7) If exhaust all options from a single query, send an invite to the first result of the query, regardless of their relationship. And continues down the list until one accepts. 
8) Probability of accepting an invitation is currently fixed - 50%. Can be changed to reflect relationship.
9) If the invite was sent to a non enjoy.work person in the first place, I assigned a probability decision (in the simulation this user might not accept, however we know this person accepted the invite). This is to make future changes to the model easier.
10) If we have created enough teams to hit the team number hit limit (see 3), add individuals without teams to the team with fewest members



# Simulation framework
## Configuration:
Set correct project ID, the code should find
- Number of teams
- Upper lower size of teams 
- (Note that for some projects adjustment to the number of teams are needed, see comments in code. **If you run into an error in running the simulation, it is HIGHLY LIKELY that it is because the team structure (number of teams*upper size) can’t contain all users, hence the adjustments.**)

## Functions:
1. invitation_eligible: checks if the invitation, if accepted, will create a team with a size exceeding the team size upper limit
2. invitation_accept: 50-50 chance the invitation will be accepted or rejected
3. add_user: adds user to sender’s team
4. remove_team: after we copied all users from the recipient’s team to the sender’s team, remove recipient team
5. invite_to_team: execute the invitation and add users to team/merge two teams. Uses add_user and remove_team
6. invite_though_query: goes through the list of recommendations from the query until an invite is made

## Basic structure of the loop
Start with the first accepted invitation.
- (a) If it is to an enjoy.work connection, redirect the invitation to the next person recommended.
  - 50% chance that the next person will accept the invitation.
  - Accept: enlist person into team if team is not full
  - Not accept: move to the next person
    - invite_through_query

- (b) If it is not to a prior connection, invite as usual
  - 50% chance that the next person will accept the invitation.
  - Accept: enlist person into team if team is not full
  - Not accept: move to the next person
    - invite_through_query

Move to the next invitation (time wise)
- If user already has a team, repeat a/b adding new members to existing team
- If user does not have a team, create new team and repeat a/b

Check size of all teams
- Any team with size lower than the lower team size limit
- Assign random unteamed users to the team
