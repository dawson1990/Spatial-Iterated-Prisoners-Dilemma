%%%-------------------------------------------------------------------
%%% @author Kevin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2016 16:33
%%%-------------------------------------------------------------------
-module(warden).
-author("Kevin").

-export([start/1,add/2,stats/1,run/2, supervisor/1,rankScores/1]).
-include_lib("eunit/include/eunit.hrl").


start(State)->
  PID=spawn(?MODULE, supervisor, [State]), % warden, supervisor -> this is the function and then state
  PID.
add(SupervisorPID,PID)-> % to supervisor add the processor ID of prisoner
  SupervisorPID!{self(),add,PID}, % send message
  receive % wait for the response to say that the function got it
    {SupervisorPID,done,Total} -> % message has to be from supervisor
      Total
  end.
stats(SupervisorPID)->
  SupervisorPID!{self(),stats},
  receive
    {SupervisorPID,State} ->
      State
  end.
run(SupervisorPID,Iterations)-> % iterations, how many times should it run
  SupervisorPID!{self(),run,Iterations},
  receive
    {SupervisorPID,done,Results} ->
      Results
  end.

% This does the work
supervisor({PrisonerList,Summary,History})->
  receive
    {Sender,add,PID}-> % sender, atom and how to send to
      Sender!{self(),done,length(PrisonerList)+1}, % sends message back saying 'done' with the new length with the added prisoner
      supervisor({[PID|PrisonerList],Summary,History}); % adds the Prisoner ID to the top of the list
    {Sender,stats} ->
      Sender!{self(),{Summary, History}}, % sends the state back to the warden
      supervisor({PrisonerList,Summary,History});
    {Sender,run,Count} ->
      {NewSummary,NewHistory}=iterate(PrisonerList,Summary,History,Count), % sends the prisoner list, the summary, history and the number of times to run
      %%returns the score so far ordered from lowest to highest, sends the current Summary to function rankScores.  rankScores takes a map and returns an ordered list of tuples
      Results = rankScores(NewSummary),
      Sender!{self(),done, Results},
      supervisor({PrisonerList,NewSummary,NewHistory})
  end.
iterate(_,Summary,History,0)->
  {Summary,History};
% Called by the Supervisor
iterate(Prisoners,Summary,History,N) ->
  {NewSummary,NewHistory}=doOneRun(Prisoners,Summary,History), % sends prisoners list, summary and history
  iterate(Prisoners,NewSummary,NewHistory,N-1).
doOneRun([],Summary,History)->
  {Summary,History};
doOneRun([First|Rest],Summary,History) ->
  {NewSummary,NewHistory}=doOnce(First,Rest,Summary,History), % sends the first element of the prisoner list, the rest of the list, summary and the history
  doOneRun(Rest,NewSummary,NewHistory).
doOnce(_,[],Summary,History)->
  {Summary,History};
doOnce(Agent,[OtherAgent|Rest],Summary,History) -> % agent is the fist element in the prisoner list, then the rest of the list, then the summary, and history
  Agent!{self(),name}, % asks the first prisoner for there name ------------------------------------------
  receive
    {Agent,name,MyName}-> % receives the first prisoners name
      ok
  end,
  OtherAgent!{self(),name}, % asks for the second prisoners for there name
  receive
    {OtherAgent,name,OtherName}-> % receives it
      ok
  end,
  OtherAgent!{self(),choose,MyName}, % second prisoner chooses the first prisoner
  receive
    {OtherAgent,choice,OtherChoice}->
      ok
  end,
  Agent!{self(),choose,OtherName}, % first prisoner chooses the second prisoner
  receive
    {Agent,choice,MyChoice}->
      ok
  end,
  OtherAgent!{self(),result,MyChoice},
%%  works when you take out list and replace with one variable
  receive
    {OtherAgent, result, OtherScore} ->
      ok
  end,
  Agent!{self(),result,OtherChoice},
  receive
    {Agent, result, MyScore} ->
      ok
  end,
  maps:put(MyName, 0 , Summary), %%puts name of inmate as key and value 0 into Summary map
  maps:put(OtherName, 0 , Summary), %%puts name of other inmate as key and value 0 into Summary map
  CurrentScore1 = maps:get(MyName, Summary,0),  %gets score for inmate from Summary, returns 0 if a value can't be found for the key
  CurrentScore2 = maps:get(OtherName, Summary,0),%gets score for other inmate from Summary, returns 0 if a value can't be found for the key
  MyNewScore = CurrentScore1 + MyScore, % combines overall score with score of current round
  OthersNewScore = CurrentScore2 + OtherScore, % combines overall score with score of current round
  NewestSummary = maps:put(MyName, MyNewScore, Summary), % puts new current score into a new map
  FinalSummary = maps:put(OtherName, OthersNewScore, NewestSummary), % puts new score of other inmate and combines it with map holding first inmates score
  OtherAgent!{self(), scores, OtherName},
  doOnce(Agent,Rest,FinalSummary,[{MyName,MyChoice,OtherName,OtherChoice}|History]).


%%function to rank scores on sentence time, takes in a map and returns a sorted list
rankScores(Scores) ->
  ListSummary = maps:to_list(Scores),%% converts map to a list of tuples with key,value in each
  %% using lists sorting, the function swaps the order of the tuple contents as tuples automatically compare from first to last, then it sorts the list
  lists:sort(fun({KeyA,ValA}, {KeyB, ValB}) ->
                {ValA,KeyA} =< {ValB,KeyB}
             end, ListSummary).

