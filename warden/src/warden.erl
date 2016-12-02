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

-export([start/1,add/2,stats/1,run/2,supervisor/1]).
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
    {SupervisorPID,History} ->
      History
  end.
run(SupervisorPID,Iterations)-> % iterations, how many times should it run
  SupervisorPID!{self(),run,Iterations},
  receive
    {SupervisorPID,done} ->
      ok
  end.
% This does the work
supervisor({PrisonerList,Summary,History})->
  receive
    {Sender,add,PID}-> % sender, atom and how to send to
      Sender!{self(),done,length(PrisonerList)+1}, % sends message back saying 'done' with the new length with the added prisoner
      supervisor({[PID|PrisonerList],Summary,History}); % adds the Prisoner ID to the top of the list
    {Sender,stats} ->
      Sender!{self(),History}, % sends the summary back to the warden
      supervisor({PrisonerList,Summary,History});
    {Sender,run,Count} ->
      {NewSummary,NewHistory}=iterate(PrisonerList,Summary,History,Count), % sends the prisoner list, the summary, history and the number of times to run
      Sender!{self(),done},
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
  Agent!{self(),result,OtherChoice},
  doOnce(Agent,Rest,Summary,[{MyName,MyChoice,OtherName,OtherChoice}|History]).