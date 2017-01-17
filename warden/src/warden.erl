%%%-------------------------------------------------------------------
%%% @author Kevin Dawson
%%% @copyright (C) 2016,
%%% @doc
%%%  standard version
%%% @end
%%% Created : 10. Nov 2016 16:33
%%%-------------------------------------------------------------------
-module(warden).
-author("Kevin").

-export([start/1,add/2,stats/1,run/2,supervisor/1]).
% contains code that will override the meaning of ets:fun2ms().  Gotten from Learn You Some Erlang
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").
%%to test private functions
-ifdef(TEST).
-export([timerun/1,rankScores/1,retrieveSummaryScore/1]).
-endif.

start(State)->
%%  creates ets table named summary which is of type ordered set and public so other processes can read and write from/to it
  ets:new(summary,[ordered_set, named_table,public ]),
  PID=spawn(?MODULE, supervisor, [State]), % warden, supervisor -> this is the function and then state
  PID.
add(SupervisorPID,PID)-> % to supervisor add the processor ID of prisoner
  SupervisorPID!{self(),add,PID}, % send message
  receive % wait for the response to say that the function got it
    {SupervisorPID,done,Total} -> % message has to be from supervisor
      Total
  end.
%returns state which contains a tuple with two lists: Summary Score and Play History
stats(SupervisorPID)->
  SupervisorPID!{self(),stats},
  receive
    {SupervisorPID,State} ->
      State
  end.
run(SupervisorPID,Iterations)-> % iterations, how many times should it run
  SupervisorPID!{self(),run,Iterations},
  receive
    {SupervisorPID,done} ->
      ok
  end.

% This does the work
supervisor({PrisonerList,History})->
  receive
    {Sender,add,PID}-> % sender, atom and how to send to
      Sender!{self(),done,length(PrisonerList)+1}, % sends message back saying 'done' with the new length with the added prisoner
      supervisor({[PID|PrisonerList],History}); % adds the Prisoner ID to the top of the list
    {Sender,stats} ->
      %selects each {Key,PID,Value} pair in ets table and returns them into a variable which is a list of tuples,
      % use of '-include_lib("stdlib/include/ms_transform.hrl").'
      Summary = ets:select(summary,ets:fun2ms(fun(X ={_Key,_PID,_Value}) -> X end)),
      RankedSummary = rankScores(Summary), %sends list of tuples to function rankScores which returns an ordered list of tuples
      Sender!{self(),{RankedSummary,History}}, % sends the state back to the warden
      supervisor({PrisonerList,History});
    {Sender,run,Count} ->
      {NewHistory}=iterate(PrisonerList,History,Count), % sends the prisoner list, the summary, history and the number of times to run
      Sender!{self(),done},
      supervisor({PrisonerList,NewHistory})
  end.
iterate(_,History,0)->
  {History};
% Called by the Supervisor
iterate(Prisoners,History,N) ->
  {NewHistory}=doOneRun(Prisoners,History), % sends prisoners list, summary and history
  iterate(Prisoners,NewHistory,N-1).
doOneRun([],History)->
  {History};
doOneRun([First|Rest],History) ->
  {NewHistory}=doOnce(First,Rest,History), % sends the first element of the prisoner list, the rest of the list, summary and the history
  doOneRun(Rest,NewHistory).
doOnce(_,[],History)->
  {History};
doOnce(Agent,[OtherAgent|Rest],History) -> % agent is the fist element in the prisoner list, then the rest of the list and history
  Agent!{self(),name}, % asks the first prisoner for there name ------------------------------------------
  receive
    {Agent,name,_MyName}-> % receives the first prisoners name
      ok
  end,
  OtherAgent!{self(),name}, % asks for the second prisoners for there name
  receive
    {OtherAgent,name,_OtherName}-> % receives it
      ok
  end,
  OtherAgent!{self(),choose,_MyName}, % second prisoner chooses the first prisoner
  receive
    {OtherAgent,choice,_OtherChoice}->
      ok
  end,
  Agent!{self(),choose,_OtherName}, % first prisoner chooses the second prisoner
  receive
    {Agent,choice,_MyChoice}->
      ok
  end,
  OtherAgent!{self(),result,_MyChoice},
  receive
    {OtherAgent, result, _OtherScore} ->
      ok
  end,
  Agent!{self(),result,_OtherChoice},
  receive
    {Agent, result, _MyScore} ->
      ok
  end,
  ets:insert_new(summary,[{_MyName,Agent,0},{_OtherName,OtherAgent,0}]), %inserts into ets table first time round prisoner name, pid and score of 0, returns false thereafter as elements exist
  CurrentScore1 = retrieveSummaryScore(_MyName), %gets overall score for inmate MyName by sending the variable as a key to function
  CurrentScore2 = retrieveSummaryScore(_OtherName),
  MyNewScore = CurrentScore1 + _MyScore, % combines overall score with score of current round
  OthersNewScore = CurrentScore2 + _OtherScore, % combines overall score with score of current round
  ets:update_element(summary,_MyName,{3,MyNewScore}), %updates ets table with key 'MyName', assigns the 3rd positions value to MyNewScore
  ets:update_element(summary,_OtherName,{3,OthersNewScore}),
  doOnce(Agent,Rest,[{_MyName,_MyChoice,_OtherName,_OtherChoice}|History]).%update history


%%function to rank scores on sentence time, takes in a list of tuples and returns a sorted list of tuples based on score
rankScores(Scores) ->
  %% using lists sorting, the function swaps the order of the tuple contents as tuples automatically compare from first to last, then it sorts the list
  lists:sort(fun({KeyA,_PIDA,ValA}, {KeyB,_PIDB,ValB}) ->
                {ValA,_PIDA,KeyA} =< {ValB,_PIDB,KeyB}
             end, Scores).

%using the paramater Name as the key, returns current score
retrieveSummaryScore(Name) ->
  %gets score associated with the key 'Name' and puts a list containing a tuple with the Key and associated value ie score into variable
  ScoreFromETS = ets:lookup(summary, Name),
%%  pattern match the list with tuple inside to get the last element in the tuple
  RetrievedScore = case ScoreFromETS of
                    %% matches last element in tuple from ScoreFromETS
                    [{_,_,X}]->
                      X;
                     %% if the tuple is empty returns a score of 0
                     [] ->
                       0
                    end,
  RetrievedScore.

%%tests


%% the timerun test is created as a regular function for the reason that on high iterations with an eunit test
%% ie > 800 the test crashes
timerun(N) ->
  StartTime = erlang:timestamp(), % time started in microseconds
  WPID =warden:start({[],[]}), % create a warden process
  PApid = prisoner:create("prisonerA", random, []), % create a prisoner process
  PBpid = prisoner:create("prisonerB", titForTat, []),
  PCpid = prisoner:create("prisonerC", suspiciousTitForTat, []),
  PDpid = prisoner:create("prisonerD", titForTatRandom, []),
  PEpid = prisoner:create("prisonerE", alwaysCoop, []),
  PFpid = prisoner:create("prisonerF", alwaysDefect, []),
  warden:add(WPID,PApid), % add prisoners to list
  warden:add(WPID,PBpid),
  warden:add(WPID,PCpid),
  warden:add(WPID,PDpid),
  warden:add(WPID,PEpid),
  warden:add(WPID,PFpid),
  warden:run(WPID, N), % run N iterations
  EndTime = erlang:timestamp(), % gets end time in microseconds
  Microseconds = timer:now_diff(EndTime,StartTime), % gets the difference in Endtime and StartTime
  Seconds = Microseconds /1000000, % division by 1000000 to get it from microseconds to seconds
  ?debugFmt("Code time with ~p iterations = ~p seconds (~p microseconds)~n", [N,Seconds,Microseconds]).