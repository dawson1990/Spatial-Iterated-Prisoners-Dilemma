%%%-------------------------------------------------------------------
%%% @author Kevin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2016 16:41
%%%-------------------------------------------------------------------
-module(prisoner).
-author("Kevin").

%% API
-export([create/3, getName/1, getStrategy/1, getState/1,alwaysCoop/2,alwaysDefect/2,titForTat/2,getChoice/1,suspiciousTitForTat/2,random/2]).

create(Name, Strategy, State) ->
  PID = spawn(?MODULE, Strategy, [Name,State]),
  PID.

getName(PrisonerPID) ->
  PrisonerPID!{self(), name},
  receive
    {PrisonerPID,name, Name} ->
      Name
  end.

getChoice(PrisonerPID) ->
  PrisonerPID!{self(), choose, kevin},
  receive
    {PrisonerPID,choice, Choice} ->
      Choice
  end.


getStrategy(PrisonerPID) ->
  PrisonerPID!{self(), strategy},
  receive
    {PrisonerPID,strategy, Strategy } ->
      Strategy
  end.

getState(PrisonerPID) ->
  PrisonerPID!{self(), state},
  receive
    {PrisonerPID,state,State} ->
      State
  end.


titForTat(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,titForTat};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      MyChoice = case HasDealtWith of
                   [{_,"defected",_}|_] ->
                     "defected";
                   _->
                      "cooperated"
                 end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          titForTat(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  titForTat(Name,State).

suspiciousTitForTat(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,titForTat};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      HasDealtWith = lists:filter(
        fun(Turn) ->
          case Turn of
            {PartnerName,_,_} ->
              true;
            _ ->
              false
          end
        end,State),
      MyChoice = case HasDealtWith of
                     [{_,"cooperated",_}|_] ->
                       "cooperated";
                     _->
                       "defected"
                 end,
      Sender!{self(),choice,MyChoice},
      receive
        {Sender, result, TheirChoice} ->
          titForTat(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  titForTat(Name,State).

alwaysDefect(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysDefect};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      MyChoice = "defected",
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result,TheirChoice} ->
          alwaysDefect(Name,[{PartnerName,TheirChoice,MyChoice}])
      end
  end,
  alwaysDefect(Name,State).

alwaysCoop(Name,State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysCoop};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      MyChoice = "cooperated",
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result, TheirChoice}->
          alwaysCoop(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  alwaysCoop(Name,State).

random(Name, State) ->
  receive
    {Sender, name} ->
      Sender!{self(),name, Name};
    {Sender, strategy} ->
      Sender!{self(),strategy,alwaysCoop};
    {Sender, state} ->
      Sender!{self(),state, State};
    {Sender,choose, PartnerName}->
      ChoicesList = ["cooperated","defected"],
      Choice = rand:uniform(2),
      MyChoice = lists:nth(Choice, ChoicesList),
      Sender!{self(),choice,MyChoice},
      receive
        {Sender,result, TheirChoice}->
          random(Name,[{PartnerName,TheirChoice,MyChoice}|State])
      end
  end,
  random(Name,State).












