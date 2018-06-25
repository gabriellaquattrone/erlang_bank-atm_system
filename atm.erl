-module(atm).
-export([loop/0]).

% Author: Gabriella Quattrone

% Operations to be performed will be sent from the console to this process.
% Bank processes will send messages to this process when necessary, and vice versa. 

% THIS PROCESS:
% does not know anything about the user accounts.
% cannot open new user accounts.
% can open up new ATMs with a set amount of cash supply:   {start,BANKPROCESS,BANKNAME,AMOUNT,ATMNAME}
% keeps track of how much cash is available in each ATM:   {cashsupply,ATMNAME} 
% allows users to deposit money into their bank accounts:  {deposit,USERNAME,AMOUNT,ATMNAME}
% allows users to withdraw money from their bank accounts: {withdraw,USERNAME,AMOUNT,ATMNAME}
% allows users to ask for their bank account balance:      {balance,USERNAME,ATMNAME}.

loop() ->
	receive
		{start, BANKPROCESS, BANKNAME, AMOUNT, ATMNAME} -> ets:new(ATMNAME, [private, named_table]), 
														   ets:insert(ATMNAME, {ATMNAME, BANKPROCESS, BANKNAME, AMOUNT}),
														   io:format("The atm ~p has started with ~p dollars cash available.~n", [ATMNAME, AMOUNT]),
								    					   loop();
		{cashsupply, ATMNAME} -> AMOUNT = ets:lookup_element(ATMNAME, ATMNAME, 4), % We look up the value since it doesn't get passed along.
								 io:format("The atm ~p has ~p dollars on hand.~n", [ATMNAME, AMOUNT]),
								 loop();
		{deposit, USERNAME, AMOUNT, ATMNAME} -> BANKPROCESS = ets:lookup_element(ATMNAME, ATMNAME, 2), 
												BANKNAME = ets:lookup_element(ATMNAME, ATMNAME, 3),
												BANKPROCESS ! {deposit, self(), USERNAME, AMOUNT, BANKNAME}, % Send a message to the bank process to do the rest.
												loop();
		{withdraw, USERNAME, AMOUNT, ATMNAME} -> BANKPROCESS = ets:lookup_element(ATMNAME, ATMNAME, 2), % None of this gets passed into this function,
												 BANKNAME = ets:lookup_element(ATMNAME, ATMNAME, 3),    % so we have to look it up again.
												 CASHSUPPLY = ets:lookup_element(ATMNAME, ATMNAME, 4),
												 NEWCASHSUPPLY = CASHSUPPLY - AMOUNT,
												 case {CASHSUPPLY >= 0} of
												 	{true} -> case {NEWCASHSUPPLY >= 0} of
												 				{true} ->  BANKPROCESS ! {withdraw, self(), USERNAME, AMOUNT, BANKNAME},
												 						   receive
												 						   		{BANKPROCESS, true, _} -> ets:insert(ATMNAME, {ATMNAME, BANKPROCESS, BANKNAME, NEWCASHSUPPLY}),
												 						   								  loop();
												 						   		{BANKPROCESS, false, _} -> loop()
												 						   end; 
												 				{false} -> io:format("Sorry, there is insufficient cash in this atm.~nTry a different atm.~n"),
												 						   loop()
												 			   end;
												 	{false} -> io:format("This atm has no more money.~nTry a different atm.~n"),
												 			   loop()
												 end;
		{balance, USERNAME, ATMNAME} -> BANKPROCESS = ets:lookup_element(ATMNAME, ATMNAME, 2),
									    BANKNAME = ets:lookup_element(ATMNAME, ATMNAME, 3),
									    BANKPROCESS ! {balance, USERNAME, BANKNAME},  % Only the bank process knows user's balances. Send it a message.
									    loop()
    end.
