-module(bank).
-export([loop/0]).

% Author: Gabriella Quattrone

% Operations to be performed will be sent from the console to this process.
% ATM processes will send messages to this process when necessary, and vice versa. 

% THIS PROCESS:
% - keeps track of user account information.  
% - does not know how much cash is in any ATM.
% - can create banks with initialized accounts: {create, [{USERNAME,AMOUNT},...,{USERNAME,AMOUNT}], BANKNAME}
% - can tell a user their balance:              {balance, USERNAME, BANKNAME}
% - can open new accounts:                      {open, USERNAME, AMOUNT, BANKNAME}
% - can respond to deposit requests:            {deposit, ATMPROCESS, USERNAME, AMOUNT, BANKNAME}
% - can respond to withdrawal requests:         {withdraw, ATMPROCESS, USERNAME, AMOUNT, BANKNAME}


loop() ->
	receive
		{create, LIST, BANKNAME} -> ets:new(BANKNAME, [private, named_table]), % Create a new table with the bank name as its name.
								    lists:map(fun(I) -> ets:insert(BANKNAME, I) end, LIST), % Inserts a list of accounts into the bank as tuples.
								    io:format("The bank ~p was created.~n", [BANKNAME]), 
								    loop();

		{balance, USERNAME, BANKNAME} -> KEY = ets:lookup(BANKNAME, USERNAME), % We need this to check the amount since it does not get passed.
									     AMOUNT = element(2, hd(KEY)),
										 io:format("Account for ~p has ~p dollars.~n", [USERNAME, AMOUNT]),
										 loop();

		{open, USERNAME, AMOUNT, BANKNAME} -> N = {USERNAME, AMOUNT}, % We need this tuple to be able to insert a new account into the table.
											  ets:insert(BANKNAME, N),
											  io:format("A new account for ~p was opened with ~p dollars!~n", [USERNAME, AMOUNT]),
											  loop();

		{deposit, ATMPROCESS, USERNAME, AMOUNT, BANKNAME} -> KEY = ets:lookup(BANKNAME, USERNAME), 
										 				     OLDAMOUNT = element(2, hd(KEY)),
															 NEWAMOUNT = OLDAMOUNT + AMOUNT,
															 ets:insert(BANKNAME, {USERNAME, NEWAMOUNT}), % Inserting overwrites the old value.
														     ATMPROCESS ! {self(), io:format("Account for ~p now has ~p dollars.~n", [USERNAME, NEWAMOUNT])},
														 	 loop();
		{withdraw, ATMPROCESS, USERNAME, AMOUNT, BANKNAME} -> KEY = ets:lookup(BANKNAME, USERNAME),
															  OLDAMOUNT = element(2, hd(KEY)),
															  case {OLDAMOUNT >= AMOUNT} of
															  	{true} ->  NEWAMOUNT = OLDAMOUNT - AMOUNT, 
															  			   ets:insert(BANKNAME, {USERNAME, NEWAMOUNT}), 
															  			   ATMPROCESS ! {self(), true, io:format("~p dollars have been withdrawn from ~p's account. ~nThe account for ~p now has ~p dollars.~n",[AMOUNT, USERNAME, USERNAME, NEWAMOUNT])},
															  			   loop();
															  	{false} -> ATMPROCESS ! {self(), false, io:format("Sorry, the account for ~p only has ~p dollars.~n", [USERNAME, OLDAMOUNT])},
															  			   loop()		    
															  end
	end.