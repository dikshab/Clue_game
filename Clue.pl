%@author Diksha Bansal

% Program finds probable guess by seeing the cards that have been guessed the most by players

% ------------ Call to start the program ------------
start :- init.
re_enter :- whosTurn.

%  The lists storing the respective data and their arities
:- dynamic totalPlayers/1.
:- dynamic presentRoom/1.

:- dynamic ownSuspects/1.
:- dynamic ownWeapons/1.
:- dynamic ownRooms/1.

:- dynamic knownSuspects/1.
:- dynamic knownWeapons/1.
:- dynamic knownRooms/1.

:- dynamic unknownSuspects/2.
:- dynamic unknownWeapons/2.
:- dynamic unknownRooms/2.


% ------------ Card Definitions ------------
% Define all the weapons
weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(leadpipe).
weapon(wrench).

% Define all the rooms
room(kitchen).
room(ballroom).
room(conservatory).
room(billiardroom).
room(library).
room(study).
room(hall).
room(lounge).
room(diningroom).

% Define all the suspects
suspect(colonelmustard).
suspect(missscarlet).
suspect(professorplum).
suspect(mrgreen).
suspect(mrswhite).
suspect(mrspeacock).

% ------------ Initialize the starting of the game ------------
init :- clearAll,
		initializeGame,
		msgPrompt,
		getTotalPlayers,
		getOwnCards.

% Completely clear game state
clearAll :- 		retractall(knownSuspects(_)),
					retractall(knownWeapons(_)),
					retractall(knownRooms(_)),
					retractall(ownSuspects(_)),
					retractall(ownWeapons(_)),
					retractall(ownRooms(_)),
					retractall(unknownSuspects(_,_)),
					retractall(unknownWeapons(_,_)),
					retractall(unknownRooms(_,_)),
					retractall(totalPlayers(_)).


% ------------ Initialize the starting game state to all cards unknown ------------
initializeGame :-  initSuspects, 
				   initWeapons, 
				   initRooms.

% Room Cards
initRooms :- 	assert(unknownRooms(diningroom, 0)),
			  assert(unknownRooms(conservatory, 0)),
			  assert(unknownRooms(kitchen, 0)),
			  assert(unknownRooms(lounge, 0)),
			  assert(unknownRooms(ballroom, 0)),
			  assert(unknownRooms(hall, 0)),
			  assert(unknownRooms(study, 0)),
			  assert(unknownRooms(billiardroom, 0)),
			  assert(unknownRooms(library, 0)).

% Weapon Cards
initWeapons :- 	
				assert(unknownWeapons(knife, 0)),
				assert(unknownWeapons(candlestick, 0)),
				assert(unknownWeapons(revolver, 0)),
				assert(unknownWeapons(leadpipe, 0)),
				assert(unknownWeapons(wrench, 0)),
				assert(unknownWeapons(rope, 0)).

% Suspect Cards
initSuspects :- 
				   assert(unknownSuspects(colonelmustard, 0)),
				   assert(unknownSuspects(missscarlet, 0)),
				   assert(unknownSuspects(mrgreen, 0)),
				   assert(unknownSuspects(mrswhite, 0)),
				   assert(unknownSuspects(professorplum, 0)),
				   assert(unknownSuspects(mrspeacock, 0)).


% ------------ Start Game Sequence ------------
msgPrompt :- 	write('Hi, I will help you win today\'s game of clue. For help, type "help." (without the ("") \n'),nl,
				helpDialogue,nl,
				write('Time to START!'), nl.

helpDialogue :- nl,
			 write('These are the suspects:\n'),
			 findall(X1,unknownSuspects(X1,_),L1),
			 printAll(L1),
			 write('These are the weapons:\n'),
			 findall(X2,unknownWeapons(X2,_),L2),
			 printAll(L2),
			 write('These are the rooms:\n'),
			 findall(X3,unknownRooms(X3,_),L3),
			 printAll(L3).

% Helper print function to print the entire list
printAll([]).
printAll([H|T]) :- write(H), nl,
				    printAll(T).
			 
% Get the total number of players
getTotalPlayers :- write('\nWhat is the total number of players playing? For example, type "3."'), nl,
				   read(TotalPlayers),
				   setPlayers(TotalPlayers).

% Set the total number of players
setPlayers(help) :- helpDialogue, getTotalPlayers.
setPlayers(TotalPlayers) :- assert(totalPlayers(TotalPlayers)).

% Get own cards
getOwnCards :- write('\nInput own cards:. For example "hall.". After entering all, type "done."' ), nl,
			 read(Card),
			 updateCard(Card).

% Determine whos turn 
whosTurn :- write('\nIs it our turn? (y/n)'),
             read(OurTurn),
             isOurTurn(OurTurn).

isOurTurn(help) :- helpDialogue, whosTurn.
isOurTurn(y)    :- myTurn.
isOurTurn(n)    :- write('\nPlease input player\'s turn, from your left?\n(0 to NumOpp - 1)\n'),
                     read(OppTurn),
                     oppTurn(OppTurn).
isOurTurn(_)    :- write('Please answer with y or n only. Try again:\n'), whosTurn.

% ------------ Updating the cards till typing "done" ------------
updateCard(done) :- whosTurn.
updateCard(help) :- helpDialogue, getOwnCards.

updateCard(Card) :- weapon(Card),
					 assert(ownWeapons(Card)),
				   	 assert(knownWeapons(Card)),
				     retractall(unknownWeapons(Card,_)),
				   	 getOwnCards.

updateCard(Card) :- room(Card),
					 assert(ownRooms(Card)),
				   	 assert(knownRooms(Card)),
				  	 retractall(unknownRooms(Card,_)),
				   	 getOwnCards.				   

updateCard(Card) :- suspect(Card),
					 assert(ownSuspects(Card)),
				   	 assert(knownSuspects(Card)),
				     retractall(unknownSuspects(Card,_)),
				   	 getOwnCards.

updateCard(_) :- write('Please input a valid card:'), nl, 
                  read(Card), updateCard(Card).

% updateUnknown to keep track of all unknown cards via everybodys guesses. 
% Each time a card is guessed its value is incremented and in the end the cards with the highest guesses will be our solutions
updateUnknown(Card) :- knownWeapons(Card).
updateUnknown(Card) :- knownRooms(Card).
updateUnknown(Card) :- knownSuspects(Card).

% Update values for unknown cards
updateUnknown(Card) :-  weapon(Card),
					 	unknownWeapons(Card,_),
					  	updateUnknownWeapons(Card).

updateUnknown(Card) :-  room(Card),
					  	unknownRooms(Card,_),
				      	updateUnknownRooms(Card).

updateUnknown(Card) :-  suspect(Card),
					  	unknownSuspects(Card,_),
				      	updateUnknownSuspects(Card).

updateUnknown(_) :- write('Please input a valid card:\n'), nl,
                     read(Card), updateUnknown(Card).

% Helper functions for updateUnknown(Card)

updateUnknownWeapons(Card) :- unknownWeapons(Card,X),
							 Y is X + 1,
							 retractall(unknownWeapons(Card,_)),
							 assert(unknownWeapons(Card, Y)).

updateUnknownSuspects(Card) :- unknownSuspects(Card,X),
							 Y is X + 1,
							 retractall(unknownSuspects(Card,_)),
							 assert(unknownSuspects(Card, Y)).

updateUnknownRooms(Card) :- unknownRooms(Card,X),
						     Y is X + 1,
						     retractall(unknownRooms(Card,_)),
						     assert(unknownRooms(Card, Y)).


% ------------ My Turn ------------
% Procedure for myTurn
myTurn :- write('\nAre you in a room?\n(y/n)'),
           read(InRoom),
           inRoomHelper(InRoom).

% Determine next step based on previous answer
inRoomHelper(help) :- helpDialogue, myTurn.
inRoomHelper(n)    :- closestRoom.
inRoomHelper(y)    :- whichRoom. 
inRoomHelper(_)    :- write('Please answer with y or n only. Try again:\n'), myTurn.

% Current player room
whichRoom :- write('\nWhich room? For Example kitchen.) \n'),
                        read(Room),
                        whichRoomHelper(Room).

whichRoomHelper(help) :- helpDialogue, whichRoom.
whichRoomHelper(Room) :- knownRooms(Room), alreadySeenRoom.
whichRoomHelper(Room) :- unknownRooms(Room,_),
							write('Were you pulled into this room?\n(y/n.)\n'),
							read(Pulled),
							pulledInto(Room, Pulled).
whichRoomHelper(_)    :- write('Please input a valid room\n'), whichRoom.

% Update presentRoom
pulledInto(Room,help) :- helpDialogue, whichRoomHelper(Room).
pulledInto(Room,y)    :- retractall(presentRoom(_)),
							   assert(presentRoom(Room)),
							   guessThis.
pulledInto(_,n) 	    :- exitRoom.

% Find closest room in case of already seen room
alreadySeenRoom :- write('This room has already been seen. \n'), 
							   closestRoom.

% Determine closest room
closestRoom :- write('\nWhat is the closest room?\n'),
                          read(ClosestRoom),
                          closestRoomHelper(ClosestRoom).

% Iterate over all rooms to find next unseen room and suggest it
closestRoomHelper(help)		  :- helpDialogue, closestRoom.
closestRoomHelper(ClosestRoom) :- knownRooms(ClosestRoom), nextClosestRoom.

closestRoomHelper(ClosestRoom) :- unknownRooms(ClosestRoom,_), 
									 retractall(presentRoom(_)),
									 assert(presentRoom(ClosestRoom)),
									 suggestRoom.

closestRoomHelper(_)    :- write('Please input a valid room:\n'), closestRoom.

% Next Closest Room Finder
nextClosestRoom :- write('\nThis room has been seen. Please input the next closest room.\n'),
                                    read(ClosestRoom),
                                    closestRoomHelper(ClosestRoom).

% Suggest a room to player
suggestRoom :- write('\nDid you go to suggested room?\n(y/n)'),
                        read(ToRoom),
                        suggestRoomHelper(ToRoom).

% Ends turn if did not go to suggested room, prompts to guess
suggestRoomHelper(help) :- helpDialogue, suggestRoom.
suggestRoomHelper(n) 	 :- endTurn.
suggestRoomHelper(y) 	 :- guessThis.
suggestRoomHelper(_) 	 :- write('Please answer with y or n only. Try again:\n'), suggestRoom.

% Applied the guessing the suspect logic
guessThis :- proabableGuess,
                winORlose.

% Determine whether win OR lose
winORlose :- write('\nDid you win?\n(y/n)'),
                         read(Win),
                         winORloseHelper(Win).

% Print success msg if player wins
winORloseHelper(help) :- helpDialogue, winORlose.
winORloseHelper(n) 	  :- inputCard.
winORloseHelper(y) 	  :- write('Congratulations, we won!'), nl, 
							 write('Wanna play again?'),nl, write('(y/n)'),
				   			 read(Play),!,
				   			 playAgain(Play).
winORloseHelper(_) 	  :- write('Please answer with y or n only. Try again:\n'), winORlose.

% Asks player to input card that was shown
inputCard :- write('\nInput shown card:\n'),
                        read(Card),
                        inputCardUpdate(Card).

% Updates the card that was shown
inputCardUpdate(help) :- helpDialogue, inputCard.

inputCardUpdate(Card) :- suspect(Card),
				   	 	   assert(knownSuspects(Card)),
				     	   retractall(unknownSuspects(Card,_)),
				     	   endTurn.

inputCardUpdate(Card) :- weapon(Card),
				   	 	   assert(knownWeapons(Card)),
				     	   retractall(unknownWeapons(Card,_)),
				     	   endTurn.

inputCardUpdate(Card) :- room(Card),
				   	 	   assert(knownRooms(Card)),
				  	 	   retractall(unknownRooms(Card,_)),
				  	 	   endTurn.	

inputCardUpdate(_) :- write('Please input a valid card:\n'),
                           inputCard.

% Informs of turn over and starts opponents turn
endTurn :- write('\nOur turn is up; now oppoents turn\n'), oppTurn.

% Promts user to exit current room but stay close 
% because we will need to re-enter it in the next turn.
exitRoom :- write('\nExit the room, but stay close!\n'),
                       endTurn.

% ------------ Guess Procedures ------------
% Write the guess for the player
proabableGuess :- guessSuspect(X),
			 guessRoom(Y),
			 guessWeap(Z),
			 write('\nGuess this: \n'),
			 printGuesses(X,Y,Z).

% Procedure to print guesses legibly
printGuesses(X,Y,Z) :- write('suspect: '),     write(X), write('\n'),
					   write('Room: '),        write(Y), write('\n'),
					   write('Weapon: '),      write(Z), write('\n').

% Always write players current room
guessRoom(Room) :- presentRoom(Room).

% Find most valued Suspect and guess it
guessSuspect(Char) :- findall(X, unknownSuspects(_,X),L),
				    max(L,Max),
					unknownSuspects(Char,Max).

% Similarly for weapon
guessWeap(Weap) :- findall(X, unknownWeapons(_,X),L),
					max(L,Max),
					unknownWeapons(Weap,Max).

% Return max of a list				
max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.


% ------------ Oppoents Turn ------------
% Keep looping through the program for each opponent
oppTurn    :- oppTurn(0).
oppTurn(X) :- totalPlayers(X),
					 write('\nYour turn to play!\n'),
					 myTurn.
oppTurn(X) :- write('\n\nIt is the '), write(X), write(' opponent\'s turn\n'),
					 Z is X + 1,
					 opponentAsk,
				     opponentWin,!,
				     oppTurn(Z). 

% Did the opponent guess?
opponentAsk :- write('\nDid your opponent guess?\n(y/n)'),
				read(Guess),
				opponentGuess(Guess).

% Did the opponent win the game?
opponentWin :- write('\nDid your opponent win the game?\n(y/n)'),
				read(Win), !,
				gameOver(Win).

% If the oppoent guessed one of our cards, we show it
opponentGuess(n).
opponentGuess(help) :- helpDialogue, opponentAsk.
opponentGuess(y) 	 :- write('What suspect did your opponent guess?\n'),
					  	read(Guess1),
					 	updateUnknown(Guess1),
					 	write('What room did they guess? \n'),
					 	read(Guess2),
					 	updateUnknown(Guess2),
					 	write('What weapon did they guess?\n'),
					 	read(Guess3),
					 	updateUnknown(Guess3),
					 	showCard(Guess1, Guess2, Guess3).

% Show card 
showCard(Guess1,_,_):- ownSuspects(Guess1),
		   				write('\nIf asked, show this Suspect: '),
		                write(Guess1), nl.

showCard(_,Guess2,_):- ownRooms(Guess2),
					    write('\nIf asked, show this Room: '),
						write(Guess2), nl.

showCard(_,_,Guess3):- ownWeapons(Guess3),
				  		write('\nIf asked, show this Weapon: '),
				   		write(Guess3), nl.

showCard(_,_,_) :- write('You do not have any cards to show'), nl.

% Display end game message
gameOver(n).
gameOver(help) :- helpDialogue, opponentWin.
gameOver(y)	:- write('\nWe lost!'), nl,
				   write('Wanna play again?'),nl, write('(y/n)'),
				   read(Play),!, playAgain(Play).

% Close program or restart game
playAgain(y) :- nl,start.
playAgain(n) :- halt.
playAgain(_) :- write('Please input either y or n'). 