# Texas_Poker_Dealer
A program that deals two 2-card poker hands and a shared five card pool according to the rules of Texas Holdem poker.
The program will determine the winning hand and return it. 

----

- The program will encode the input cards first due to "Aces" in Texas owns a rank of 13 rather than 1.
- The program will next deal cards to the two players and find their best hands.
    * Tie breaking for all circustances were considered and dealt with.
- The program will return the winner's best hand after decode the cards into normal form.

----

e.g. 
* input command: `winner = Poker.deal [1,12,23,34,51,16,27,38,49]`
* program output: winner: ["10S","12C","12H","12S","1H"]