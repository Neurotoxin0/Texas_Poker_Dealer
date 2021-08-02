module Poker where
    import Data.List

    -- deal
    deal input = do
        -- re-code cards' rank: ace -> largest; others -> rank - 1
        let cards = encode input
        
        -- pick out common cards: cards @ index [5..9]   
        let common = drop 4 cards

        -- add player1&2's cards
        let p1 = sort (common ++ [cards !! 0, cards !! 2])
        let p2 = sort (common ++ [cards !! 1, cards !! 3])

        -- find players' best hands
        ---- return forrmat: (highest_point, best_cards)
        ------highest_point: range: (1-10); rotal flush = 10, high card = 1
        let p1_point = findbest p1
        let p2_point = findbest p2

        if fst p1_point > fst p2_point then output (snd p1_point)
        else if fst p1_point < fst p2_point then output (snd p2_point)
        else output(tiebreaking p1_point p2_point)


------------------------------------------------
    -- Main methods

    -- Output
    output cards = do
        let processed = isolate(decode cards)
        let clubs       = map (\card -> show(rank_decode card)++"C") (processed !! 0)
        let diamonds    = map (\card -> show(rank_decode card)++"D") (processed !! 1)
        let hearts      = map (\card -> show(rank_decode card)++"H") (processed !! 2)
        let spades      = map (\card -> show(rank_decode card)++"S") (processed !! 3)
        
        sort(clubs ++ diamonds ++ hearts ++ spades)

    
    -- Encode & Decode
    encode input = [encode_process rank | rank <- input]
    decode input = [decode_process rank | rank <- input]

    encode_process rank = 
        case rank of
            1  -> 13        -- Case: Aces
            14 -> 26
            27 -> 39
            40 -> 52
            _  -> rank - 1  -- Case: not Ace cards

    decode_process rank = 
        case rank of
            13  -> 1
            26 -> 14
            39 -> 27
            52 -> 40
            _  -> rank + 1
    

    -- Find Best Hand
    findbest cards = royalflush cards -- chain reaction was used


    -- Royal Flush
    royalflush cards = do
        let tmp1 = sequential cards 5
        let result = preferential tmp1
        
        if (length result /= 0) then do
            if rank_decode (head result) == 9   -- cards with rank 10 in this program will be showed as rank 9 [see "encode" above]
            then (10, result)
            else straightflush cards
        else straightflush cards 

    
    -- Straight Flush
    straightflush cards = do
        let tmp1 = sequential cards 5
        let result = preferential tmp1
        
        if length result /= 0 then (9, result)
        else foak cards


    -- Four of A Kind
    foak cards = do
        let tmp1 = preferential (samerank cards 4)
        let tmp2 = singlecard (cards \\ tmp1)
        let result = sort(tmp1 ++ [tmp2])
        
        if length tmp1 /= 0 then (8, result)
        else fullhouse cards
        

    -- Full House
    fullhouse cards = do
        let tmp1 = samerank cards 3   -- 3 cards with same rank
        let tmp2 = preferential tmp1
        
        let tmp3 = cards \\ tmp2  -- disclude chosen cards(line above)
        let tmp4 = preferential (samerank tmp3 2)   -- 2 cards with same rank

        if (length tmp2 /= 0) && (length tmp4 /= 0) then do
            let result = sort(tmp2 ++ tmp4)
            (7, result)
        else flush cards
            
    
    -- Flush
    flush cards = do
        let tmp1 = isolate cards
        let tmp2 = map (\suit -> length suit) tmp1
        let tmp3 = maximum tmp2
        
        if tmp3 >= 5 then do
            let index = head(elemIndices tmp3 tmp2)
            let tmp2 = reverse(tmp1 !! index)

            (6, take 5 tmp2)    -- pick largest 5
        
        else straight cards
        

    -- Straight
    straight cards = do
        let tmp1 = sort(map (\card -> rank_decode card) cards)
        let tmp2 = sequential tmp1 5

        if length tmp2 /= 0 then do
            let tmp3 = preferential tmp2
            let result = sort(map (\rank -> rank_encode rank cards) tmp3)
            (5, result)
        
        else toak cards

    
    -- Three of A Kind
    toak cards = do
        let tmp1 = preferential (samerank cards 3)
        let tmp2 = singlecard (cards \\ tmp1)
        let tmp3 = singlecard ((cards \\ tmp1) \\ [tmp2])

        if length tmp1 /= 0 then do
            let result = sort(tmp1 ++ [tmp2] ++ [tmp3])
            (4, result) 
        else twopairs cards


    -- Two Pairs
    twopairs cards = do
        let tmp1 = preferential (samerank cards 2)
        let tmp2 = preferential (samerank (cards \\ tmp1) 2)
        let tmp3 = singlecard ( (cards \\ tmp1) \\ tmp2 )

        if (length tmp1 /= 0) && (length tmp2 /= 0) then do
            let result = sort(tmp1 ++ tmp2 ++ [tmp3])
            (3, result)
        else pair cards


    -- Pair
    pair cards = do
        let tmp1 = preferential (samerank cards 2)
        let tmp2 = singlecard (cards \\ tmp1)
        let tmp3 = singlecard ( (cards \\ tmp1) \\ [tmp2] )
        let tmp4 = singlecard ( ((cards \\ tmp1) \\ [tmp2]) \\ [tmp3] )

        if (length tmp1 /= 0) then do
            let result = sort(tmp1 ++ [tmp2] ++ [tmp3] ++ [tmp4])
            (2, result)
        else highcard cards


    -- High Card
    highcard cards = do
        let tmp1 = map (\card -> rank_decode card) cards
        let tmp2 = reverse (sort tmp1)
        let result = sort(take 5 tmp2)
        (1, result)


------------------------------------------------
    -- Tie Breaking
    tiebreaking p1_point p2_point = do
        let point = fst p1_point    -- either of them; since tie -> same
        let card1 = snd p1_point
        let card2 = snd p2_point

        case point of
            9 -> tie_straightflush card1 card2
            8 -> tie_foak card1 card2
            7 -> tie_fullhouse card1 card2
            6 -> tie_flush card1 card2
            5 -> tie_straight card1 card2
            4 -> tie_toak card1 card2
            3 -> tie_twopairs card1 card2
            2 -> tie_pair card1 card2
            1 -> tie_highcard card1 card2   -- really bad luck, huh?
            _ -> [] -- catch

       
    tie_straightflush card1 card2 = do
        let tmp1 = rank_decode(head card1)
        let tmp2 = rank_decode(head card2)

        if tmp1 > tmp2 then card1
        else card2
        
    tie_foak card1 card2 = do
        let tmp1 = head( head(samerank card1 4) )
        let tmp2 = rank_decode tmp1
        let tmp3 = head( head(samerank card2 4) )
        let tmp4 = rank_decode tmp3

        if tmp2 > tmp4 then card1
        else card2

    tie_fullhouse card1 card2 = do
        let tmp1 = head( head(samerank card1 3) )
        let tmp2 = rank_decode tmp1
        let tmp3 = head( head(samerank card2 3) )
        let tmp4 = rank_decode tmp3

        if tmp2 > tmp4 then card1
        else card2

    tie_flush card1 card2 = do
        let tmp1 = rank_decode(head (reverse card1))
        let tmp2 = rank_decode(head (reverse card2))

        if tmp1 > tmp2 then card1
        else card2
            
    tie_straight card1 card2= do
        let tmp1 = map (\card -> rank_decode card) card1
        let tmp2 = head(sort tmp1)
        let tmp3 = map (\card -> rank_decode card) card2
        let tmp4 = head(sort tmp3)

        if tmp2 > tmp4 then card1
        else card2

    tie_toak card1 card2 = do
        let tmp1 = head( head(samerank card1 3) )
        let tmp2 = rank_decode tmp1
        let tmp3 = head( head(samerank card2 3) )
        let tmp4 = rank_decode tmp3

        if tmp2 > tmp4 then card1
        else card2

    tie_twopairs card1 card2 = do
        let tmp1 = samerank card1 2         -- get two pair
        let tmp2 = tmp1 !! 0                -- first pair
        let tmp3 = tmp1 !! 1                -- second pair
        let c1p1 = rank_decode (head tmp2)  -- rank 1
        let c1p2 = rank_decode (head tmp3)  -- rank 2
        let tmp1 = sort [c1p1,c1p2]         -- sort two pairs
        let c1large = tmp1 !! 0             -- card1's biggest pair rank
        let c1small = tmp1 !! 1             -- smaller pair
        let c1single = head( (card1 \\ tmp2) \\ tmp3 )  -- the single card

        let tmp1 = samerank card2 2         
        let tmp2 = tmp1 !! 0                
        let tmp3 = tmp1 !! 1                
        let c2p1 = rank_decode (head tmp2)  
        let c2p2 = rank_decode (head tmp3)  
        let tmp1 = sort [c2p1,c2p2]         
        let c2large = tmp1 !! 0             
        let c2small = tmp1 !! 1
        let c2single = head( (card2 \\ tmp2) \\ tmp3 ) 

        if c1large > c2large then card1
        else if c1large < c2large then card2
        else do
            if c1small > c2small then card1
            else if c1small < c2small then card2
            else do
                if c1single > c2single then card1
                else if c1single < c2single then card2
                else [] -- really? exactly same??

    tie_pair card1 card2 = do
        let tmp1 = head(samerank card1 2)                         
        let c1pair = rank_decode (head tmp1)  
        let c1single = card1 \\ tmp1

        let tmp1 = head(samerank card2 2)                         
        let c2pair = rank_decode (head tmp1)  
        let c2single = card2 \\ tmp1

        if c1pair > c2pair then card1
        else if c1pair < c2pair then card2
        else do
            if c1single > c2single then card1
            else card2

    tie_highcard card1 card2 = do
        if card1 > card2 then card1
        else card2


------------------------------------------------
    -- Other methods

    -- Remove element from list [Implemented but not used]
    rm _[] = []
    rm element (h:t) 
        | h == element = rm element t
        | otherwise = h : rm element t

    
    -- Deduplicate element from list
    deduplicate []  = []
    deduplicate [elem] = [elem]
    deduplicate (elem1:elem2:t)
        | elem1 == elem2  = deduplicate (elem1:t)
        | otherwise = elem1 : deduplicate (elem2:t)


    -- Decode Rank
    --- return actual rank for given cards
    rank_decode card =
        if card <= 13 then card     -- for fun :)
        else if card <= 26 then card - 13
        else if card <= 39 then card - 26
        else card - 39


    -- Encode Rank
    --- return card(s) that exist in given "cards" with given "decoded rank(1-13)"
    rank_encode rank cards = do
        let tmp1 = filter (\card -> mod(card-rank) 13 == 0) cards
        head tmp1


    --- Isolate cards by suit
    isolate cards = do
        -- Clubs
        let clubs = sort(filter (<=13) cards)
        let tmp1 = cards \\ clubs
        -- Diamonds
        let diamonds = sort(filter (<=26) tmp1)
        let tmp2 = tmp1 \\ diamonds
        -- Hearts
        let hearts = sort(filter (<=39) tmp2)
        let tmp1 = tmp2 \\ hearts
        -- Spades
        let spades = sort(filter (<=52) tmp1)

        [clubs,diamonds,hearts,spades]


    
    
    -- Find Continous cards
    sequential cards num = do
        let result = []
        let tmp1 = isolate cards -- isolate cards by suit
        
        -- process clubs
        let tmp2 = tmp1 !! 0
        let tmp3 = result ++ sequential_check tmp2 num
        let result = tmp3
        
        -- process diamonds
        let tmp2 = tmp1 !! 1
        let tmp3 = result ++ sequential_check tmp2 num
        let result = tmp3

        -- process hearts
        let tmp2 = tmp1 !! 2
        let tmp3 = result ++ sequential_check tmp2 num
        let result = tmp3

        -- process spades
        let tmp2 = tmp1 !! 3
        let tmp3 = result ++ sequential_check tmp2 num
        let result = tmp3

        result
    --- For the next two sub functions:
    ---- check all elements in the given list to see if meets needs (# of sequential cards)
    ------ iter over every element, creating a list in range of (the current processing element + num)
    ------ if the created list is a subset of cards: do have all required element -> do have # of sequential cards -> meet needs
    ------- return sequential cards
    ---
    ---- Sub function 1 for "sequential": return a list contain True of False
    ------True for the index element will meet needs
    sequential_check cards num = do
        let tmp1 = map (\card -> isSubsequenceOf [card..card+num-1] cards) cards
        sort(sequential_output cards tmp1 num)
    
    ---- Sub function 2 for "sequential": take True & False list from sub 1 and return sequential cards that meet needs
    sequential_output cards check num = do
        let tmp1 = elemIndices True check   -- return index of cards that meet needs
        if length tmp1 == 0 then []
        else if length tmp1 == 1 then [[(cards !! (head tmp1))..(cards !! (head tmp1) + num - 1)]]
        else [[(cards !! (last tmp1))..(cards !! (last tmp1)) + num - 1]] -- pick the largest rank
    



    -- Find cards with same rank - double recursion
    --samerank :: [Int] -> Int -> [[Int]]
    samerank cards num = do
        let tmp1 = map (\card1 -> samerank_sub cards card1 num) (sort cards)
        let tmp2 = deduplicate (sort tmp1)
        let result = tmp2 \\ [[-1]]
        sort(result)
    
    --samerank_sub :: [Int] -> Int -> Int -> [Int]
    samerank_sub cards card1 num = do
        let count = map (\card2 -> samerank_count card1 card2) cards

        if sum count >= num then    -- should consider > or == seperately, but situation like that is not likely to happen
            -- rank_decode (rank_encode card1) cards    -- next line is simplier here; replaced
            filter (\card -> mod(abs(card1-card)) 13 == 0) cards
            
        else [-1]

    --samerank_count :: Int -> Int -> Int
    samerank_count card1 card2 = do
        if mod(abs(card1-card2)) 13 == 0 then 1
        else 0


    
    
    -- Preferential selection
    preferential results = do
        if length results == 1 then (head (sort results))
        else if length results > 1 then do   -- return the largest
            let tmp1 = map (\card -> head card) results
            let tmp2 = map (\card -> rank_decode card) tmp1 -- translate cards into real rank
            let index = elemIndices (maximum tmp2) tmp2  -- index of the largest
            
            sort(results !! (head index))
        
        else ([])
    

    -- Find Single Card
    ----return the largest one
    singlecard cards = do
        let tmp1 = map (\card -> rank_decode card) (sort cards)
        rank_encode (maximum tmp1) cards
