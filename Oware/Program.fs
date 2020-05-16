module Oware

open System.Diagnostics
open System.IO
open System.Diagnostics
open System.Diagnostics

type StartingPosition =
    | South
    | North

let getBoard board = // gets a list representing each house and the numbers of seeds inside 
    match board with 
    |b,pos,score -> b

let getTurn board = // gets the position of who is currently playing 
    match board with 
    |b,pos,score -> pos

let getScore board = // gets the value of the current score
    match board with 
    |b,pos,score -> score 

let nextTurn board = // changes the turn after a play
    match board with 
    |b,pos,score -> match pos with 
                    |South -> North
                    |North -> South

let getSeeds n state =
    let board= getBoard state 
    match board with
    |[]-> failwith "Board cannot be empty"
    | _-> List.fold( fun (i,value) item -> i+1,match i=n with |true -> Some item |_ ->value) (1,None) board |>
          fun (_,x)-> match x with //iterates through whole board looking to see if the house exists and returns it seeds if it does
                      | None-> failwith "House number doesn't exist"
                      | Some seeds-> seeds

let print a = sprintf "%A" a

let applyPoint state num = // adds points to the score after a capture 
    let s,n= getScore state
    match (getTurn state) with
    |South -> (s+num,n) 
    |North -> (s,n+num)

let rec checkzero turn list = //this function returns true if capturing would remove all the pieces on the opponenets board
    match list with
    | [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] -> false //match case to catch special case where a draw would prevent capture of final seeds
    | _ -> match (turn) with 
            |South -> match list with
                      | [0; 0; 0; 0; 0; 0; _; _; _; _; _; _] -> true // next player's turn with no capture
                      | _ -> false // can proceed to capture
            |North -> match list with 
                      | [_; _; _; _; _; _; 0; 0; 0; 0; 0; 0] -> true // next player's turn with no capture
                      | _ -> false // can proceed to capture 

let rec checklegal turn list = //this function is used to check if the players move will keep the game in motion i.e make sure they make a play to give the opponent a piece if they have none 
    match list with
    | [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] -> false //match case to catch special case where a draw would lead to erroneous data
    | _ -> match (turn) with 
            |South -> match list with
                      | [_; _; _; _; _; _; 0; 0; 0; 0; 0; 0] -> true //opponents houses are empty 
                      | _ -> false // valid move 
            |North -> match list with 
                      | [0; 0; 0; 0; 0; 0; _; _; _; _; _; _] -> true //opponents houses are empty
                      | _ -> false // valid move
   
let rec harvest board nuBoard currenthouse endinghouse switch = // this function does the seed capturing 
    match (board) with
    | [] -> ((getBoard nuBoard)),(nextTurn nuBoard),(getScore nuBoard) // return the new board created that has seeds captured and score increased
    | h::t -> match getTurn nuBoard with
              | North -> match endinghouse >= 7 && endinghouse <=12 with   //make sure seeds are captured from the right side
                         |true ->  match (currenthouse<endinghouse) with
                                    |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch // continue iterating through the list  and adding values to new list until ending house is reached
                                    |false -> match (currenthouse = endinghouse) with   // the endinghouse value is determined by the distributeSeed function allowing this function to know where to start capturing seeds
                                              | true -> match h with 
                                                        | 3|2 -> let score = applyPoint nuBoard h // if the ending house value is 2 or 3 add the captured seeds to the score
                                                                 let switch = true   // switch prevents contigous seeds that are not 2 or 3 from being captured 
                                                                 harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch // add zero to the new list to represent seeds from that house have been captured
                                                        | _ ->  let switch = false   // switch set to false means that the ending house was not a 2 or 3 therefore no captures will be done 
                                                                harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                              | false -> match (currenthouse>endinghouse) with   // the section that follows is used to capture contigous seeds 
                                                         | true -> match switch with   // if the switch is set to true it means that the seeds in the previous house have been captured 
                                                                    |true -> match h with
                                                                             | 3|2 -> let score = applyPoint nuBoard h
                                                                                      let switch = true
                                                                                      harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                                             | _ -> let switch = false
                                                                                    harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                                    |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                         | false -> failwith "Something probably went wrong with (currenthouse > endinghouse)"
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
              | South -> match endinghouse >= 1 && endinghouse <=6 with // code identical to North condition except for a few points 
                         |true ->  match (currenthouse<endinghouse) with
                                    |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                    |false -> match (currenthouse = endinghouse) with
                                              | true -> match h with
                                                        | 3|2 -> let score = applyPoint nuBoard h
                                                                 match endinghouse + 1 <> 7 with // this prevents the rare case that this function allows the south player to capture from house 6
                                                                 | true -> let switch = true
                                                                           harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                                 | false -> let switch = false
                                                                            harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                        | _ -> let switch = false
                                                               harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                              | false -> match (currenthouse>endinghouse) with
                                                         | true -> match switch with 
                                                                    |true -> match h with
                                                                            | 3|2 -> let score = applyPoint nuBoard h
                                                                                     let switch = true
                                                                                     harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                                            | _ -> let switch = false
                                                                                   harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                                    |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                         | false -> failwith "Something probably went wrong with (currenthouse > endinghouse)"
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch

let distributeSeeds house board =
    let score = getScore board
    let origin = false // origin is used to determine whether or not we are on the original house and will therefore be used to skip it during sowing
    let originhouse = house
    let rec sow currentHouse house listi (newBoard,seeds) endingHouse origin =
        match listi with 
        |[] -> match (seeds>0) with
               |true -> let origin = true
                        sow 1 0 (List.rev (getBoard newBoard)) (([],(getTurn newBoard),(score)), seeds) endingHouse (origin)  //Reached the end of the list with more seeds to be distributed i.e only sowed into houses that were after the initial
               |_ -> let endingHouse = (13 - endingHouse) // the harvest function will iterate through the sown board backwards so the ending house position needs re - adjustment 
                     match checklegal (getTurn newBoard) (List.rev (getBoard newBoard)) with // Must play to give opponent pieces, if they have none 
                     | true -> ((getBoard board),getTurn board,getScore newBoard) //return original board before play, player has to use another hole
                                // some output for the user should go here i.e " You must play to give your opponent pieces"
                     | false -> let harvestedBoard = harvest ((getBoard newBoard)) ([],getTurn newBoard,getScore newBoard) 1 endingHouse false // harvest seeds regardless of whether or not they will remove pieces from opponents board 
                                match checkzero (getTurn harvestedBoard) (getBoard harvestedBoard) with // check if opponents board is empty 
                                | true -> (List.rev (getBoard newBoard),nextTurn newBoard,getScore newBoard) // return board before capture
                                | false -> harvestedBoard // return harvested board         
        |h::t -> match (currentHouse < house) with 
                 |true -> sow (currentHouse+1) house t (((h::(getBoard newBoard)),(getTurn board),(getScore newBoard)),seeds) endingHouse (origin)   //  Do nothing to the house
                 |_ -> match origin with
                       | true -> match originhouse = currentHouse with
                                 |true ->let origin = false // once set back to false will not be changed again
                                         sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin) // skip original house when sowing 
                                 |false -> match (currentHouse = house) with
                                           |true ->match (h>0) with        //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newBoard)),(getTurn board),(getScore newBoard)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                           |_ ->match (currentHouse > house) with 
                                                |true ->match (seeds>0) with
                                                        |true -> match seeds=1 with 
                                                                    |true -> let endingHouse= currentHouse // this finds the value of the house that the last seed was dropped into 
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin)    //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
                       | false -> match (currentHouse = house) with // this branch is used when origin is false, mostly identical to code above
                                        |true ->match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newBoard)),(getTurn board),(getScore newBoard)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                        |_ -> match (currentHouse > house) with 
                                                |true ->match (seeds>0) with
                                                        |true -> match seeds=1 with
                                                                    |true -> let endingHouse= currentHouse
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list //try adding point here
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin)        //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
    match house>0 with 
    |true -> sow 1 house (getBoard board) (([],(getTurn board),(getScore board)) ,0) 0 false 
    |_ -> failwith "House does not exist"

let checkValid house position = // prevents the player from manipulating the opponents houses 
    match position with
    |South -> house<=6
    |_ -> house>6

let useHouse n board = 
    match ((getSeeds n board)>0) with
    |true -> 
                match (checkValid n (getTurn board)) with // check valid move - house number and position must match e.g. North cannot move from house 1
                |true -> distributeSeeds n board
                |_ -> board
    |_ -> board
                   
let start position = 
   ([4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4],position,(0,0))

let score board =  // gets the current score 
    getScore board

let gameState board =
    match (getScore board) with 
     | s,n -> match s<24 && n<24 with
              |true -> match (getTurn board) with 
                        |South -> "South's turn"
                        |North -> "North's turn" 
              |false -> match s=24 && n = 24 with
                        |true -> "Game ended in a draw"
                        |false -> match s>=25 && n <25 with
                                  | true -> "South won"
                                  | false -> match n>=25 && s<25 with
                                             | true -> "North won"
                                             | false -> failwith "Game outcome unknown"
    
[<EntryPoint>]
let main _ = 0 // return an integer exit code
