﻿module Oware

open System.Diagnostics
open System.IO
open System.Diagnostics
open System.Diagnostics

type StartingPosition =
    | South
    | North

let getBoard state = // gets a list representing each house and the numbers of seeds inside 
    match state with 
    |b,pos,score -> b

let getTurn state = // gets the position of who is currently playing 
    match state with 
    |b,pos,score -> pos

let getScore state = // gets the value of the current score
    match state with 
    |b,pos,score -> score 

let nextTurn state = // changes the turn after a play
    match state with 
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

let distributeSeeds house state =
    let score = getScore state
    let origin = false // origin is used to determine whether or not we are on the original house and will therefore be used to skip it during sowing
    let originhouse = house
    let rec sow currentHouse house listi (newState,seeds) endingHouse origin =
        match listi with 
        |[] -> match (seeds>0) with
               |true -> let origin = true
                        sow 1 0 (List.rev (getBoard newState)) (([],(getTurn newState),(score)), seeds) endingHouse (origin)  //Reached the end of the list with more seeds to be distributed i.e only sowed into houses that were after the initial
               |_ -> let endingHouse = (13 - endingHouse) // the harvest function will iterate through the sown board backwards so the ending house position needs re - adjustment 
                     match checklegal (getTurn newState) (List.rev (getBoard newState)) with // Must play to give opponent pieces, if they have none 
                     | true -> ((getBoard state),getTurn state,getScore newState) //return original board before play, player has to use another hole
                                // some output for the user should go here i.e " You must play to give your opponent pieces"
                     | false -> let harvestedBoard = harvest ((getBoard newState)) ([],getTurn newState,getScore newState) 1 endingHouse false // harvest seeds regardless of whether or not they will remove pieces from opponents board 
                                match checkzero (getTurn harvestedBoard) (getBoard harvestedBoard) with // check if opponents board is empty 
                                | true -> (List.rev (getBoard newState),nextTurn newState,getScore newState) // return board before capture
                                | false -> harvestedBoard // return harvested board         
        |h::t -> match (currentHouse < house) with 
                 |true -> sow (currentHouse+1) house t (((h::(getBoard newState)),(getTurn state),(getScore newState)),seeds) endingHouse (origin)   //  Do nothing to the house
                 |_ -> match origin with
                       | true -> match originhouse = currentHouse with
                                 |true ->let origin = false // once set back to false will not be changed again
                                         sow (currentHouse+1) house t ((((h)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds)) endingHouse (origin) // skip original house when sowing 
                                 |false -> match (currentHouse = house) with
                                           |true ->match (h>0) with        //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newState)),(getTurn state),(getScore newState)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                           |_ ->match (currentHouse > house) with 
                                                |true ->match (seeds>0) with
                                                        |true -> match seeds=1 with 
                                                                    |true -> let endingHouse= currentHouse // this finds the value of the house that the last seed was dropped into 
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds)) endingHouse (origin)    //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
                       | false -> match (currentHouse = house) with // this branch is used when origin is false, mostly identical to code above
                                        |true ->match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newState)),(getTurn state),(getScore newState)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                        |_ -> match (currentHouse > house) with 
                                                |true ->match (seeds>0) with
                                                        |true -> match seeds=1 with
                                                                    |true -> let endingHouse= currentHouse
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list //try adding point here
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newState)),(getTurn state),(getScore newState)),(seeds)) endingHouse (origin)        //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
    match house>0 with 
    |true -> sow 1 house (getBoard state) (([],(getTurn state),(getScore state)) ,0) 0 false 
    |_ -> failwith "House does not exist"

let checkValid house position = // prevents the player from manipulating the opponents houses 
    match position with
    |South -> house<=6
    |North -> house>6

    //edit Harvest needs to be fixed to pass tests
let rec Harvest currentScore currentHouse lastSowedHouse canCapture state xs out =
    match xs with
    | [] -> ((List.rev out),(getTurn state),(currentScore))
    | head::tail -> match currentHouse < lastSowedHouse with
                    | true -> Harvest currentScore (currentHouse+1) lastSowedHouse canCapture state tail (head::out)
                    | _ ->  match (canCapture,head) with
                            | (true,2) | (true,3) ->  Harvest (applyPoint state head) (currentHouse+1) lastSowedHouse canCapture state tail (0::out)
                            | _ -> Harvest currentScore (currentHouse+1) lastSowedHouse false state tail (head::out)


let rec SOW index house seedsInHand isSowing state xs out =
    match seedsInHand=0 with
    | true -> match xs with
                | []->  match checklegal (getTurn state) (List.rev out) with // Must play to give opponent pieces, if they have none 
                        | true -> ((getBoard state),getTurn state,getScore state)
                        | false -> match (getTurn state , index<=6) with
                                    | (South,true) | (North,false) -> (List.rev out,nextTurn(state),(getScore state))
                                    | (South,false) | (North,true) -> let harvestedBoard = (Harvest (getScore state) 1 index true state (List.rev out) [])
                                                                      match checkzero (getTurn harvestedBoard) (getBoard harvestedBoard) with // check if opponents board is empty 
                                                                      | true -> (List.rev out,nextTurn state,getScore state) // return board before capture
                                                                      | false -> harvestedBoard
                | _ ->  (((List.rev out)@xs),nextTurn(state),(getScore state)) //Returns after sowing
    | _ -> match xs with
            | [] -> SOW 1 house seedsInHand isSowing state (List.rev out) []
            | head::tail -> match index=house with
                            | true -> SOW (index+1) house seedsInHand true state tail (0::out) 
                            | false ->  match isSowing with
                                        | true -> SOW (index+1) house (seedsInHand-1) isSowing state tail ((head+1)::out)
                                        |_ -> SOW (index+1) house seedsInHand isSowing state tail (head::out)
                                        
  
let useHouse house state =                                 
    match ((getSeeds house state)>0) with
    |true -> match (checkValid house (getTurn state)) with // check valid move - house number and position must match e.g. North cannot move from house 1
             | true ->  SOW 1 house (getSeeds house state) false state (getBoard state) []  //distributeSeeds house state
             | _ -> state
    |_ -> state
                   
let start position = 
   ([4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4],position,(0,0)) //creates an initial state of the board (board,position,(south score,north score))

let score state =  // gets the current score 
    getScore state

let gameState state =
    let s,n=getScore state
    match s<24 && n<24 with
    |true ->    match (getTurn state) with 
                        |South -> "South's turn"
                        |North -> "North's turn" 
    |false ->   match s=24 && n = 24 with
                |true -> "Game ended in a draw"
                |false -> match s>=25 && n <25 with
                          | true -> "South won"
                          | false -> match n>=25 && s<25 with
                                     | true -> "North won"
                                     | false -> failwith "Game outcome unknown"
    
[<EntryPoint>]
let main _ = 0 // return an integer exit code