module Oware

open System.Diagnostics
open System.IO
open System.Diagnostics
open System.Diagnostics

type StartingPosition =
    | South
    | North

let getBoard board =
    match board with 
    |b,pos,score -> b

let getTurn board =
    match board with 
    |b,pos,score -> pos

let getScore board =
    match board with 
    |b,pos,score -> score 

let nextTurn board =
    match board with 
    |b,pos,score -> match pos with 
                    |South -> North
                    |North -> South

let getSeeds house board =  //Calls getSpecific  For later when need to check stuff(listi,SouthPoints,NorthPoints) as board 
    let rec seeds currentHouse houseLookingFor board =
       match board with
            |[] -> failwith "The house chosen is not on the board (line 33: getSeeds)"
            |head::tail -> match currentHouse = houseLookingFor with 
                           |true -> head
                           |_ -> seeds (currentHouse+1) houseLookingFor tail
    match (house>0),(house<13) with        //checking seed pos actually exists
    |true,true -> seeds 1 house (getBoard board)
    |_ -> failwith "position not available (line 22: getSeeds)"

let print a = sprintf "%A" a

let applyPoint board num =
    match (getScore board) with 
    |s,n -> 
            match (getTurn board) with
            |South -> ((s+num),n)
            |North -> (s,(n+num))

let distributeSeeds house board =
    let rec sow currentHouse house listi score (newBoard,seeds) =
        match listi with 
        |[] -> match (seeds>0) with
               |true -> sow 1 0 (List.rev (getBoard newBoard)) score (([],(getTurn newBoard),(getScore newBoard)), seeds)    //Reached the end of the list with more seeds to be distributed
               |_ -> ((List.rev (getBoard newBoard)),(nextTurn newBoard),(getScore newBoard))                   //Reached the end of list (turn over)
        |h::t -> 
                 match (currentHouse < house) with 
                 |true -> sow (currentHouse+1) house t score (((h::(getBoard newBoard)),(getTurn board),(getScore board)),seeds)    //Do nothing to the house
                 |_ -> 
                        match (currentHouse = house) with
                        |true -> 
                                match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                |true -> sow (currentHouse+1) house t score (((0::(getBoard newBoard)),(getTurn board),(getScore board)),h)     //Set head to 0 and seeds to head
                                |_ -> failwith "Cannot sow from empty house"                                                                   
                        |_ -> 
                                match (currentHouse > house) with 
                                |true -> 
                                        match (seeds>0) with 
                                        |true -> sow (currentHouse+1) house t score ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore board)),(seeds-1))   //distribute seed to the head of the list
                                        |_ -> 
                                             sow (currentHouse+1) house t score ((((h)::(getBoard newBoard)),(getTurn board),(getScore board)),(seeds))          //no more seeds to distribute
                                            
                                |_ -> failwith "Well Shit (distributeSeeds)"
    match house>0 with 
    |true -> sow 1 house (getBoard board) (getScore board) (([],(getTurn board),(getScore board)) ,0)
    |_ -> failwith "House does not exist"

let checkValid house position = 
    match position with
    |South -> house<=6
    |_ -> house>6

let useHouse n board = 
    match ((getSeeds n board)>0) with
    |true -> 
                match (checkValid n (getTurn board)) with     // check valid move
                |true -> distributeSeeds n board
                |_ -> board
    |_ -> board
                   
let start position = 
   ([4;4;4;4;4;4;4;4;4;4;4;4],position,(0,0))
    

let score board =  
    getScore board

let gameState board = 
    match (getTurn board) with 
    |South -> "South's turn"
    |North -> "North's turn" //Come back to

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
