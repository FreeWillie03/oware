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
    |_ -> failwith "position not available (line 39: getSeeds)"

let print a = sprintf "%A" a

let applyPoint board num =
    match (getScore board) with 
    |s,n -> 
            match (getTurn board) with
            |South -> ((s+num),n)
            |North -> (s,(n+num))

let rec harvest board nuBoard currenthouse endinghouse =
    match (board) with
    | [] -> ((getBoard nuBoard)),(nextTurn nuBoard),(getScore nuBoard) 
    | h::t -> match getTurn nuBoard with
              | North -> match endinghouse >= 7 && endinghouse <=12 with 
                         |true ->  match (currenthouse<endinghouse) with
                                      |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                                      |false -> match (currenthouse <= endinghouse) with
                                                | true -> match h with
                                                           | 3|2 -> let score = applyPoint nuBoard h
                                                                    harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse
                                                           | _ -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                                                | false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
              | South -> match endinghouse >= 1 && endinghouse <=6 with 
                         |true ->  match (currenthouse<endinghouse) with
                                      |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                                      |false -> match (currenthouse <= endinghouse) with
                                                | true -> match h with
                                                           | 3|2 -> let score = applyPoint nuBoard h
                                                                    harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse
                                                           | _ -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                                                | false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse

let distributeSeeds house board =
    let score = getScore board
    let rec sow currentHouse house listi (newBoard,seeds) endingHouse =
        match listi with 
        |[] -> match (seeds>0) with
               |true -> sow 1 0 (List.rev (getBoard newBoard)) (([],(getTurn newBoard),(score)), seeds) endingHouse    //Reached the end of the list with more seeds to be distributed
               |_ -> //((List.rev (getBoard newBoard)),(nextTurn newBoard),(getScore newBoard))                 //Reached the end of list (turn over)
                     let endingHouse = (13 - endingHouse)
                     harvest ((getBoard newBoard)) ([],getTurn newBoard,getScore newBoard) 1 endingHouse
        |h::t -> 
                 match (currentHouse < house) with 
                 |true -> sow (currentHouse+1) house t (((h::(getBoard newBoard)),(getTurn board),(getScore newBoard)),seeds) endingHouse    //Do nothing to the house
                 |_ -> 
                        match (currentHouse = house) with
                        |true -> 
                                match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                |true -> sow (currentHouse+1) house t (((0::(getBoard newBoard)),(getTurn board),(getScore newBoard)),h) endingHouse     //Set head to 0 and seeds to head
                                |_ -> failwith "Cannot sow from empty house"                                                                   
                        |_ -> 
                                match (currentHouse > house) with 
                                |true -> 
                                        match (seeds>0) with
                                        |true -> match seeds=1 with
                                                 |true -> let endingHouse= currentHouse
                                                          sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse
                                                 |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse  //distribute seed to the head of the list //try adding point here
                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse          //no more seeds to distribute
                                            
                                |_ -> failwith "Well Shit (distributeSeeds)"
    match house>0 with 
    |true -> sow 1 house (getBoard board) (([],(getTurn board),(getScore board)) ,0) 0
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
   ([4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4],position,(0,0))

let score board =  
    getScore board

let gameState board =
    match (getScore board) with 
     | s,n -> match s<25 && n<25 with
              |true -> match (getTurn board) with 
                        |South -> "South's turn"
                        |North -> "North's turn" //Come back to
              |false -> match s=n with
                        |true -> "Game ended in a draw"
                        |false -> match s>=25 && n <25 with
                                  | true -> "North won"
                                  | false -> match n>=25 && s<25 with
                                             | true -> "South won"
                                             | false -> failwith "Game outcome unknown"
    

[<EntryPoint>]
let main _ =
    
    let playGame numbers =
        let rec play xs game =
            match xs with
            | [] -> game
            | x::xs -> play xs (useHouse x game)
        play numbers (start South)

    let expr = getBoard (playGame [1])
    let hyo = score (playGame [1])
    printfn "%A %A" expr hyo
    let ay = System.Console.ReadKey()
    
    (*let board = start North |> useHouse 12*)
    
    0 // return an integer exit code
