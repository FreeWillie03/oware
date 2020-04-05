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

let rec checkzero turn list = //this function returns true if capturing would remove all the pieces on the opponenets board
    match list with
    | [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] -> false //match case to catch special case where a draw would prevent capture of final seeds
    | _ -> match (turn) with 
            |South -> match list with
                      | [0; 0; 0; 0; 0; 0; _; _; _; _; _; _] -> true
                      | _ -> false
            |North -> match list with 
                      | [_; _; _; _; _; _; 0; 0; 0; 0; 0; 0] -> true
                      | _ -> false

let rec checkzero2 turn list = //this function returns true if capturing would remove all the pieces on the opponenets board
    match list with
    | [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] -> false //match case to catch special case where a draw would prevent capture of final seeds
    | _ -> match (turn) with 
            |South -> match list with
                      | [_; _; _; _; _; _; 0; 0; 0; 0; 0; 0] -> true
                      | _ -> false
            |North -> match list with 
                      | [0; 0; 0; 0; 0; 0; _; _; _; _; _; _] -> true
                      | _ -> false
   
let rec harvest board nuBoard currenthouse endinghouse switch = //this function does the seed capturing 
    match (board) with
    | [] -> ((getBoard nuBoard)),(nextTurn nuBoard),(getScore nuBoard) 
    | h::t -> match getTurn nuBoard with
              | North -> match endinghouse >= 7 && endinghouse <=12 with //make sure seeds are captured from the right side
                         |true ->  match (currenthouse<endinghouse) with
                                      |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                      |false -> match (currenthouse = endinghouse) with // the endinghouse value is determined by the distribution function allow the function to know where to start capturing seeds
                                                | true -> match h with
                                                           | 3|2 -> let score = applyPoint nuBoard h
                                                                    let switch = true // switch prevents seeds that are not contigous from being captured
                                                                    harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                           | _ ->  let switch = false
                                                                   harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                | false -> match (currenthouse>endinghouse) with
                                                           | true -> match switch with 
                                                                      |true -> match h with
                                                                                | 3|2 -> let score = applyPoint nuBoard h
                                                                                         let switch = true
                                                                                         harvest t ((0::(getBoard nuBoard)),getTurn nuBoard,score) (currenthouse+1) endinghouse switch
                                                                                | _ ->let switch = false
                                                                                      harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                                      |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                                           | false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
              | South -> match endinghouse >= 1 && endinghouse <=6 with 
                         |true ->  match (currenthouse<endinghouse) with
                                      |true ->  harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                                      |false -> match (currenthouse = endinghouse) with
                                                | true -> match h with
                                                           | 3|2 -> let score = applyPoint nuBoard h
                                                                    match endinghouse + 1 <> 7 with
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
                                                           | false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch
                         |false -> harvest t ((h::(getBoard nuBoard)),getTurn nuBoard,getScore nuBoard) (currenthouse+1) endinghouse switch

let distributeSeeds house board =
    let predistribute = board
    let score = getScore board
    let origin = false
    let originhouse = house
    let rec sow currentHouse house listi (newBoard,seeds) endingHouse origin =
        match listi with 
        |[] -> match (seeds>0) with
               |true -> let origin = true
                        sow 1 0 (List.rev (getBoard newBoard)) (([],(getTurn newBoard),(score)), seeds) endingHouse (origin)  //Reached the end of the list with more seeds to be distributed
               |_ -> let endingHouse = (13 - endingHouse)
                     match checkzero2 (getTurn newBoard) (List.rev (getBoard newBoard)) with
                     | true -> ((getBoard board),getTurn board,getScore newBoard)
                     | false -> 
                                let harvestedBoard = harvest ((getBoard newBoard)) ([],getTurn newBoard,getScore newBoard) 1 endingHouse false
                                match checkzero (getTurn harvestedBoard) (getBoard harvestedBoard) with
                                | true -> (List.rev (getBoard newBoard),nextTurn newBoard,getScore newBoard)
                                | false -> harvestedBoard          
        |h::t -> 
                 match (currentHouse < house) with 
                 |true -> sow (currentHouse+1) house t (((h::(getBoard newBoard)),(getTurn board),(getScore newBoard)),seeds) endingHouse (origin)   //Do nothing to the house
                 |_ -> match origin with
                       | true -> match originhouse = currentHouse with
                                 |true ->let origin = false
                                         sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin)
                                 |false -> match (currentHouse = house) with
                                        |true ->match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newBoard)),(getTurn board),(getScore newBoard)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                        |_ -> 
                                                match (currentHouse > house) with 
                                                |true -> 
                                                        match (seeds>0) with
                                                        |true -> match seeds=1 with
                                                                    |true -> let endingHouse= currentHouse
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list //try adding point here
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin)        //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
                       | false -> match (currentHouse = house) with
                                        |true ->match (h>0) with                                                                                                //Check that the house isn't empty (again)
                                                            |true -> sow (currentHouse+1) house t (((0::(getBoard newBoard)),(getTurn board),(getScore newBoard)),h) endingHouse (origin)   //Set head to 0 and seeds to head
                                                            |_ -> failwith "Cannot sow from empty house"                                                                       
                                        |_ -> 
                                                match (currentHouse > house) with 
                                                |true -> 
                                                        match (seeds>0) with
                                                        |true -> match seeds=1 with
                                                                    |true -> let endingHouse= currentHouse
                                                                             sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)
                                                                    |false -> sow (currentHouse+1) house t ((((h+1)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds-1)) endingHouse (origin)  //distribute seed to the head of the list //try adding point here
                                                        |_ -> sow (currentHouse+1) house t ((((h)::(getBoard newBoard)),(getTurn board),(getScore newBoard)),(seeds)) endingHouse (origin)        //no more seeds to distribute
                                                |_ -> failwith "Well Shit (distributeSeeds)"
    match house>0 with 
    |true -> sow 1 house (getBoard board) (([],(getTurn board),(getScore board)) ,0) 0 false
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
    
    0 // return an integer exit code
