#r "packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq"

open System
open System.IO
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

let linesLimit = 10000 // for quick testing
let recommendSongsLimit = 25

let splitLine (line : string) =
    let strings = line.Split()
    let countLog = Math.Log(float strings.[2], float 2) |> Math.Round |> int |> (+) 1
    strings.[0], strings.[1], countLog

let addData (users : Map<_, Map<string, int>>, songs : Set<_>) (user, song, count) =
    let songsCount =
        match users.TryFind user with
        | Some songsCount -> songsCount.Add(song, count)
        | None -> Map.ofList [song, count]
    users.Add(user, songsCount), songs.Add song

// average listening count for a user
let calcAverages = Map.toSeq >> Seq.map snd >> Seq.averageBy float

let readData filename =
    let users, songs =
        File.ReadLines filename
        |> Seq.truncate linesLimit
        |> PSeq.map splitLine
        |> PSeq.fold addData (Map.empty, Set.empty)
    users |> Map.map (fun _ counts -> (counts, calcAverages counts)), songs

// users: Map<string (* user *), Map<string (* song *), int (* count *) * float (* avgCount *)>>
// songs: Set<string>
let users, songs = readData "train_triplets.txt"
printfn "users: %A songs: %A" users.Count songs.Count

let similarities =
    let similarity (counts1, avg1) (counts2, avg2) =
        let songs = Map.toSeq >> PSeq.map fst >> Set.ofSeq
        let songCount (counts : Map<_,_>) song = counts.Item song

        let square x = x * x
        let norm counts avg song = square (float (songCount counts1 song) - avg)

        let commonSongs = Set.intersect (songs counts1) (songs counts2)
        let songNorm counts avg = Set.fold (fun acc song -> acc + (norm counts avg song)) 0. commonSongs
        let songScalar x = (float (songCount counts1 x) - avg1) * (float (songCount counts2 x) - avg2)

        let scalar = Set.fold (fun acc x -> acc + songScalar x) 0. commonSongs
        let user1Norm = songNorm counts1 avg1
        let user2Norm = songNorm counts2 avg2

        if user1Norm <> 0. && user2Norm <> 0. then
            (scalar / Math.Sqrt user1Norm * user2Norm) |> (+) 1. |> (*) 50.
        else
            0.

    let rec pairs = function
        | head :: tail as l -> List.map (fun x -> head, x) l @ pairs tail
        | _ -> []

    let calcSimilarity ((u1, counts1), (u2, counts2)) = (u1, u2), similarity counts1 counts2
    Map.toList >> pairs >> List.map calcSimilarity >> Map.ofList

let userSimilarities = similarities users

let rec similarity user1 user2 =
    match userSimilarities.TryFind (user1, user2) with
    | Some similarity -> similarity
    | None -> userSimilarities.Item (user2, user1)

let userSongCount user song =
    let counts, _ = users.Item user
    match counts.TryFind song with
    | Some count -> count
    | None -> 0

let prediction user song =
    let prediction (prediction, similaritySum) (user, similarity) =
        prediction + similarity * (float (userSongCount user song)), similaritySum + similarity

    let similarUsers = 
        users
        |> Map.toSeq
        |> PSeq.map (fun (otherUser, _) -> otherUser, similarity user otherUser / 100.)
        |> Seq.sortByDescending snd
        |> Seq.truncate 25
    
    let songPrediction, similaritySum = Seq.fold prediction (0.,0.) similarUsers
    if similaritySum <> 0. then songPrediction / similaritySum else 0.

let recommendations (user: string, songsCounts : Map<string, int>) =
    songs
    |> PSeq.filter (not << songsCounts.ContainsKey)
    |> PSeq.map (fun song -> song, prediction user song)
    |> Seq.sortByDescending snd
    |> Seq.truncate recommendSongsLimit
    |> PSeq.toList

let user = "b80344d063b5ccb3212f76538f3d9e43d87dca9e"
let userCounts = users.Item user |> fst

recommendations (user, userCounts)
|> List.iter (printfn "%A")