open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.RegularExpressions

type Seat =
    {
        Code: int * char
        Reservee: string option
    }

    override this.ToString () =
        let row, col = this.Code
        sprintf "%02d%c" row col

    static member Index seat = 
        let row, col = seat.Code
        (row - 1) * 4 + int col - 65

    static member IsAvailable seat =
        (seat.Reservee = None)

    static member IsReserved seat = 
        not (Seat.IsAvailable seat)

    static member Unreserve seat = 
        { seat with Reservee = None }

    static member Reserve seat name = 
        { seat with Reservee = Some name }

    static member DisplayInListFormat seat = 
        printfn $"{seat.ToString()}\t{seat.Reservee.Value}"

    static member DisplayCodeIfAvailable seat = 
        printf "%s" (if seat.Reservee = None then $"{seat.ToString()} " else " X  ")

type Seats = 
    {
        Seats: Seat list
    }

    static member Reserve seat seats reservee =
        let index = Seat.Index seat
        { Seats = seats |> List.updateAt index (Seat.Reserve seat reservee) }
    
    static member Unreserve seat seats =
        let index = Seat.Index seat
        seats |> List.updateAt index (Seat.Unreserve seat)
    
    static member TransferSeat source destination seats =
        let indexSource = Seat.Index source
        let indexDestination = Seat.Index destination

        {
            Seats = 
                seats
                |> List.updateAt indexDestination { destination with Reservee = source.Reservee }  
                |> List.updateAt indexSource { source with Reservee = None }
        }

    static member CancelReservation seat seats =
        {
            Seats = seats |> Seats.Unreserve seat
        }

    static member GetReservedSeats seats=
        {
            Seats = 
                seats
                |> List.filter Seat.IsReserved
        }

    static member DisplaySeats seats =
        printfn "\nSeats:"
        seats.Seats
        |> List.iter
            ( fun seat -> 
                if (Seat.Index seat) % 4 = 0 then printfn ""
                Seat.DisplayCodeIfAvailable seat
            )
        printfn "\n"

    static member DisplayReservations seats =
        printfn "\nReservations:\n"
        printfn "Seat\tReservee"
        seats.Seats
            |> List.iter 
                (
                    fun seat -> 
                        Seat.DisplayInListFormat seat
                )
        printfn ""

    static member EmptySeats =
        {
            Seats = 
                [
                    for i in 0..19 do
                        for j in 0..3 ->
                            {
                                Code = i + 1, char (j + 65)
                                Reservee = None
                            }
                ]
        }

type Serializer = 

    static member Options =
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        options

    static member Serialize (seats: Seats) = 
        JsonSerializer.Serialize<Seats> (seats, Serializer.Options)

    static member Deserialize (json: string) =
        JsonSerializer.Deserialize<Seats> (json, Serializer.Options)

type File = 
    { Path: string }
        
type Menu = 
    { 
        Menu: Map<int, string> 
    }

    static member Output (menu: Menu) =
        menu.Menu
        |> Map.iter 
            (
                fun item out -> 
                    printfn $"{item}. {out}"
            )

type Data =
    {
        File: File
        Seats: Seats
        Menu: Menu
        Fun: unit
    }

    static member GetData path =
        File.ReadAllText path
        |> Serializer.Deserialize

    static member UpdateData path data =
        let json = Serializer.Serialize data
        File.WriteAllText (path, json)
        data


let file = { Path = "./Data.json" }

let menu = 
    {
        Menu = 
            Map 
                [
                    (0, "Exit")
                    (1, "Create reservation/s")
                    (2, "Transfer seat/s")
                    (3, "Cancel a reservation")
                ]
    }

let data = 
    {
        File = file
        Seats = Data.GetData file.Path
        Menu = menu
        Fun = ()
    }

let getInput out =
    printf "%s >> " out
    Console.ReadLine()

let (|SeatCode|_|) input =
    let m = Regex.Match (input, @"(?<!\S)(?<row>(?:0[1-9])|(?:[1-9])|(?:1[0-9])|(?:20))(?<col>(?:[A-D]))(?!\S)")
    if m.Success 
    then 
        Some 
            {
                Code = int m.Groups["row"].Value, char m.Groups["col"].Value
                Reservee = None
            }
    else None

let handleReservation (data: Data) (seat: Seat) =

    let seats = data.Seats.Seats
    let index = Seat.Index seat
    if Seat.IsAvailable (seats.Item index)
    then 
        
        let reservee = getInput "Input reservee's name"
        {
            data with
                Seats = Data.UpdateData data.File.Path (Seats.Reserve seat seats reservee)
                Fun = (
                        printfn "\nLog: Seat reservation successful.\n\n"
                    )
        }
    else
        {
            data with Fun = printfn "\nLog: Seat is already reserved to somebody else.\n\n"
        }

let rec createReservation (data: Data) = 

    Seats.DisplaySeats data.Seats

    match (getInput "Input seat code (Ex. 01A) or 0 to Exit").ToUpper() with
    | "0" -> { data with Fun = printf "\nLog: Exiting reservation menu..\n\n" }
    | SeatCode seat -> createReservation (handleReservation data seat)
    | _ -> createReservation { data with Fun = printf "\nLog: Invalid Input..\n\n" }

let rec handleTransfer (data: Data) (source: Seat) = 

    let seats = data.Seats.Seats
    let indexSource = Seat.Index source
    let source = seats.Item indexSource

    if Seat.IsReserved (source)
    then
        Seats.DisplaySeats data.Seats
        match (getInput "Input seat code to transfer to").ToUpper() with
        |  SeatCode destination -> 
            let indexDestination = Seat.Index destination
            let destination = seats.Item indexDestination

            if Seat.IsAvailable (destination)
            then 
                {
                    data with 
                        Seats = Data.UpdateData data.File.Path (Seats.TransferSeat source destination seats)
                        Fun = printf "\nLog: Seat transfer successful..\n\n"
                }
            else
                handleTransfer 
                    { 
                                data with 
                                    Fun = printfn "\nLog: Seat is already reserved to somebody else.\n\n" 
                        } 
                        source
        | _ -> 
            handleTransfer 
                    { 
                                data with 
                                    Fun = printf "\nLog: Invalid Input...\n\n" 
                        }
                        source
    else
        {
            data with Fun = printf "\nSeat has no reservation..\n\n"
        }


let rec transferSeats (data: Data) =

    Seats.DisplayReservations (Seats.GetReservedSeats data.Seats.Seats)

    match (getInput "Input seat code (Ex. 01A) to transfer or 0 to Exit").ToUpper() with
    | "0" -> { data with Fun = printf "\nLog: Exiting transfering seats menu..\n\n" }
    | SeatCode seat -> transferSeats (handleTransfer data seat)
    | _ -> transferSeats { data with Fun = printf "\nLog: Invalid Input..\n\n" }

let rec handleCancellation (data: Data) (seatToCancel: Seat) =

    let index = Seat.Index seatToCancel
    let seats = data.Seats.Seats

    if Seat.IsReserved (seats.Item index)
    then
        match getInput "Input \"YES\" to confirm cancellation, or 0 to Exit" with
        | "0" -> { data with Fun = printf "\nLog: Cancellation was cancelled.\n\n" }
        | "YES" -> 
            { 
                data with 
                    Seats = Data.UpdateData data.File.Path (Seats.CancelReservation seatToCancel seats)
                    Fun = printf "\nLog: Reservation was successfully cancelled.\n\n"
            }
        | _ -> handleCancellation { data with Fun = printf "\nInvalid input..\n\n" } seatToCancel
    else
        {
            data with Fun = printf "\nLog: Seat is not reserved.\n\n"
        }

let rec cancelReservation (data: Data) =

    Seats.DisplayReservations (Seats.GetReservedSeats data.Seats.Seats)

    match (getInput "Input seatcode (Ex. 01A) to cancel or 0 to Exit").ToUpper() with
    | "0" -> { data with Fun = printf "\nLog: Exiting cancel reservation menu\n\n" }
    | SeatCode seat -> cancelReservation (handleCancellation data seat)
    | _ -> transferSeats { data with Fun = printf "\nLog: Invalid Input..\n\n" }

let rec reservationSystem (data: Data) =

    Seats.DisplaySeats data.Seats

    data.Menu |> Menu.Output

    match getInput "Choose action" with
    | "0" -> { data with Fun = printfn "\nLog: Exiting Program..\n\n" }
    | "1" -> reservationSystem (createReservation data)
    | "2" -> reservationSystem (transferSeats data)
    | "3" -> reservationSystem (cancelReservation data)
    | _ -> reservationSystem { data with Fun = printfn "\nLog: Invalid Input..\n\n"}

(reservationSystem data).Fun