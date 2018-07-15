open Tea
open App

type model = 
  { position: Chess.position
  ; board: Board.model
  }


type msg =
  | Board_msg of Board.msg
  | Random_button
  | Random_move of Chess.move
[@@bs.deriving {accessors}]

let update model = function 
  | Board_msg msg -> 
    let board, cmd = Board.update model.board msg in 
    {model with board}, Cmd.map board_msg cmd
  | Random_button -> 
    model, 
    begin match Chess.game_status model.position with
      | Play move_list -> 
        move_list 
        |> List.length
        |> Random.int 0
        |> Random.generate (fun random_number -> List.nth move_list random_number |> random_move)
      | _ -> Cmd.none
    end
  | Random_move move ->
    let position = Chess.make_move model.position move 0 in
    {model with position }, Cmd.none


let view model =
  let open Html in 
  let color = match model.position.turn with | Black -> "Black" | White -> "White" in
  div [] 
    [ Board.view model.position.ar model.board |> map board_msg
    ; p [] [Printf.sprintf "Move %d. It is %s's move." model.position.number color |> text ]
    ; p [] [ map board_msg Board.flip_button_view
           ; button 
               [ onClick Random_button ]
               [ text "Make a random move" ]
           ]
    ]

let init () = 
  { position = Chess.init_position
  ; board = Board.init
  }, Cmd.none

let main = App.standardProgram
    { init
    ; update
    ; view 
    ; subscriptions   = (fun _ -> Sub.none)}
