open Graphics;;

(*types*)
type tile =
        |Dead
        |Alive
;;

(*global values*)
let x_size = ref 100 ;; (*x size*)
let y_size = ref 55 ;; (*y size*)
let side = ref 20 ;;
let running = ref true ;;
let moved_tile = ref true ;;
let x = ref 0 ;;
let y = ref 0 ;;
let col1 = ref 0x49494d ;; (*background*)
let col2 = ref 0xa6a7aa ;; (*tiles*)
let col3 = ref 0x717175 ;; (*grid*)
let hide = ref false ;; (* hide grid*)




(*functions*)
let color_of_tile t = (*tile -> color*)
        match t with
        |Dead -> !col1
        |Alive -> !col2
;;

let customize_window () = (*unit -> unit*)
        open_graph " 1400x900+300-100";
        set_window_title "Game of Life";
        set_color !col1;
        fill_rect 0 0 (size_x ()) (size_y ())
;;

let make_map () = (*unit -> tile array array*)
        Array.make_matrix !x_size !y_size Dead
;;

let print_map m = (*tile array array -> unit*)
        set_color !col1;
        fill_rect 0 0 (size_x ()) (size_y ());

        for i = 0 to (!x_size-1) do
                for j = 0 to (!y_size-1) do
                        set_color (color_of_tile m.(i).(j));
                        fill_rect (!side*i) (!side*j) (!side-2) (!side-2);
                done
        done;
        for k = 1 to (!y_size-1) do
                set_color !col3;
                fill_rect (!side-1) (k * !side - 1) ((!x_size-2) * !side) 0;
        done;
        for l = 1 to (!x_size-1) do
                set_color !col3;
                fill_rect (l * !side - 1) (!side-1) 0 ((!y_size-2) * !side);
        done
;;

let adjust_side m = (*tile array array -> unit*)
        let side_x = (size_x ()) / !x_size in
        let side_y = (size_y ()) / !y_size in
        let side_temp = !side in
        if side_x < side_y then side := side_x
        else side := side_y;
        if !side <> side_temp then print_map m
;;

let copy_matrix m1 m2 = (*tile array array -> tile array array -> unit*)
        for i = 0 to (!x_size-1) do
                for j = 0 to (!y_size-1) do
                        m2.(i).(j) <- m1.(i).(j)
                done
        done
;;

let alive_neighbours m x y = (*tile array array -> int -> int -> int*)
        let count = ref 0 in
        for i = 0 to 2 do
                for j = 0 to 2 do
                        if m.(x-1+i).(y-1+j) = Alive then count := !count+1
                done
        done;
        !count
;;

let update_map m = (*tile array array -> unit*)
        let m_temp = Array.make_matrix !x_size !y_size Dead in
        copy_matrix m m_temp;
        for i = 1 to (!x_size-2) do
                for j = 1 to (!y_size-2) do
                        if m.(i).(j) = Dead then
                               match alive_neighbours m i j with
                                |3 -> m_temp.(i).(j) <- Alive
                                |_ -> m_temp.(i).(j) <- Dead
                        else
                                match (alive_neighbours m i j)-1 with
                                |2 | 3 -> m_temp.(i).(j) <- Alive
                                |_ -> m_temp.(i).(j) <- Dead
                done
        done;
        copy_matrix m_temp m
;;

let reset_map m = (*tile array array -> unit*)
        for i = 0 to (!x_size-1) do
                for j = 0 to (!y_size-1) do
                        m.(i).(j) <- Dead
                done
        done;
        print_map m
;;

let process_key m k = (*tile array array -> char -> unit*)
        match k with
        |'\027' -> running := false
        |'r' -> reset_map m
        |'h' -> if !hide then (hide := false; col3 := 0x717175) else (hide := true; col3 := !col1); print_map m
        |_ -> update_map m; print_map m
;;

let process_click m = (*tile array array -> unit*)
        let mouse_x, mouse_y = mouse_pos () in
        x := mouse_x / !side;
        y := mouse_y / !side;
        moved_tile := false;
        if (1 <= !x && !x < !x_size-1) && (1 <= !y && !y < !y_size-1) then
                if m.(!x).(!y) = Alive then (
                        m.(!x).(!y) <- Dead;
                        set_color !col1;
                        fill_rect (!x * !side) (!y * !side) (!side-2) (!side-2) )
                else (
                        m.(!x).(!y) <- Alive;
                        set_color !col2;
                        fill_rect (!x * !side) (!y * !side) (!side-2) (!side-2) )
;;

let game m = (*game function*)
        adjust_side m;

        let cs = wait_next_event [Button_down; Key_pressed; Mouse_motion] in
        if cs.keypressed then
                process_key m cs.key;

        if cs.button && !moved_tile then
                process_click m;
        let mouse_x, mouse_y = mouse_pos () in
        if !x <> mouse_x / !side then moved_tile := true;
        if !y <> mouse_y / !side then moved_tile := true;
;;

let _ = (*main*)
        customize_window ();
        let map = make_map () in
        adjust_side map;
        print_map map;
        while !running do
                game map
        done
;;
