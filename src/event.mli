(** High-level representation of input events. *)
type t =
  | KeyDown of Key.t
    (** A keyboard key was pressed down.   
        [Key.t] identifies which key. *)
  | KeyUp of Key.t
    (** A keyboard key was released.      
        [Key.t] identifies which key. *)
  | MouseButtonDown of Mouse.button * (int * int)
    (** A mouse button was pressed.       
        [(button, (x,y))] where [button] is the mouse button,    
        and [(x,y)] are the coordinates at the time of press. *)
  | MouseButtonUp of Mouse.button * (int * int)
    (** A mouse button was released.      
        [(button, (x,y))] where [button] is the mouse button,    
        and [(x,y)] are the coordinates at the time of release. *)
  | MouseMotion of (int * int)
    (** The mouse pointer moved.          
        [(x,y)] are the new coordinates of the cursor. *)
  | MouseWheel of int
    (** The mouse wheel was scrolled.     
        [int] is the scroll amount (positive for up, negative for down). *)
  | MouseDrag of Mouse.button * (int * int)
    (** A drag event with a mouse button held.  
        [(button, (x,y))] where [button] is the button being dragged,  
        and [(x,y)] are the current cursor coordinates. *)