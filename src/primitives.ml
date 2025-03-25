type point = {
    x : int ;
    y : int ;
}

type t =
| Circle of point * float * int
| FilledCircle of point * float * int
| Line of point * point * int
| Pixel of point * int
| Polygon of point list * int
| FilledPolygon of point list * int
| Rect of point * point * int
| FilledRect of point * point * int
| Triangle of point * point * point * int
| FilledTriangle of point * point * point * int
| Char of point * Fontv2.t * char * int
| String of point * Fontv2.t * string * int

