open Claudius
open OUnit2

let test_bidirectional _ =
  let keycodes =
    [
      (0x4000004F, Key.Right);
      (0x40000050, Key.Left);
      (0x40000051, Key.Down);
      (0x40000052, Key.Up);
      (0x400000E1, Key.Shift_L);
      (0x400000E5, Key.Shift_R);
      (0x400000E0, Key.Control_L);
      (0x400000E4, Key.Control_R);
      (0x400000E2, Key.Alt_L);
      (0x400000E6, Key.Alt_R);
      (0x40000039, Key.CapsLock);
      (0x40000053, Key.NumLock);
      (0x40000047, Key.ScrollLock);
      (0x4000003A, Key.F1);
      (0x4000003B, Key.F2);
      (0x4000003C, Key.F3);
      (0x4000003D, Key.F4);
      (0x4000003E, Key.F5);
      (0x4000003F, Key.F6);
      (0x40000040, Key.F7);
      (0x40000041, Key.F8);
      (0x40000042, Key.F9);
      (0x40000043, Key.F10);
      (0x40000044, Key.F11);
      (0x40000045, Key.F12);
      (0x40000049, Key.Insert);
      (0x4000007F, Key.Delete);
      (0x4000004A, Key.Home);
      (0x4000004D, Key.End);
      (0x4000004B, Key.PageUp);
      (0x4000004E, Key.PageDown);
      (0x00000020, Key.Space);
      (0x0000001B, Key.Escape);
      (0x00000061, Key.A);
      (0x00000062, Key.B);
      (0x00000063, Key.C);
      (0x00000064, Key.D);
      (0x00000065, Key.E);
      (0x00000066, Key.F);
      (0x00000067, Key.G);
      (0x00000068, Key.H);
      (0x00000069, Key.I);
      (0x0000006A, Key.J);
      (0x0000006B, Key.K);
      (0x0000006C, Key.L);
      (0x0000006D, Key.M);
      (0x0000006E, Key.N);
      (0x0000006F, Key.O);
      (0x00000070, Key.P);
      (0x00000071, Key.Q);
      (0x00000072, Key.R);
      (0x00000073, Key.S);
      (0x00000074, Key.T);
      (0x00000075, Key.U);
      (0x00000076, Key.V);
      (0x00000077, Key.W);
      (0x00000078, Key.X);
      (0x00000079, Key.Y);
      (0x0000007A, Key.Z);
      (0x00000030, Key.Num0);
      (0x00000031, Key.Num1);
      (0x00000032, Key.Num2);
      (0x00000033, Key.Num3);
      (0x00000034, Key.Num4);
      (0x00000035, Key.Num5);
      (0x00000036, Key.Num6);
      (0x00000037, Key.Num7);
      (0x00000038, Key.Num8);
      (0x00000039, Key.Num9);
    ]
  in
  List.iter
    (fun (code, key) ->
      assert_equal ~msg:"key mapping" key (Keysdl.of_backend_keycode code);
      assert_equal ~msg:"key mapping" code (Keysdl.to_backend_keycode key))
    keycodes

let test_unknown _ =
  let neg = -1 in
  assert_equal ~msg:"unknown key" Key.Unknown
    (Keysdl.of_backend_keycode 0x00000000);
  assert_equal ~msg:"unknown key" neg (Keysdl.to_backend_keycode Key.Unknown)

let suite =
  "Keysdl Tests"
  >::: [
         "test_bidirectional" >:: test_bidirectional;
         "test_unknown" >:: test_unknown;
       ]

let () = run_test_tt_main suite
