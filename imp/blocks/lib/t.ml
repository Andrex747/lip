(*[Cmd
  (Decl ([IntVar "y"; IntVar "x"],
    Seq
     (Seq (Assign ("x", Const 10),
       Decl ([IntVar "x"], Seq (Assign ("x", Const 20), Assign ("y", Var "x")))),
     Assign ("y", Var "x"))),

  {envstack = [<fun>]; memory = <fun>; firstloc = 0});

 Cmd
  (Seq
    (Decl ([IntVar "x"], Seq (Assign ("x", Const 20), Assign ("y", Var "x"))),
    Assign ("y", Var "x")),
  
  {envstack = [<fun>; <fun>]; memory = <fun>; firstloc = 2});




 Cmd (Seq (Assign ("y", Var "x"), Assign ("y", Var "x")),
  {envstack = [<fun>; <fun>; <fun>]; memory = <fun>; firstloc = 3});


 Cmd (Assign ("y", Var "x"),
  {envstack = [<fun>; <fun>; <fun>]; memory = <fun>; firstloc = 3});
 St {envstack = [<fun>; <fun>; <fun>]; memory = <fun>; firstloc = 3}]*)