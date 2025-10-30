let interlaced_mandatory_and_opt
    _ ?never _ ?sometimes _ ?always _
  =
  ignore never;
  ignore always;
  ignore sometimes

let interlaced_labeled_and_opt
    ~lab1:_ ?never ~lab2:_ ?sometimes ~lab3:_ ?always ()
  =
  ignore never;
  ignore always;
  ignore sometimes

let interlaced_mandatory_labeled_and_opt
    ~lab1:_ ?never _ ?sometimes ~lab2:_ ?always ()
  =
  ignore never;
  ignore always;
  ignore sometimes

let () =
  interlaced_mandatory_and_opt () () () ~always:() ();
  interlaced_mandatory_and_opt () () () ~always:() ~sometimes:() ();
  interlaced_labeled_and_opt ~lab1:() ~always:() ~lab2:() ~lab3:() ();
  interlaced_labeled_and_opt ~lab1:() ~always:() ~lab2:() ~lab3:() ~sometimes:() ();
  interlaced_mandatory_labeled_and_opt ~lab1:() () ~always:() ~lab2:() ();
  interlaced_mandatory_labeled_and_opt ~lab1:() () ~always:() ~lab2:() ~sometimes:() ();
  ()
