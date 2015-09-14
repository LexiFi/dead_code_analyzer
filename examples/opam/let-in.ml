let complicated ?a ?b ?c d = d

let parent =
  let fst_son = complicated ~a:2 0 in
  let snd_son =
    let fst_grand_son = complicated ~a:0 ~b:1 12 in
    let snd_grand_son = 
      let great_greand_son = complicated ~a:13 0 in
      great_greand_son in
    snd_grand_son + fst_grand_son in
  fst_son + snd_son
