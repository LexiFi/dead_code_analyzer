type unused_single_unit = {unused_single_unit : unit}
type 'a unused_single_param = {unused_single_param : 'a}
type 'a unused_mix = {
  unused_unit: unit;
  unused_param : 'a
}

type written_single_unit = {written_single_unit : unit}
type 'a written_single_param = {written_single_param : 'a}
type 'a written_mix = {
  written_unit: unit;
  written_param : 'a
}

type used_single_unit = {used_single_unit : unit}
type 'a used_single_param = {used_single_param : 'a}
type 'a used_mix = {
  used_unit: unit;
  used_param : 'a
}

type internally_used_single_unit = {internally_used_single_unit : unit}
type 'a internally_used_single_param = {internally_used_single_param : 'a}
type 'a internally_used_mix = {
  internally_used_unit: unit;
  internally_used_param : 'a
}

type externally_used_single_unit = {externally_used_single_unit : unit}
type 'a externally_used_single_param = {externally_used_single_param : 'a}
type 'a externally_used_mix = {
  externally_used_unit: unit;
  externally_used_param : 'a
}

type 'a partially_used_mix_1 = {
  partially_used_unit: unit;
  partially_used_param : 'a
}

type 'a partially_used_mix_2 = {
  partially_used_unit: unit;
  partially_used_param : 'a
}

type 'a partially_used_in_match = {
  partially_used_in_match_unit: unit;
  partially_used_in_match_param : 'a
}

