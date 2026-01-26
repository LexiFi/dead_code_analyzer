module Make (P: Bsa_api.PARAM) = struct
  let unused = P.used
  let used = P.used
  let internally_used = P.used
  let externally_used = P.used
  let sometimes_used = P.used
end
