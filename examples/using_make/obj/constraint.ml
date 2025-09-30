class p = object
  method f = ()
  method g = ()
end

class virtual c = object(this: 'this)
  constraint 'this = #p
  initializer this#f
end

class x = object
  inherit p
  inherit c
end
