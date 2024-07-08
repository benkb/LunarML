structure Perl = struct
open Perl
fun callback (f : value vector -> General.unit) : value
    = function (fn args =>
                   ( f args
                   ; Perl.undefined
                   )
               )
end;
