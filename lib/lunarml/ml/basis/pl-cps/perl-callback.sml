structure Perl = struct
open Perl
fun callback (f : value vector -> General.unit) : value
    = function (fn args =>
                   ( LunarML.DelimCont.pushPrompt (LunarML.DelimCont.topLevel, fn () => f args)
                   ; Perl.undefined
                   )
               )
end;
