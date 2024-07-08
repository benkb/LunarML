structure CommandLine : sig
              val name : unit -> string
              val arguments : unit -> string list
          end = struct
local _esImport [pure] { argv } from "node:process";
in
fun name () = Perl.encodeUtf8 (Perl.unsafeFromValue (Perl.sub (argv, Perl.fromInt 1)))
fun arguments () = let val n = Perl.unsafeFromValue (Perl.field (argv, "length"))
                       fun go i = if i < n then
                                      let val x = Perl.unsafeFromValue (Perl.sub (argv, Perl.fromInt i)) : WideString.string
                                      in Perl.encodeUtf8 x :: go (i + 1)
                                      end
                                  else
                                      []
                   in go 2
                   end
end
end;
