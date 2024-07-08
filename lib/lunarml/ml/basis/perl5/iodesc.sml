structure IODesc :> sig
              eqtype iodesc
              val hash : iodesc -> word
              val compare : iodesc * iodesc -> order
              val toDesc : Perl.value -> iodesc
              val fromDesc : iodesc -> Perl.value
              val release : iodesc -> unit
          end = struct
type iodesc = int
val hash = Word.fromInt
val compare = Int.compare
val objToDescMap = LunarML.assumeDiscardable (fn () => Perl.new Perl.Lib.WeakMap #[]) () (* key: object, value: int *)
val descToObjMap = LunarML.assumeDiscardable (fn () => Perl.new Perl.Lib.Map #[]) () (* key: int, value: object *)
val freeList : (int * int list) ref = ref (0, [])
fun newDesc () : int = case !freeList of
                           (n, []) => ( freeList := (n + 1, [])
                                      ; n
                                      )
                         | (n, i :: is) => ( freeList := (n, is)
                                           ; i
                                           )
fun toDesc obj = let val v = Perl.method (objToDescMap, "get") #[obj]
                 in if Perl.=== (v, Perl.undefined) then
                        let val d = newDesc ()
                        in Perl.method (objToDescMap, "set") #[obj, Perl.fromInt d]
                         ; Perl.method (descToObjMap, "set") #[Perl.fromInt d, obj]
                         ; d
                        end
                    else
                        Perl.unsafeFromValue v : int
                 end
fun fromDesc i = Perl.method (descToObjMap, "get") #[Perl.fromInt i]
fun release i = if Perl.unsafeFromValue (Perl.method (descToObjMap, "has") #[Perl.fromInt i]) then
                    let val (n, is) = !freeList
                        val () = freeList := (n, i :: is)
                        val obj = Perl.method (descToObjMap, "get") #[Perl.fromInt i]
                    in Perl.method (descToObjMap, "delete") #[Perl.fromInt i]
                     ; Perl.method (objToDescMap, "delete") #[obj]
                     ; ()
                    end
                else
                    ()
end;
