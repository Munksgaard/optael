fun fjernNyLinje s = String.substring (s, 0, (size s - 1))

(* hentLinjer : string -> string list *)
local
    (* hentLinjer' : instream -> string list *)
    fun hentLinjer' ind = 
        let val linje = TextIO.inputLine ind
        in 
            case linje of
                SOME "\n" => hentLinjer' ind
              | SOME s => fjernNyLinje s :: hentLinjer' ind
              | NONE => []
        end;
in
    fun hentLinjer fil_ind = 
        let val fil_stream = TextIO.openIn fil_ind
        in hentLinjer' fil_stream
        end;
end;

(* hentDrikkevarer : unit -> string list *)
fun hentDrikkevarer () = hentLinjer "data/statisk/drikkevarer";

fun hentKategori s = hd (String.tokens (fn x => x = #":") s)

(* hentStregkategorier : unit -> string list *)
fun hentStregkategorier () = 
    let 
        val linjer = hentLinjer "data/statisk/stregkategorier"
    in map hentKategori linjer end;

(* hentRusser : unit -> string list *)
fun hentRusser () = hentLinjer "data/statisk/russer";

(* hentVejledere : unit -> string list *)
fun hentVejledere () = hentLinjer "data/statisk/vejledere";

fun taelDrik (p, drik) =
    (print ("Hvor mange " ^ drik ^ " har " ^ p ^ " drukket?\n");
     let 
         val s = TextIO.inputLine TextIO.stdIn
     in
         if s = SOME "\n" then
             (drik, 0)
         else
             case Int.fromString (valOf s) of
                 SOME n => (drik, n)
               | NONE => (print "Ugyldigt input, proev igen.\n"; 
                          taelDrik (p, drik))
     end);
         
(* taelPerson : string * string list -> (string * int) list *)
fun taelPerson (_, []) = []
  | taelPerson (p, drik :: drikke) = taelDrik (p, drik) :: 
                                     taelPerson(p, drikke)

(* taelopListe : string list * string list -> (string * (string * int) list) list *)
fun taelOpListe ([], _) = []
  | taelOpListe (p :: ps, drikke) = (p, taelPerson (p, drikke)) :: 
                                    taelOpListe (ps, drikke);

fun optaellingTilStreng [] = ""
  | optaellingTilStreng ((p, opt) :: opts) =
    let 
        fun aux [] = ""
          | aux ((drik, n) :: drikke) =
            drik ^ ": " ^ Int.toString n ^ "\n" ^ aux drikke
    in
        "[" ^ p ^ "]\n" ^ aux opt ^ "\n" ^ optaellingTilStreng opts
    end;

fun optaelVejledere () =
    let
        val vejledere = hentVejledere ()
        val drikke = hentStregkategorier ()
    in
        print "Optaeller nu vejledere\n";
        taelOpListe (vejledere, drikke)
    end;

fun optaelRusser () =
     let
        val russer = hentRusser ()
        val drikke = hentStregkategorier ()
    in
        print "Optaeller nu russer\n";
        taelOpListe (russer, drikke)
    end;

fun optaelStreglister titel =
     let 
         val vejlOpt = optaelVejledere ()
         val rusOpt = optaelRusser ()
         val _ = FileSys.mkDir ("data/streglister/" ^ titel)
         val vejlStream = TextIO.openOut ("data/streglister/" ^ titel ^ "/vejlederstreger.txt")
         val rusStream = TextIO.openOut ("data/streglister/" ^ titel ^ "/russtreger.txt")
     in
         TextIO.output (vejlStream, optaellingTilStreng vejlOpt);
         TextIO.flushOut vejlStream;
         TextIO.output (rusStream, optaellingTilStreng rusOpt);
         TextIO.flushOut rusStream;
         (vejlOpt, rusOpt)
     end;

fun taelOpDrikke [] = []
  | taelOpDrikke (d :: ds) =
    (print ("Hvor mange " ^ d ^ " er der tilbage?\n");
     let
         val s = TextIO.inputLine TextIO.stdIn
     in
         if s = SOME "\n" then
             (d, 0) :: taelOpDrikke ds
         else
             case Int.fromString (valOf s) of
                 SOME n => (d, n) :: taelOpDrikke ds
               | NONE => (print "Ugyldigt input, proev igen.\n"; 
                          taelOpDrikke ds)
     end);

fun drikkeOptaellingTilStreng [] = ""
  | drikkeOptaellingTilStreng ((d,n) :: ds) =
    d ^ ": " ^ Int.toString n ^ "\n" ^ drikkeOptaellingTilStreng ds

fun optaelDrikkevarer titel =
    let
        val drikkevarer = hentDrikkevarer ()
        val drikOpt = taelOpDrikke drikkevarer
        val s = drikkeOptaellingTilStreng drikOpt
        val _ = FileSys.mkDir ("data/optaellinger/" ^ titel)
        val drikStream = TextIO.openOut ("data/optaellinger/" ^ titel ^ "/drikkevarer.txt")
    in
        TextIO.output (drikStream, s);
        TextIO.flushOut drikStream;
        drikOpt
    end;
