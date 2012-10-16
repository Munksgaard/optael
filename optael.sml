val DATADIR = "data/";
val STREGDIR = DATADIR ^ "streglister/"
(* val _optaellingDir_ = _dataDir_ ^ "optaellinger/" *)
(* val _indkoebDir_ = _dataDir ^ "indkoeb/" *)
(* val _statiskDir_ = _dataDir ^ "statisk/" *)
(* val _stregKategorier_ = _statiskDir_ ^ "stregkategorier" *)
(* val _drikkeVarer_ = _statiskDir_ ^ "drikkevarer" *)
(* val _russer_ = _statiskDir_ ^ "russer" *)
(* val _vejledere_ = _statiskDir_ ^ "vejledere" *)

fun fjernNyLinje s = String.substring (s, 0, (size s - 1))

local
    fun partitionerStreng' (_, [], _) = NONE
      | partitionerStreng' (c, (x :: xs), acc) =
        if c = x then
            SOME (implode (rev acc), implode xs)
        else
            partitionerStreng' (c, xs, x :: acc)
in
    fun partitionerStreng c streng =
        partitionerStreng' (c, explode streng, [])
end;

fun tolkLinje f s =
    case partitionerStreng #":" s of
        SOME (a,b) => (case f b of
                           SOME n => (a, n)
                         | NONE => raise Fail "Du er rigtig dum")
      | NONE => raise Fail "Du er dum"

val tolkLinjeInt = tolkLinje Int.fromString;

(* hentLinjer : string -> string list *)
local
    (* hentLinjer' : instroem -> string list *)
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
        let val fil_stroem = TextIO.openIn fil_ind
        in hentLinjer' fil_stroem before TextIO.closeIn fil_stroem
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
         val vejlStroem = TextIO.openOut ("data/streglister/" ^ titel ^ "/vejlederstreger.txt")
         val rusStroem = TextIO.openOut ("data/streglister/" ^ titel ^ "/russtreger.txt")
     in
         TextIO.output (vejlStroem, optaellingTilStreng vejlOpt);
         TextIO.flushOut vejlStroem;
         TextIO.output (rusStroem, optaellingTilStreng rusOpt);
         TextIO.flushOut rusStroem;
         (vejlOpt, rusOpt) before
         (TextIO.closeOut vejlStroem; TextIO.closeOut rusStroem)
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
        val drikStroem = TextIO.openOut ("data/optaellinger/" ^ titel ^ "/drikkevarer.txt")
    in
        TextIO.output (drikStroem, s);
        TextIO.closeOut drikStroem;
        drikOpt
    end;

fun hentNavnFraStroem stroem =
    case TextIO.input1 stroem of
        SOME #"[" => hentNavnFraStroem stroem
      | SOME #"]" => hentNavnFraStroem stroem
      | SOME #"\n" => ""
      | SOME c => str c ^ hentNavnFraStroem stroem
      | NONE => "";

fun hentStregerFraStroem' stroem =
    case TextIO.inputLine stroem of
        SOME "\n" => []
      | SOME s =>
        tolkLinjeInt s :: hentStregerFraStroem' stroem
      | NONE => []

fun hentStregerFraStroem stroem =
    if TextIO.endOfStream stroem then
        []
    else (hentNavnFraStroem stroem, hentStregerFraStroem' stroem) ::
         hentStregerFraStroem stroem;

fun hentRusStregerFraDag s =
    let
        val rus_fil = "data/streglister/" ^ s ^ "/russtreger.txt"
        val rus_stroem = TextIO.openIn rus_fil
    in
        hentStregerFraStroem rus_stroem
        before TextIO.closeIn rus_stroem
    end;

fun hentVejlStregerFraDag s =
    let
        val vejl_fil = "data/streglister/" ^ s ^ "/vejlederstreger.txt"
        val vejl_stroem = TextIO.openIn vejl_fil
    in
        hentStregerFraStroem vejl_stroem
        before TextIO.closeIn vejl_stroem
    end;

(* sammenfletDagStreger ((string * (string * int) list) list * (string * (string * int) list) -> (string * (string * int) list) *)
(* fun sammenfletDagStreger ([], streger) = streger *)
(*   | sammenfletDagStreger ((x :: xs), streger) = *)

(* fletParListe ('a * 'b) list * ('a * 'b) list -> ('a * 'b) list *)
fun fletPar f x [] = [x]
  | fletPar f (x, n) ((y, m) :: ys) =
    if x = y then
        (x, f(n, m)) :: ys
    else
        (y,m) :: fletPar f (x,n) ys

fun fletParListe f [] streger = streger
  | fletParListe f (x :: xs) streger = fletParListe f xs (fletPar f x streger)

fun fletStreger (x, y) = fletParListe op+ x y

fun fletPerson (pers, []) = [pers]
 | fletPerson ((ny_navn, ny_streger), (g_navn, g_streger) :: resten) =
   if ny_navn = g_navn then
       (ny_navn, fletStreger (ny_streger, g_streger)) :: resten
   else
       (g_navn, g_streger) :: fletPerson ((ny_navn, ny_streger), resten)

fun fletPersoner ([], acc) = acc
 | fletPersoner (x :: xs, acc) = fletPersoner (xs, fletPerson(x, acc));

local
    fun hentStreger' f dstroem acc =
        case FileSys.readDir dstroem of
            SOME d => hentStreger' f dstroem (fletPersoner (f d, acc))
          | NONE => acc
in
    fun hentVejlStreger () =
        let val d = FileSys.openDir "data/streglister"
        in hentStreger' hentVejlStregerFraDag d [] before FileSys.closeDir d end;
    fun hentRusStreger () =
        let val d = FileSys.openDir "data/streglister"
        in hentStreger' hentRusStregerFraDag d [] before FileSys.closeDir d end;
end;

local
    fun totalDruk' ([], acc) = acc
      | totalDruk' ((_, x) :: xs, acc) = totalDruk' (xs, fletStreger (x, acc));
in
    fun totalDruk () = totalDruk' (fletPersoner (hentVejlStreger (), hentRusStreger()), [])
end;

fun hentIndkoeb fil_navn =
    let val linjer = hentLinjer fil_navn
    in map tolkLinjeInt linjer end;

local
    fun hentAlleIndkoeb' dstroem acc =
        case FileSys.readDir dstroem of
            SOME d => hentAlleIndkoeb' dstroem (fletStreger(hentIndkoeb ("data/indkoeb/antal/" ^ d), acc))
          | NONE => acc
in
    fun hentAlleIndkoeb () =
        let val dstroem = FileSys.openDir "data/indkoeb/antal/"
        in hentAlleIndkoeb' dstroem []
           before FileSys.closeDir dstroem end;
end;

fun hentKategoriMapping fil =
    let val linjer = hentLinjer fil
        val opdelteLinjer = map (fn x => valOf (partitionerStreng #":" x)) linjer
    in
        map (fn (kategori, drikke) =>
                (kategori, String.tokens (fn x => x = #",") drikke)) opdelteLinjer
    end;

(* fraDrikTilKategori : string -> string *)
local
    fun findKategori [] drik = raise Fail ("Kategorien for " ^ drik ^ " kunne ikke findes")
      | findKategori ((kat, drikke) :: kats) drik =
        if List.exists (fn x => x = drik ) drikke then
            kat
        else findKategori kats drik
in
    fun fraDrikTilKategori drik =
        let val mapping = hentKategoriMapping "data/statisk/stregkategorier"
        in
            findKategori mapping drik
        end
end;

fun drikkeTilKategorier drikke =
    fletStreger (map (fn (drik, antal) => (fraDrikTilKategori drik, antal)) drikke, []);

fun hentOptaelling titel =
    drikkeTilKategorier (map tolkLinjeInt (hentLinjer ("data/optaellinger/"^titel^"/drikkevarer.txt")));

fun listeFratraekning (x,y) = fletParListe op- x y;

fun svind titel =
    let val indkoeb = hentAlleIndkoeb ()
        val optaelling = hentOptaelling titel
        val streger = totalDruk();
    in listeFratraekning
           (listeFratraekning (drikkeTilKategorier indkoeb, optaelling), streger)
    end;

fun hentEkstraomkostninger () =
    map tolkLinjeInt (hentLinjer ("data/indkoeb/ekstra.txt"));

fun hentPriser () = map (tolkLinje Real.fromString) (hentLinjer ("data/indkoeb/priser.txt"))

fun realAntal x = map (fn (y,z) => (y, real z)) x;

fun sumParListe xs = #2 (hd (fletStreger (map (fn (drik, antal) => ("", antal)) xs, [])));

fun foldPerson (navn, drikke) = (navn, round (#2 (hd (fletParListe op+ (map (fn (drik, antal : real) => ("", antal)) drikke) []))));

fun rusGaeld titel =
    let val indkoeb = hentAlleIndkoeb ()
        val optaelling = hentOptaelling titel
        val priser = fletParListe op+ (map (fn (drik, antal) => (fraDrikTilKategori drik, antal)) (hentPriser ())) []
        val ekstra = real (sumParListe (hentEkstraomkostninger ()));
        val russtreger = hentRusStreger ();
        val vejlstreger = hentVejlStreger ();
        val total_druk = totalDruk ();
        val antal_vaek = fletParListe op- (drikkeTilKategorier indkoeb) optaelling;
        val total_streger = real (sumParListe total_druk);
        val drikOmk = fletParListe op* priser (realAntal antal_vaek);
        val stkPris = fletParListe op/ drikOmk (realAntal total_druk);
        val ekstraStk = (ekstra / total_streger + 1.0)
        val endeligStkPris = map (fn (x,y) => (x, y + ekstraStk)) stkPris
        val rusPriser = map (fn (navn, x) => (navn, (fletParListe op* endeligStkPris (realAntal x)))) russtreger
        val vejlPriser = map (fn (navn, x) => (navn, (fletParListe op* endeligStkPris (realAntal x)))) vejlstreger
    in
        map foldPerson (vejlPriser @ rusPriser)
end;

fun skrivBetaling navn beloeb =
    let val fil = TextIO.openAppend "data/betalinger/betalinger.txt"
    in TextIO.output (fil, navn ^ ": " ^ (Int.toString beloeb) ^ "\n")
       before TextIO.closeOut fil
    end;

fun udskrivPersoner titel list =
    let val gaeld = rusGaeld titel
        fun udskrivPersoner' _ [] _ _ = ()
          | udskrivPersoner' titel (x :: xs) gaeld n =
            (print (Int.toString n ^ ": " ^ x ^ " (" ^ Int.toString (#2 (valOf (List.find (fn (navn, _) => navn = x) gaeld))) ^ " kr)\n");
             udskrivPersoner' titel xs gaeld (n+1))
    in
        udskrivPersoner' titel list gaeld 0
    end;


fun betal titel =
    let
        val personer = (hentVejledere () @ hentRusser())
        val _ = print "Hvem skal betale?\n\n";
        val _ = udskrivPersoner titel personer;
        val _ = print "\n\nIndtast indeks: ";
        val person = valOf (Int.fromString (fjernNyLinje (valOf (TextIO.inputLine TextIO.stdIn))))
        val _ = print "Indtast indbetaling i hele kroner: "
        val beloeb = valOf (Int.fromString (fjernNyLinje (valOf (TextIO.inputLine TextIO.stdIn))))
    in
        skrivBetaling (List.nth (personer, person)) beloeb
    end;
