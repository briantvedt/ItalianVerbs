structure TestCases =
struct

  exception Parse

  datatype Group = NonFinite|Present|Imperfect|PastDef|Future|Cond

  type testcase = {
    infinitive: string,
    group: Group,
    forms: string list
  }

  fun parseHeader header =
    let
      val subs = String.fields (fn c => (c = #"/")) header
      val infinitive = hd subs
      val group = case hd (tl subs)
        of "non_finite" => NonFinite
         | "present" => Present
         | "imperfect" => Imperfect
         | "past_definite" => PastDef
         | "future" => Future
         | "conditional" => Cond
         | _ => raise Parse
    in
      (infinitive, group)
    end

  fun parseForms formtxt =
    let
      val words = String.fields (fn c => (c = #",")) formtxt
    in
      List.map (String.translate (fn c => if (c = #" ") then "" else (str c))) words
    end

  fun parseTestCase txt =
    let
      val fields = String.fields (fn c => (c = #":")) txt
      val (infinitive, group) = parseHeader (hd fields)
      val forms = parseForms (hd (tl fields))
    in
      infinitive :: forms
    end
      
  fun readTestCase strm =
    case TextIO.inputLine strm
      of SOME txt => SOME (parseTestCase txt)
       | NONE => NONE 

  fun runAll () = let
    val strm = TextIO.openIn "testcases";
    fun run_lines strm =
      case TextIO.inputLine strm of
        SOME txt => ( print txt; run_lines strm )
        | _ => ()
  in
    run_lines strm;
    TextIO.closeIn strm
  end

end
