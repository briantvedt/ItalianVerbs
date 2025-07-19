structure TestCases =
struct

  exception Parse

  datatype Group = NonFinite|Present|Imperfect|PastDef|Future|Cond

  structure C = Conjugate

  val subject_classes = [
        (C.First, C.Singular), (C.Second, C.Singular), (C.Third, C.Singular),
        (C.First, C.Plural), (C.Second, C.Plural), (C.Third, C.Plural)
      ]

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
      List.map (String.translate
                 (fn c => if (c = #" " orelse c = #"\n") then "" else (str c))) words
    end

  fun parseTestCase txt =
    let
      val fields = String.fields (fn c => (c = #":")) txt
      val (infinitive, group) = parseHeader (hd fields)
      val forms = parseForms (hd (tl fields))
    in
      {infinitive=infinitive, group=group, forms=forms}:testcase
    end
      
  fun readTestCase strm =
    case TextIO.inputLine strm
      of SOME txt => SOME (parseTestCase txt)
       | NONE => NONE 

  fun verify ({infinitive, group, forms}:testcase) =
    let
      val actual = case group
        of NonFinite => [C.gerund infinitive, C.present_participle infinitive,
                         C.past_participle infinitive]
         | Present => map (C.present_indicative infinitive) subject_classes
         | Imperfect => map (C.imperfect infinitive) subject_classes
         | PastDef => map (C.past_definite infinitive) subject_classes
         | Future => map (C.future infinitive) subject_classes
         | Cond => map (C.conditional infinitive) subject_classes
    in
      app (fn (x, y) => print (x ^ " " ^ y ^ "\n"))
        (List.mapPartial (fn (x:string, y:string) => if (x = y) then NONE else SOME (x, y))
          (ListPair.zipEq (actual, forms)))
    end

  fun runAll () = let
    val strm = TextIO.openIn "testcases";
    fun run_lines strm =
      case TextIO.inputLine strm of
        SOME txt => ( verify (parseTestCase txt); run_lines strm )
        | _ => ()
  in
    run_lines strm;
    TextIO.closeIn strm
  end

end
