structure TestCases =
struct

  exception Parse

  datatype Group = NonFinite|Present|Imperfect|PastDef|Future|Cond

  fun groupFromString string = case string
    of "non_finite" => NonFinite
     | "present" => Present
     | "imperfect" => Imperfect
     | "past_definite" => PastDef
     | "future" => Future
     | "conditional" => Cond
     | _ => raise Parse

  type testcase = {
    infinitive: string,
    group: Group,
    forms: string list
  }

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
