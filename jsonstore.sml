structure JsonStore =
struct

  (* for there is a problem reading the store itself *)
  exception Store

  (* if something unexpected is encountered in the JSON data *)
  exception Unexpected

  structure P = OS.Path
  structure U = JSONUtil

  fun get_location () =
    case OS.Process.getEnv "IT_JSON_STORE"
      of SOME loc => loc
       | NONE => raise Store

  fun get_entry_path infinitive =
    P.joinDirFile {
      file = P.joinBaseExt {base=infinitive, ext=SOME "json"},
      dir = P.joinDirFile {
        file = String.substring (infinitive, 0, 1),
        dir = P.joinDirFile {
          file = "content",
          dir = get_location()
        }
      }
    }

  fun preparse_entry (infinitive) : {group:string, sort:int,
            elements:{value:string, sort:int, form:string option} list} list =
    let
      val file = JSONParser.openFile (get_entry_path infinitive)
      val root = JSONParser.parse file
      val objv = U.asArray (U.get (root, [U.SEL "conjugations"]))
      fun loop i result =
        if i = Vector.length objv then result else
          let
            val curr_group = if null result then "" else #group (hd result)
            val curr_group_sort = if null result then ~1 else #sort (hd result)
            val curr_sort = if null result then ~1 else #sort (hd (#elements (hd result)))
            val obj = Vector.sub(objv, i)
            val group = U.asString (U.get (obj, [U.SEL "group"]))
            val group_sort = U.asInt (U.get (obj, [U.SEL "group_sort"]))
            val element = {
                value = U.asString (U.get (obj, [U.SEL "value"])),
                sort = U.asInt (U.get (obj, [U.SEL "sort"])),
                form = if U.hasField "form" obj then SOME (U.asString (U.get (obj, [U.SEL "form"]))) else NONE
              }
            val (curr_elements, link_result) =
              if group = curr_group then
                (group_sort = curr_group_sort orelse raise Unexpected;
                 (#sort element = curr_sort + 1 andalso #sort element <= 5) orelse raise Unexpected;
                  (#elements (hd result), tl result))
              else
                (group_sort > curr_group_sort orelse raise Unexpected;
                 #sort element = 0 orelse raise Unexpected;
                  ([], result))
          in
            loop (i+1) ({
                group = group, sort = group_sort,
                elements = element::curr_elements
              } :: link_result)
          end
    in
      JSONParser.close file;
      loop 0 []
    end

  fun parse_entry (infinitive) : (string * string list) list =
    List.revMap
      (fn grp => (#group grp, List.revMap (fn elt => #value elt) (#elements grp)))
    (preparse_entry infinitive)

  fun write_testcase (strm, lemma) =
    let
      val entry = parse_entry(lemma)
      fun write_line str = TextIO.outputSubstr(strm, Substring.full (str ^ "\n"))       
      val gerund = ref "";
      val prespart = ref "";
      val pastpart = ref "";
      fun cleanup strings =
        map (fn s =>
                let val fs = String.fields (fn ch => ch = #",") s in
                      List.nth(fs, 0) end
            ) strings
      fun maybe_write_item item =
          case item
            of ("indicative/present", forms) =>
                  write_line (lemma ^ "/present:" ^ String.concatWith "," forms)
             | ("indicative/imperfect", forms) =>
                  write_line (lemma ^ "/imperfect:" ^ String.concatWith "," forms)
             | ("indicative/pasthistoric", forms) =>
                  write_line (lemma ^ "/past_definite:" ^ String.concatWith "," forms)
             | ("indicative/future", forms) =>
                  write_line (lemma ^ "/future:" ^ String.concatWith "," forms)
             | ("conditional/present", forms) =>
                  write_line (lemma ^ "/conditional:" ^ String.concatWith "," forms)
             | ("subjunctive/present", forms) =>
                  write_line (lemma ^ "/subjunctive:" ^ String.concatWith "," forms)
             | ("subjunctive/imperfect", forms) =>
                  write_line (lemma ^ "/subjunctive_imperfect:" ^ String.concatWith "," forms)
             | ("imperative", forms) =>
                  write_line (lemma ^ "/imperative:" ^ String.concatWith "," (cleanup forms))
             | ("gerund", forms) => (gerund := List.nth(forms, 0))
             | ("pastparticiple", forms) => (pastpart := List.nth(forms, 0))
             | ("presentparticiple", forms) => (prespart := List.nth(forms, 0))
             | _ => ()
    in
      ( app maybe_write_item entry;
        write_line (lemma ^ "/non_finite:" ^ String.concatWith "," [!gerund, !prespart, !pastpart]) )
    end

  fun generate_testcases() =
    let
      val strip = String.translate (fn ch => if Char.isSpace ch then "" else String.str ch);
      (* TODO: write to 'testcases' file *)
      val outputStrm = TextIO.stdOut;
      fun loop strm =
        case TextIO.inputLine strm
          of SOME txt => (write_testcase (outputStrm, strip txt); loop strm)
           | NONE => TextIO.closeIn strm (* TextIO.closeOut outputStrm *)
    in
      loop (TextIO.openIn "target")
    end

end
