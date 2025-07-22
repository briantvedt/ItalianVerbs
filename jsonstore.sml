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
        (* TODO: verify montonicity of element sorts *)
        if i = Vector.length objv then result else
          let
            val curr_group = if null result then "" else #group (hd result)
            val curr_group_sort = if null result then ~1 else #sort (hd result)
            val obj = Vector.sub(objv, i)
            val group = U.asString (U.get (obj, [U.SEL "group"]))
            val group_sort = U.asInt (U.get (obj, [U.SEL "group_sort"]))
            val element = {
                value = U.asString (U.get (obj, [U.SEL "value"])),
                sort = U.asInt (U.get (obj, [U.SEL "sort"])),
                (* TODO: read form when present *)
                form = NONE
              }
            val (curr_elements, link_result) =
              if group = curr_group then
                (group_sort = curr_group_sort orelse raise Unexpected;
                  (#elements (hd result), tl result))
              else
                (group_sort > curr_group_sort orelse raise Unexpected;
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

  (* TODO: function to build 'testcases' from infinitives in 'target' *)

end
