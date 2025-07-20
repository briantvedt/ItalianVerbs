structure JsonStore =
struct

  exception Store

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

  fun parse_entry (infinitive) : {group:string, sort:int,
            elements:{value:string, sort:int, form:string option} list} list =
    let
      val file = JSONParser.openFile (get_entry_path infinitive)
      val root = JSONParser.parse file
      val objs = U.asArray (U.get (root, [U.SEL "conjugations"]))
      val result = List.tabulate(Vector.length objs, fn i =>
            let val obj = Vector.sub(objs, i) in {
                group = U.asString (U.get (obj, [U.SEL "group"])),
                sort = U.asInt (U.get (obj, [U.SEL "group_sort"])),
                elements = []
              }
            end)
    in
      JSONParser.close file;
      result
    end

  (* TODO: function to build 'testcases' from infinitives in 'target' *)

end
