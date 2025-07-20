structure JsonStore =
struct

  exception Store

  structure P = OS.Path

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

  (* TODO: function to build 'testcases' from infinitives in 'target' *)

end
