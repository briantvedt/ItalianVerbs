structure JsonStore =
struct

  exception Store

  fun get_store_location () =
    case OS.Process.getEnv "IT_JSON_STORE"
      of SOME loc => loc
       | NONE => raise Store

  (* TODO: function to build 'testcases' from infinitives in 'target' *)

end
