signature CONJUGATE =
sig
  datatype person = First | Second | Third
  datatype number = Singular | Plural

  val gerund : string -> string
  val present_participle : string -> string
  val past_participle : string -> string
  val present_indicative : string -> person * number -> string
  val imperfect : string -> person * number -> string
  val past_definite : string -> person * number -> string
  val future : string -> person * number -> string
  val conditional : string -> person * number -> string
end

structure Conjugate :> CONJUGATE =
struct
  datatype person = First | Second | Third
  datatype number = Singular | Plural

  exception BadInfinitive

  (* TODO: replace these with the correct unicode values *)
  structure Letter =
  struct
    val a_grave = "a`"
    val e_acute = "e'"
    val i_grave = "i`"
    val o_grave = "o`"
  end

  datatype category = Are | Ere | Ire

  fun categorize infinitive = (
    String.size infinitive >= 3 orelse raise BadInfinitive;
    case String.substring(infinitive, String.size infinitive - 3, 3)
     of "are" => Are | "ere" => Ere | "ire" => Ire
      | _ => raise BadInfinitive
  )

  fun decompose infinitive = (
    String.size infinitive >= 4 orelse raise BadInfinitive;
    (String.substring(infinitive, 0, String.size infinitive - 3), categorize infinitive)
  )

  fun gerund infinitive =
    let val (stem, cat) = (decompose infinitive) in
      stem ^ (case cat of Are => "a" | _ => "e") ^ "ndo"
    end

  fun present_participle infinitive =
    let val (stem, cat) = (decompose infinitive) in
      stem ^ (case cat of Are => "a" | _ => "e") ^ "nte"
    end

  fun past_participle infinitive =
    let val (stem, cat) = (decompose infinitive) in
      stem ^ (case cat of Are => "a" | Ere => "u" | Ire => "i") ^ "to"
    end

  fun present_indicative infinitive (person, number) =
    let val (stem, cat) = (decompose infinitive) in
      case (person, number)
        of (First, Singular) => stem ^ "o"
         | (Second, Singular) => stem ^ "i"
         | (Third, Singular) => stem ^ (case cat of Are => "a" | _ => "e")
         | (First, Plural) => stem ^ "iamo"
         | (Second, Plural) => stem ^ "ate"
         | (Third, Plural) => stem ^ "ano"
    end

  fun imperfect infinitive (person, number) =
    let val (stem, cat) = (decompose infinitive) in
      let val stemyv = stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i") ^ "v" in
        case (person, number)
          of (First, Singular) => stemyv ^ "o"
           | (Second, Singular) => stemyv ^ "i"
           | (Third, Singular) => stemyv ^ "a"
           | (First, Plural) => stemyv ^ "amo"
           | (Second, Plural) => stemyv ^ "ate"
           | (Third, Plural) => stemyv ^ "ano"
      end
    end

  fun past_definite infinitive (person, number) =
    let
      val (stem, cat) = (decompose infinitive)
      val stemy = stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i")
    in
      case (person, number)
        of (First, Singular) => stemy ^ "i"
         | (Second, Singular) => stemy ^ "sti"
         | (Third, Singular) => stem ^ "<twist>"
         | (First, Plural) => stemy ^ "mmo"
         | (Second, Plural) => stemy ^ "ste"
         | (Third, Plural) => stemy ^ "rano"
    end

  fun future infinitive (person, number) =
    let
      val (stem, cat) = (decompose infinitive)
      val stemyr = stem ^ (case cat of Ire => "i" | _ => "e") ^ "r"
    in
      case (person, number)
        of (First, Singular) => stemyr ^ Letter.o_grave
         | (Second, Singular) => stemyr ^ "ai"
         | (Third, Singular) => stemyr ^ Letter.a_grave
         | (First, Plural) => stemyr ^ "emo"
         | (Second, Plural) => stemyr ^ "ete"
         | (Third, Plural) => stemyr ^ "anno"
    end

  fun conditional infinitive (person, number) =
    let val (stem, cat) = (decompose infinitive) in
      stem ^ (case cat of Are => "a" | _ => "e") ^ "nte"
    end
end

