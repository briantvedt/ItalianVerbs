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
  val subjunctive : string -> person * number -> string
  val subjunctive_imperfect : string -> person * number -> string
  val imperative : string -> person * number -> string
end

structure Conjugate :> CONJUGATE =
struct
  datatype person = First | Second | Third
  datatype number = Singular | Plural

  exception BadInfinitive

  structure Letter =
  struct
    val a_grave = "\195\160"
    val e_acute = "\195\169"
    val i_grave = "\195\172"
    val o_grave = "\195\178"
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
         | (Second, Plural) => stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i") ^ "te"
         | (Third, Plural) => stem ^ (case cat of Are => "a" | _ => "o") ^ "no"
    end

  fun imperfect infinitive (person, number) =
    let
      val (stem, cat) = (decompose infinitive)
      val stemyv = stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i") ^ "v"
    in
        case (person, number)
          of (First, Singular) => stemyv ^ "o"
           | (Second, Singular) => stemyv ^ "i"
           | (Third, Singular) => stemyv ^ "a"
           | (First, Plural) => stemyv ^ "amo"
           | (Second, Plural) => stemyv ^ "ate"
           | (Third, Plural) => stemyv ^ "ano"
    end

  fun past_definite infinitive (person, number) =
    let
      val (stem, cat) = (decompose infinitive)
      val stemy = stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i")
    in
      case (person, number)
        of (First, Singular) => stemy ^ "i"
         | (Second, Singular) => stemy ^ "sti"
         | (Third, Singular) => stem ^ (case cat of
                  Are => Letter.o_grave | Ere => Letter.e_acute | Ire => Letter.i_grave)
         | (First, Plural) => stemy ^ "mmo"
         | (Second, Plural) => stemy ^ "ste"
         | (Third, Plural) => stemy ^ "rono"
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
    let
      val (stem, cat) = (decompose infinitive)
      val stemyr = stem ^ (case cat of Ire => "i" | _ => "e") ^ "r"
    in
      case (person, number)
        of (First, Singular) => stemyr ^ "ei"
         | (Second, Singular) => stemyr ^ "esti"
         | (Third, Singular) => stemyr ^ "ebbe"
         | (First, Plural) => stemyr ^ "emmo"
         | (Second, Plural) => stemyr ^ "este"
         | (Third, Plural) => stemyr ^ "ebbero"
    end

    fun subjunctive infinitive (person, number) =
      let
        val (stem, cat) = decompose infinitive
        val stemy = stem ^ (case cat of Are => "i" | _ => "a")
      in
        case (person, number)
          of (First, Singular) => stemy
           | (Second, Singular) => stemy
           | (Third, Singular) => stemy
           | (First, Plural) => stem ^ "iamo"
           | (Second, Plural) => stem ^ "iate"
           | (Third, Plural) => stemy ^ "no"
      end

    fun subjunctive_imperfect infinitive (person, number) =
      let
        val (stem, cat) = decompose infinitive
        val stemy = stem ^ (case cat of Are => "a" | Ere => "e" | Ire => "i")
      in
        case (person, number)
          of (First, Singular) => stemy ^ "ssi"
           | (Second, Singular) => stemy ^ "ssi"
           | (Third, Singular) => stemy ^ "sse"
           | (First, Plural) => stemy ^ "ssemo"
           | (Second, Plural) => stemy ^ "ste"
           | (Third, Plural) => stemy ^ "ssero"
      end

    fun imperative infinitive (person, number) =
      let
        val (stem, cat) = decompose infinitive
        val stemy = stem ^ (case cat of Are => "i" | _ => "a")
      in
        case (person, number)
          of (First, Singular) => "" (* incorrect, actually does not exist *)
           | (Second, Singular) => stem ^ (case cat of Are => "a" | _ => "i")
           | (Third, Singular) => stemy
           | (First, Plural) => stem ^ "iamo"
           | (Second, Plural) => stemy ^ "te" (* wrong! *)
           | (Third, Plural) => stemy ^ "no"
      end

end

