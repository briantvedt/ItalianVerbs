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

  fun present_participle infinitive = ""
  fun past_participle infinitive = ""
  fun present_indicative infinitive (person, number) = ""
  fun imperfect infinitive (person, number) = ""
  fun past_definite infinitive (person, number) = ""
  fun future infinitive (person, number) = ""
  fun conditional infinitive (person, number) = ""
end

