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

  fun gerund infinitive = ""
  fun present_participle infinitive = ""
  fun past_participle infinitive = ""
  fun present_indicative infinitive (person, number) = ""
  fun imperfect infinitive (person, number) = ""
  fun past_definite infinitive (person, number) = ""
  fun future infinitive (person, number) = ""
  fun conditional infinitive (person, number) = ""
end

