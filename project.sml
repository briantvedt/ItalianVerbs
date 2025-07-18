structure Project =
struct

  fun make() = CM.make("project.cm");

  fun test() = TestCases.runAll();

end
