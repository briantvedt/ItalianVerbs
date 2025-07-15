structure Project =
struct

  fun make() = CM.make("project.cm");

  fun run() = TestCases.runAll();

end
