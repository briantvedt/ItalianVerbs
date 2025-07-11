structure Project =
struct

  fun make() = CM.make("project.cm");

  fun run() = TestCases.parseTestCase "abc/def:here";

end
