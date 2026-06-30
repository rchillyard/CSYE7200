# CSYE 7200 — Building and Testing the Course Repository

## The Repository

The repository is provided for you as an adjunct to the course.
We follow the principles of [test-driven development](https://en.wikipedia.org/wiki/Test-driven_development) (TDD) and [continuous integration](https://en.wikipedia.org/wiki/Continuous_integration) (CI). 
The code is made up of three types:

- **Assignments** — these contain the code you need to implement the assignments. The module names begin with `assignment-`.
- **Labs** — these contain the code you need to implement in our "lab" sessions. The module names begin with `lab-`.
- **Exemplars** — these contain code which you can consult to give you more detail on the topics covered in the lectures. The module names begin with `ex-` or `spark-`.

You can find the repository at https://github.com/rchillyard/CSYE7200.git.
There will typically be a new branch created for each semester (the default branch--if I remember).

## The Short Version

When you first clone the repository and run all the tests, here is what to expect:

- **Two compile errors** in `lab-sorted` — these are expected and explained below. All other modules should compile cleanly.
- **Many tests will be marked as cancelled** (shown in grey) — this is intentional. Cancelled tests indicate code you still need to implement.
- **A small number of tests will pass** (shown in green) — these test infrastructure that is already complete.
- **No tests should be red (failing)** — if you see red failures, something unexpected has gone wrong.

Your goal throughout the course is to implement the code marked with `// TO BE IMPLEMENTED` (or `???`) in each assignment module until all tests turn green.

---

## The Compile Errors in `lab-sorted`

When you build the whole project, you will see two identical compile errors in the `lab-sorted` module:

```
error: could not find implicit value for parameter comparer: Comparer[String]
```

This is intentional. Part of your work in `lab-sorted` is to define an `implicit` (or `given`) instance of `Comparer[String]`. Until you do, the module cannot compile. This is the only module in the repository where a missing student implementation causes a compile error rather than a cancelled test — it's unavoidable given the way Scala resolves implicit parameters at compile time rather than at runtime.

**What to do:** Work on `lab-sorted` as directed in the lab session. Once you provide the `Comparer[String]` instance, the compile errors will disappear and the tests will either pass or cancel as expected.

**In the meantime:** You can build and test all other modules without touching `lab-sorted`. In sbt, you can exclude it:

```
sbt "all/test" -- excludes lab-sorted
```

Or simply work module by module:

```
sbt "assignment-hello-world/test"
sbt "assignment-lazy/test"
```

---

## Why Are So Many Tests Cancelled?

The course repository is set up so that you can run all tests at any time without being overwhelmed by red failures. Instead of failing when it encounters unimplemented code, the test framework detects the missing implementation and marks the test as **cancelled**.

A cancelled test message looks like this:

```
- should compute the sum of a list !!! CANCELED
  You need to implement the code at MyList.scala:42
```

The message tells you exactly which file and line number contains the code you need to implement. Find that location, replace the `???` with your own implementation, and re-run the tests.

This is powered by a custom ScalaTest mixin called `CancelOnNotImplemented` — you do not need to understand its internals, but here is what it does for you:

- Any test that would fail due to a `???` (unimplemented method) is automatically marked as cancelled instead of failed.
- The cancellation message includes the file name and line number of the missing implementation.
- This works for direct method calls, as well as methods wrapped in `Try` or `Future`.

---

## What `// TO BE IMPLEMENTED` Means

Throughout the source files in the assignment modules, you will see markers like:

```scala
def myMethod(x: Int): Int = ??? // TO BE IMPLEMENTED
```

or

```scala
lazy val result: String = ??? // TO BE IMPLEMENTED
```

These are the places where you write your code. Replace the `???` with a working implementation. The `???` is Scala's built-in way of marking unimplemented code — it compiles fine but throws a `NotImplementedError` at runtime, which is what the `CancelOnNotImplemented` mechanism catches.

A few things to note:

- **Do not edit Spec files** (files ending in `Spec.scala`) unless the assignment explicitly says you may. The tests are written for you.
- **`lazy val` vs `val`**: some implementations use `lazy val` rather than `val`. This is intentional — it prevents the object from failing to initialise before your implementation is in place.
- **Follow the `// TO BE IMPLEMENTED` markers** — they are the complete list of what you need to write for each assignment.

---

## Running Tests

### All tests (except lab-sorted until implemented)

```bash
sbt test
```

### A single module

```bash
sbt "assignment-lazy/test"
```

### From IntelliJ

Open the sbt tool window (**View → Tool Windows → sbt**), type `test`, and press Enter. Or right-click a Spec file and choose **Run**.

---

## Environment Requirements

- **JDK 21** (LTS) — required. Hadoop/Spark do not yet support JDK 23 or later.
  Download from [https://adoptium.net](https://adoptium.net) — choose **Temurin 21**.
- **Scala 3.3.x** — configured automatically by sbt; you do not need to install it separately.
- **sbt 1.9.9** — the build tool. Download from [https://www.scala-sbt.org](https://www.scala-sbt.org).
- **IntelliJ IDEA** (Community edition is fine) with the Scala plugin installed.

See the **Setup** assignment on Canvas for full installation instructions.

---

## Summary

| Symptom | What it means | What to do |
|---------|--------------|------------|
| Compile error in `lab-sorted` | `Comparer[String]` not yet defined | Implement it in the lab session |
| Test cancelled (grey) | Method not yet implemented | Find the file:line in the message and implement it |
| Test passing (green) | Implementation correct | Nothing — well done! |
| Test failing (red) | Unexpected error | Check your implementation; ask on Slack or Canvas |

If you are seeing red failures unexpectedly, please post in the class Slack workspace or Canvas discussion board before assuming something is wrong with the repository.