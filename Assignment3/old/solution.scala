
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers




// Throughout the solution, we make use of the "Smurf naming convention" in
// order to disambiguate between the different languages.

object CW2 {

  /* =====  Abstract syntax tree for MiniMD  ===== */
  abstract class MiniMDExpr
  // A block of MiniMD expressions. The top-level of a program is a MDDoc.
  case class MDDoc(contents: List[MiniMDExpr]) extends MiniMDExpr

  // A paragraph of free text / bold / italic / section headers
  case class MDPar(contents: List[MiniMDExpr]) extends MiniMDExpr
  case class MDVerbatim(contents: String) extends MiniMDExpr

  // Free text
  case class MDFreeText(text: String) extends MiniMDExpr
  case class MDBold(text: String) extends MiniMDExpr
  case class MDItalic(text: String) extends MiniMDExpr
  case class MDUnderlined(text: String) extends MiniMDExpr


  // List items
  case class MDListItem(exprs: List[MiniMDExpr]) extends MiniMDExpr

  case class MDBulletedList(listItems: List[MDListItem]) extends MiniMDExpr
  case class MDNumberedList(listItems: List[MDListItem]) extends MiniMDExpr

  // Sections. Content is a block.
  case class MDSectionHeader(text: String) extends MiniMDExpr
  case class MDSubsectionHeader(text: String) extends MiniMDExpr

  // Links
  case class MDLink(text: String, url: String) extends MiniMDExpr


  /* ===== Printing Language ===== */
  import scala.language.implicitConversions


  // ======================================================================
  // Part 1: Pretty-printing
  // ======================================================================

  trait Printer {
    // The abstract Doc type
    type Doc

    // An empty document
    val nil : Doc

    // A block of text
    def text(s: String): Doc

    // A line break / newline
    val line: Doc

    // Append two Docs together
    def append(x:Doc, y: Doc): Doc

    // A doc which is nested to a certain level.
    def nest(i: Int, doc: Doc): Doc

    // A doc whose content is rendered without indentation
    // (nesting is suspended)
    def unnest(doc: Doc): Doc

    // A method which prints the Doc to a string.
    def print(doc: Doc): String

    // some Scala incantations to allow infix <> document append operation
    class AppendAssoc(x: Doc) {
      def <> (y: Doc): Doc = append(x,y)
    }
    implicit def doc2AppendAssoc(x: Doc): AppendAssoc = new AppendAssoc(x)



    // ======================================================================
    //  Exercise 1             
    // ====================================================================== 

    def quote(d1: Doc): Doc = // BEGIN ANSWER
      text("\"") <> d1 <> text("\"")
    // END ANSWER

    def braces(d1: Doc): Doc = // BEGIN ANSWER
      text("{") <> d1 <> text("}")
    // END ANSWER

    def anglebrackets(d1: Doc): Doc = // BEGIN ANSWER
      text("<") <> d1 <> text(">")
    // END ANSWER

    /* ======================================================================
     *  Exercise 2
     * ====================================================================== */
  
    def sep(d: Doc, ds: List[Doc]): Doc = // BEGIN ANSWER
      ds match {
        case Nil => nil
        case List(d) => d
        case d2::ds2 => d2 <> d <> sep(d,ds2)
      }
    // END ANSWER

  }


  // ======================================================================
  // Exercise 3
  // ======================================================================

  // BEGIN ANSWER

  abstract class DocAST
  case class AppendDoc(d1: DocAST, d2: DocAST) extends DocAST
  case class NilDoc() extends DocAST
  case class TextDoc(s: String) extends DocAST
  case class LineDoc() extends DocAST
  case class NestDoc(level: Int, doc: DocAST) extends DocAST
  case class UnnestDoc(doc: DocAST) extends DocAST


  object MyPrinter extends Printer {
    type Doc = DocAST
    def append(x: Doc, y:Doc): Doc = AppendDoc(x,y)
    val nil: Doc = NilDoc()
    def text(s: String): Doc = TextDoc(s)
    val line: Doc = LineDoc()
    def nest(i: Int,d: Doc): Doc = NestDoc(i,d)
    def unnest(d: Doc): Doc = UnnestDoc(d)
    def print(d: Doc): String = printInner(0, true, d)

    def printInner(nestLevel: Int, shouldNest: Boolean, doc: Doc): String = doc match {
      case AppendDoc(d1, d2) =>
        printInner(nestLevel, shouldNest, d1) + printInner(nestLevel, shouldNest, d2)
      case NilDoc() => ""
      case TextDoc(s) => s
      case NestDoc(level, d1) =>
        printInner(nestLevel + level, shouldNest, d1)
      case UnnestDoc(d1) =>
        printInner(nestLevel, false, d1)
      case LineDoc() =>
        if (shouldNest) {
          "\n" + (" " * nestLevel)
        } else {
          "\n"
        }
    }
  }

  object StatefulPrinter extends Printer {
    class PrinterState(var nestLevel: Int, var shouldNest: Boolean, var output: String)
    type Doc = DocAST
    def append(x: Doc, y:Doc): Doc = AppendDoc(x,y)
    val nil: Doc = NilDoc()
    def text(s: String): Doc = TextDoc(s)
    val line: Doc = LineDoc()
    def nest(i: Int,d: Doc): Doc = NestDoc(i,d)
    def unnest(d: Doc): Doc = UnnestDoc(d)
    def print(d: Doc): String = {
      var state = new PrinterState(0,true,"");
      def go(d: Doc):Unit = d match {
        case AppendDoc(d1,d2) => {
          go(d1)
          go(d2)
        }
        case NilDoc() => {}
        case TextDoc(s) => {
          state.output += s
        }
        case LineDoc() => {
          state.output += "\n"
          if (state.shouldNest) {
            state.output += (" " * state.nestLevel)
          }
        }
        case NestDoc(i,d) => {
          state.nestLevel += i
          go(d)
          state.nestLevel -= i
        }
        case UnnestDoc(d) => {
          state.shouldNest = false
          go(d)
          state.shouldNest = true
        }
      }
      go(d)
      return state.output
    }
  }

  object FunctionalPrinter extends Printer {
    type Doc = ((Int, Boolean)) => String
    val nil = {x: (Int, Boolean) => ""}
    def append(d1: Doc, d2: Doc) = {x: (Int, Boolean) => d1(x) + d2(x)}
    def text(s: String) = {x: (Int, Boolean) => s}
    val line = {x: (Int, Boolean) =>
      if (x._2) {
        ("\n" + " " * x._1)
      } else {
        ("\n")
      }
    }
    def nest(i: Int, d: Doc) = {x: (Int, Boolean) =>
      d((x._1+i,x._2))
    }
    def unnest(d: Doc) = {x: (Int, Boolean) =>
      d((x._1,false))
    }
    def print(d: Doc) = d(0,true)
  }

  // END ANSWER

  import MyPrinter._

  trait Formatter[T] {
    def format(e: T): Doc
    def formatList(xs: List[T]): Doc = sep(nil,xs.map{x: T => format(x)})
  }

  // ======================================================================
  // Exercise 4
  // ======================================================================

  object MarkdownFormatter extends Formatter[MiniMDExpr] {

    def printNumberedList(num: Int, docs: List[MDListItem]): Doc = docs match {
      case Nil => nil
      case (MDListItem(innerItems))::xs =>
        val printedItems = formatList(innerItems)

        (text(num.toString)) <> text(". ") <> printedItems <> line <> printNumberedList(num + 1, xs)
    }

    def format(e: MiniMDExpr) = e match {
      case MDDoc(xs) => formatList(xs)

      case MDPar(xs) => formatList(xs) <> line <> line

      case MDVerbatim(txt) => text("{{{") <> line <> text(txt) <> text("}}}") <> line

      case MDLink(desc, url) =>
        text("(") <> text(desc) <> text(")") <> text("[") <> text(url) <> text("]")

      case MDFreeText(txt) => text(txt)

      case MDSectionHeader(txt) => text("== ") <> text(txt) <> text(" ==") <> line

      case MDSubsectionHeader(txt) => text("=== ") <> text(txt) <> text(" ===") <> line

      case MDBold(txt) => text("*") <> text(txt) <> text("*")

      case MDItalic(txt) => text("`") <> text(txt) <> text("`")

      case MDUnderlined(txt) => text("_") <> text(txt) <> text("_")

      case MDBulletedList(listItems) => {
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc =
          listItems.foldLeft(nil) {(acc, l) =>
            val lineDoc =
              l match {
                case MDListItem(lineExprs) => formatList(lineExprs)
                }

            acc <> text("* ") <> lineDoc <> line
          }
        itemsDoc <> line
      }
      case MDNumberedList(listItems) => {
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc = printNumberedList(1, listItems)
        itemsDoc <> line
      }
    }
  }

  /* ======================================================================
   *  Exercise 5
   * ====================================================================== */

  object LatexFormatter extends Formatter[MiniMDExpr] {

    def tag(tagName: String, inner: Doc): Doc = text("\\" + tagName) <> braces(inner)
    def block(tagName: String, inner: Doc): Doc =
      text("\\begin") <> braces(text(tagName)) <> nest(2,line <> inner) <>
        line <> text("\\end") <> braces(text(tagName)) <> line

    def format(e: MiniMDExpr) = e match {
      case MDDoc(xs) => formatList(xs)

      case MDPar(xs) => formatList(xs) <> line <> line

      case MDVerbatim(txt) => unnest(text("\\begin{verbatim}") <> line <> text(txt) <> text("\\end{verbatim}")) <> line

      case MDLink(desc, url) => tag("href", text(url)) <> braces(text(desc))

      case MDFreeText(txt) => text(txt)

      case MDSectionHeader(txt) => tag("section", text(txt)) <> line

      case MDSubsectionHeader(txt) => tag("subsection", text(txt)) <> line

      case MDBold(txt) => tag("textbf", text(txt))

      case MDItalic(txt) => tag("textit", text(txt))

      case MDUnderlined(txt) => tag("underline", text(txt))

      case MDBulletedList(listItems) =>
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc = listItems.map{case MDListItem(lineExprs) =>
          text("\\item ") <> formatList(lineExprs)
        }
        block("itemize", sep(line,itemsDoc)) <> line

      case MDNumberedList(listItems) =>
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc = listItems.map{case MDListItem(lineExprs) =>
          text("\\item ") <> formatList(lineExprs)
        }
        block("enumerate", sep(line,itemsDoc)) <> line 
    }

  }

  // ======================================================================
  //  Exercise 6
  // ====================================================================== 


  object HTMLFormatter extends Formatter[MiniMDExpr] {


    def blockA(tagName: String, attrs: Doc, inner: Doc): Doc =
      text("<" + tagName) <> attrs <> text(">") <> inner <>
        text("</" + tagName + ">")
    def block(tagName: String, inner: Doc) = blockA(tagName, nil, inner)

    def attr(key: String, value: String) = text(" ") <> text(key) <> text("=") <> quote(text(value))

    def format(e: MiniMDExpr) = e match {
      case MDDoc(xs) => formatList(xs)

      case MDPar(xs) => block("p",formatList(xs)) <> line <> line

      case MDVerbatim(txt) => unnest(block("pre", text(txt))) <> line

      case MDLink(desc, url) => blockA("a", attr("href", url), text(desc))

      case MDFreeText(txt) => text(txt)

      case MDSectionHeader(txt) => block("h1", text(txt)) <> line

      case MDSubsectionHeader(txt) => block("h2", text(txt)) <> line

      case MDBold(txt) => block("b", text(txt))

      case MDItalic(txt) => block("i", text(txt))

      case MDUnderlined(txt) => block("u", text(txt))

      case MDBulletedList(listItems) => {
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc =
          listItems.foldLeft(nil) {(acc, l) =>
            val lineDoc =
              l match {
                case MDListItem(lineExprs) => formatList(lineExprs)
              }

            acc <> line <> block("li", lineDoc)
          }
        block("ul", nest(2, itemsDoc) <> line) <> line
      }

      case MDNumberedList(listItems) => {
        // Each item in a bulleted list is a list of expressions fitting on one line
        val itemsDoc =
          listItems.foldLeft(nil) {(acc, l) =>
            val lineDoc =
              l match {
                case MDListItem(lineExprs) =>
                  lineExprs.foldLeft(nil) {(lineAcc, exp) => lineAcc <> (format(exp)) }
              }
            acc <> line <> block("li", lineDoc)
          }
        block("ol", nest(2, itemsDoc) <> line) <> line
      }
    }
  }

  // ======================================================================
  // Part 2: Random test case generation
  // ======================================================================

  object Rng {
    trait Gen[+A] {
      val rng = scala.util.Random

      def get(): A // abstract

      def map[B](f: A => B):Gen[B] =
      {
        val self = this
        new Gen[B] { def get() = f(self.get()) }
      } 

      // **********************************************************************
      // Exercise 7
      // **********************************************************************

      def flatMap[B](f: A => Gen[B]) = // BEGIN ANSWER
      {
        val self = this
        new Gen[B] { def get() = f(self.get()).get() }
      }
        // END ANSWER
    }

    // **********************************************************************
    // Exercise 8
    // **********************************************************************

    def const[T](c: T): Gen[T] =
      // BEGIN ANSWER
      new Gen[T] {
        def get() = c
      }
    // END ANSWER

    def flip: Gen[Boolean] = new Gen[Boolean] {
      def get() = rng.nextBoolean()
    }

    def range(min: Integer, max: Integer): Gen[Integer] =
      // BEGIN ANSWER
      new Gen[Integer] {
        def get() = rng.nextInt(max+1 - min) + min
      }
    // END ANSWER

    def fromList[T](items: List[T]): Gen[T] =
      // BEGIN ANSWER
    {
      if (items.length < 1) {
        throw new IllegalArgumentException("Gen[T] must have at least one item to choose from.")
      }
      range(0, items.length-1).map{x: Integer => items(x)}
    }
    // END ANSWER

    // **********************************************************************
    // Exercise 9
    // **********************************************************************

    def genList[T](n: Integer, g: Gen[T]): Gen[List[T]] = // BEGIN ANSWER
      new Gen[List[T]] {
        def get() = {
          def go(n: Integer):List[T] = {
            if (n == 0) {
              Nil
            } else {
              g.get() :: go(n-1)
            }
          }
          go(n)
        }
        // END ANSWER
      }

    // **********************************************************************
    // Exercise 10
    // **********************************************************************

    def genFromList[A](gs: List[Gen[A]]): Gen[A] =
      for (g <- fromList(gs)) yield (g.get())



    // **********************************************************************
    // Exercise 11
    // **********************************************************************

    def genSectionText: Gen[String] =
      // BEGIN ANSWER
      fromList(List("Chapter 1", "Introduction", "Conclusion"))
    // END ANSWER

    def genSubsectionText: Gen[String] =
      // BEGIN ANSWER
      fromList( List("Section 1.1", "Table of Contents", "References"))
    // END ANSWER

    def genListitemText: Gen[String] =
      // BEGIN ANSWER
      fromList(List("Apples", "Oranges", "Spaceship"))
    // END ANSWER

    def genLinkText: Gen[(String, String)] =
      // BEGIN ANSWER
      fromList(List(("Google", "http://www.google.com"), ("Facebook", "http://www.facebook.com")))
    // END ANSWER

    def genFreeText: Gen[String] =
      // BEGIN ANSWER
      fromList(List("It was a dark and stormy night", "It was the best of times"))
    // END ANSWER

    def genVerbatimText: Gen[String] =
      // BEGIN ANSWER
      fromList(List("== This isn't valid MiniMD ===\n", "*Neither_ _is' 'this*\n"))
    // END ANSWER



    // **********************************************************************
    // Exercise 12
    // **********************************************************************
    // BEGIN ANSWER
    def genParagraph: Gen[MiniMDExpr] =  {
      val generators = List(
        for (x <- genFreeText) yield (MDFreeText(x)),
        for (x <- genFreeText) yield (MDItalic(x)),
        for (x <- genFreeText) yield (MDBold(x)),
        for (x <- genFreeText) yield (MDUnderlined(x)),
        for (x <- genLinkText) yield (MDLink(x._1, x._2)))
      for (i <- range(3,5);
        l <- genList(i, genFromList(generators)))
      yield (MDPar(l))
    }

    def genMiniMDElt: Gen[MiniMDExpr] = {
      val generators = List(
        genParagraph,
        for (x <- genVerbatimText) yield (MDVerbatim(x)),
        for (x <- genSectionText) yield (MDSectionHeader(x)),
        for (x <- genSubsectionText) yield (MDSubsectionHeader(x)),
        for (i <- range(2,4);
          l <- genList(i,genListitemText))
        yield (MDBulletedList(l.map{x => MDListItem(List(MDFreeText(x)))})),
        for (i <- range(2,4);
          l <- genList(i,genListitemText))
        yield (MDNumberedList(l.map{x => MDListItem(List(MDFreeText(x)))}))
      )
      genFromList(generators)
    }
        

    // END ANSWER

    def genMiniMDExpr(n: Integer): Gen[MiniMDExpr] =
      // BEGIN ANSWER
      genList(n, genMiniMDElt).map{x: List[MiniMDExpr] => MDDoc(x)}
    // END ANSWER


  }




  /*======================================================================
   The rest of this file is support code, which you should not (and do not
   need to) change.
   ====================================================================== */
  /* ===== MiniMD Parser ===== */

  object MiniMDParser extends RegexParsers {
    val eol = sys.props("line.separator")

    // Here be dragons.

    type P[+A] = Parser[A]
    private val eoi = """\z""".r // end of input
    private val separator = eoi | eol

    override val skipWhitespace = false

    // Paragraph: Either a long line of free text with the whitespace
    // stripped out, or a list with whitespace kept in
    type Line = String
    type Paragraph = String



    // Parses an input file
    def parse(input: String): MiniMDExpr = {
      val source = scala.io.Source.fromFile(input)
      val lines = try source.mkString finally source.close()
      val paragraphs = tokeniseParagraphs(lines)

      println("Paragraphs:")
      println(paragraphs + "\n\n")
      val parsedParagraphs = paragraphs.flatMap((par: Paragraph) => parseParagraph(par))

      normalise(MDDoc(parsedParagraphs))
    }

    // Top-level parse function: takes a string, returns a MiniMDExpr.
    // Throws an error upon failure.
    def parseParagraph(input: String): List[MiniMDExpr] = {
      if (input.trim.length == 0) { Nil }
      else {
        parseAll(paragraph, input) match {
          case Success(ast, _) => List(ast)
          case (e: NoSuccess) => {
            sys.error(e.msg + ", " + e.next.pos + ", " + e.next.source)
            Nil
          }
        }
      }
    }


    // Given an input string, generates a list of paragraphs
    def tokeniseParagraphs(input: String): List[Paragraph] = {

      def isEmptyLine(s: String) = s.trim.length == 0

      def isBulletListItem(s: String) = s.startsWith("* ")
      def isNumberListItem(s: String) = """^\d+\. """.r.findFirstIn(s).isDefined
      def isListItem(s: String) = isBulletListItem(s) || isNumberListItem(s)

      def isStartVerbatim(s: String) = s.trim == "{{{"
      def isEndVerbatim(s: String) = s.trim == "}}}"
      def isSection(s: String) = s.trim.startsWith("==")


      def gatherList(isBulleted: Boolean, par: Paragraph,
        remainder: List[Line]): (Paragraph, List[Line]) = remainder match {
        case Nil => (par, remainder)
        case x::xs =>
          if (isBulleted && isBulletListItem(x)) {
            gatherList(isBulleted, par + x, xs)
          } else if (!isBulleted && isNumberListItem(x)) {
            gatherList(isBulleted, par + x, xs)
          } else if (isEmptyLine(x)) {
            (par, xs)
          } else {
            (par, remainder)
          }
      }

      def gatherParagraph(par: Paragraph, remainder: List[Line]):
          (Paragraph, List[Line]) =
        remainder match {
          case Nil => (par, remainder)
          case x::xs =>
            if (isEmptyLine(x)) {
              (par, xs)
            } else if (isListItem(x) || isStartVerbatim(x)) {
              (par, remainder)
            } else {
              gatherParagraph(par + x.stripLineEnd + " ", xs)
            }
        }

      def gatherVerbatim(par: Paragraph, remainder: List[Line]):
          (Paragraph, List[Line]) =
        remainder match {
          case Nil => (par, remainder)
          case x::xs =>
            if (isEndVerbatim(x)) {
              (par + x.trim, xs)
            } else {
              gatherVerbatim(par + x, xs)
            }
        }

      def eatEmptyLines(remainder: List[Line]):
          (List[Line]) =
        remainder match {
          case Nil => Nil
          case x::xs =>
            if (isEmptyLine(x)) {
              eatEmptyLines(xs)
            } else {
              remainder
            }
        }

      def doTokeniseParagraphs(remainder: List[Line]): List[Paragraph] =
        remainder match {
          case Nil => Nil
          case x::xs =>
            if (isEmptyLine(x)) {
              doTokeniseParagraphs(xs)
            } else if (isSection(x)) {
              x.stripLineEnd::(doTokeniseParagraphs(xs))
            } else if (isStartVerbatim(x)) {
              val (par, newRemainder) = gatherVerbatim("", remainder)
              par::(doTokeniseParagraphs(newRemainder))
            } else if (isBulletListItem(x)) {
              val (par, newRemainder) = gatherList(true, "", remainder)
              par::(doTokeniseParagraphs(newRemainder))
            } else if (isNumberListItem(x)) {
              val (par, newRemainder) = gatherList(false, "", remainder)
              par::(doTokeniseParagraphs(newRemainder))
            } else {
              val (par, newRemainder) = gatherParagraph("", remainder)
              par::(doTokeniseParagraphs(newRemainder))
            }
        }

      val linesList = input.linesWithSeparators.toList
      doTokeniseParagraphs(linesList)
    }

    def delimitedSequence(delimiter: Char, breakOnNL: Boolean): P[String] = {
      (rep(delimitedChar(delimiter, breakOnNL)) <~ aChar) ^^ {
        case seq => seq.mkString
      }
    }

    def num: P[Int] = "[0-9]+".r ^^ { case n => n.toInt }

    def delimitedChar(delimiter: Char, breakOnNL: Boolean): P[Char] =
      acceptIf {ch => (ch != delimiter) || (breakOnNL && ch == eol)} {_ => "Delimiter char reached"}

    def aChar: P[Char] = Parser { in =>
      if (in.atEnd) {
        Failure("End of input", in)
      } else {
        Success(in.first, in.rest)
      }
    }

    def isDelimiterChar(c: Char): Boolean =
      c match {
        case '*' => true
        case '_' => true
        case '`' => true
        case '=' => true
        case '\r' => true
        case '\n' => true
        case '(' => true
        case '{' => true
        case _ => false
      }

    def bulletedListItem = {
      ("* " ~> listExpr) <~ eol ^^ { case e => MDListItem(e) }
    }

    def bulletedList: P[MiniMDExpr] = {
      rep1(bulletedListItem) ^^ { case seq => MDBulletedList(seq) }
    }

    def numberedListItem: P[MDListItem] =
      ((num <~ ". ") ~ listExpr) <~ eol ^^ {
        case (num ~ e) => MDListItem(e)
      }

    def numberedList: P[MiniMDExpr] = {
      rep1(numberedListItem) ^^ { case seq => MDNumberedList(seq) }
    }

    def parseDelimiterRun(breakOnNL: Boolean): P[MiniMDExpr] = {
      ("*" ~> delimitedSequence('*', breakOnNL) ^^ { case str => MDBold(str) }) |
      ("`" ~> delimitedSequence('`', breakOnNL) ^^ { case str => MDItalic(str) }) |
      ("_" ~> delimitedSequence('_', breakOnNL) ^^ { case str => MDUnderlined(str) })
    }

    def freeChar: P[Char] =
      acceptIf {ch => !isDelimiterChar(ch)} {_ => "Delimiter char reached"}

    // Parse until we hit a delimiter character.
    def freeText: P[MiniMDExpr] =
      aChar ~ rep(freeChar) ^^ { case ch ~ xs => MDFreeText((ch::xs).mkString) }

    def subsectionHeader: P[MiniMDExpr] =
      ("===" ~> "[^=]+".r <~ "===[ ]*".r) ^^ { case headertxt => MDSubsectionHeader(headertxt.trim) }

    def sectionHeader: P[MiniMDExpr] =
      ("==" ~> "[^=]+".r <~ "==[ ]*".r) ^^ { case headertxt => MDSectionHeader(headertxt.trim) }

    def link: P[MiniMDExpr] =
      ("(" ~> ("[^)]*".r) <~ ")") ~ ("[" ~> ("[^\\]]*".r) <~ "]") ^^ {
        case desc~url => MDLink(desc, url)
      }

    def verbatim: P[MiniMDExpr] =
      ("{{{" ~> eol ~> """(?s).*?(?=}}})""".r.unanchored <~ "}}}") ^^ { case vrb => MDVerbatim(vrb) }

    def listExpr: P[List[MiniMDExpr]] = {
      rep1((guard(not(eol))) ~> (parseDelimiterRun(true) | freeText))
    }

    def expr: P[MiniMDExpr] = {
      parseDelimiterRun(false) | link | freeText
    }

    def plainPar: P[MiniMDExpr] = {
      rep1(expr) ^^ { xs => MDPar(xs) }
    }

    def paragraph: P[MiniMDExpr] =
       subsectionHeader | sectionHeader | bulletedList | numberedList | verbatim | plainPar



    /* Normalisation pass.
     * We'll get a stream of FreeText / lists / section headers from
     * the parser.
     * We want to ensure that if we have two consecutive FreeTexts,
     * that they're combined into one.
     */
    def normaliseInner(es: List[MiniMDExpr]): List[MiniMDExpr] = es match {
      case Nil => Nil
      case MDFreeText(s1)::MDFreeText(s2)::xs => normaliseInner(MDFreeText(s1 ++ s2)::xs)
      case e::xs => normalise(e)::normaliseInner(xs)
    }

    def normalise(e: MiniMDExpr): MiniMDExpr = e match {
      case MDDoc(xs) => MDDoc(normaliseInner(xs))
      case MDPar(xs) => MDPar(normaliseInner(xs))
      case MDBulletedList(xs) =>
        MDBulletedList(xs.map(x => normalise(x).asInstanceOf[MDListItem]))
      case MDNumberedList(xs) =>
        MDNumberedList(xs.map(x => normalise(x).asInstanceOf[MDListItem]))
      case MDListItem(xs) => MDListItem(normaliseInner(xs))
      case e => e
    }

  }

  
  object Main {
    def usage() {
      println("MiniMD formatter version 1.8 (November 19, 2015)")
      println("Usage: scala CW2Solution.jar <infile> <mode> <outfile>")
      println("<infile> is the input file name, or RANDOM to generate a random test")
      println("<mode> is one of \"html\", \"md\" or \"latex\", and defaults to \"md\"")
      println("<outfile> is optional and defaults to \"output\"")
    }

    def getInput(infile: String): MiniMDExpr = {
      if (infile == "RANDOM") {
        println("Generating random test file...")
        Rng.genMiniMDExpr(100).get()
      }
      else {
        println("Reading " + infile)
        MiniMDParser.parse(infile)
      }
    }

    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }

    def writeHtml(outfile:String, html: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write("<html>\n<body>\n<!-- Beginning of MiniMD code -->\n")
        out.write(html)
        out.write("<!-- End of MiniMD code -->\n</body>\n")
      }
    }

    def writeMd(outfile: String, md: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write(md)
      }
    }

    def writeLatex(outfile: String, latex: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write("\\documentclass{article}\n" +
          "\\usepackage[colorlinks=true]{hyperref}\n" +
          "\\begin{document}\n" +
          "%%% Beginning of MiniMD content\n")
        out.write(latex)
        out.write("%%% End of MiniMD content\n" +
          "\\end{document}\n")
      }
    }

  }
  def main(args: Array[String]): Unit = {
    if (args.length >= 1 ) {
      val infile = args(0)
      val mode =  if (args.length >= 2) { args(1) } else { "md" }
      // Check that outfile is one of html, md or latex
      if (mode == "html" || mode == "md" || mode == "latex") {

        val parsed = Main.getInput(infile)

        mode match {
          case "html" => {
            println("Generating HTML...")
            val genHtml = print(HTMLFormatter.format(parsed))
            println(genHtml)
            if (args.length >= 3) {
              Main.writeHtml(args(2), genHtml)
            }
          }
          case "md" => {
            println("Generating MiniMD...")
            val genMd = print(MarkdownFormatter.format(parsed))
            println(genMd)
            if (args.length >= 3) {
              Main.writeMd(args(2), genMd)
            }
          }
          case "latex" => {
            println("Generating LaTeX...")
            val genLatex = print(LatexFormatter.format(parsed))
            println(genLatex)
            if (args.length >= 3) {
              Main.writeLatex(args(2),genLatex)
            }
          }
        }
      } else {
        Main.usage()
      }
    } else {
      Main.usage()
    }
  }
}

