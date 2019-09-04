/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_2

import scala.language.reflectiveCalls
import uk.gov.nationalarchives.csv.validator.schema.{Literal, ArgProvider, QuoteChar, GlobalDirective}
import uk.gov.nationalarchives.csv.validator.schema.v1_1.{SchemaParser => SchemaParser1_1}

trait SchemaParser extends SchemaParser1_1 {

  /**
   * [59] StringProvider ::= ColumnRef | StringLiteral
   */
  override lazy val stringProvider: PackratParser[ArgProvider] = "StringProvider" ::= noext | concat | urlDecode | columnRef | stringLiteral ^^ {
    s => Literal(Some(s))
  }

  lazy val urlDecode: PackratParser[ArgProvider] = "UriDecode" ::= "uriDecode(" ~> stringProvider ~ opt("," ~> stringProvider)  <~ ")" ^^ {
    case value ~ charset => UriDecode(value, charset)
  }

  lazy val quotedLiteral: Parser[Char] = "QuoteLiteral" ::= "\"" ~> """[^"]*""".r <~ "\""  ^^  {
    _.head
  }

  /**
    * [60] QuoteChar ::=  CharacterLiteral
    */
  lazy val quoteChar: PackratParser[QuoteChar] = "QuoteChar" ::= (characterLiteral | quotedLiteral) ^^ {
    QuoteChar(_)
  }

  /**
    * [61] QuoteCharDirective ::= DirectivePrefix "quote" SeparatorChar
    */
  lazy val quoteCharDirective: PackratParser[QuoteChar] = "QuoteCharDirective" ::= directivePrefix ~> "quoteChar" ~> quoteChar

  /**
    * [4] GlobalDirectives	::=	SeparatorDirective? QuotedDirective? TotalColumnsDirective? PermitEmptyDirective? (NoHeaderDirective | IgnoreColumnNameCaseDirective)?   /* expr: unordered */
    */
  override lazy val globalDirectives: PackratParser[List[GlobalDirective]] = "GlobalDirectives" ::= opt(mingle(List(separatorDirective, quotedDirective, quoteCharDirective, totalColumnsDirective, permitEmptyDirective, noHeaderDirective | ignoreColumnNameCaseDirective).map(positioned(_) <~ opt(eol))).withFailureMessage("Invalid global directive")) ^^ {
    _.getOrElse(List.empty)
  }

}
