// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)


#show: doc => article(
  title: [Appendix A],
  subtitle: [Survey Experiment Vignettes and Survey Items],
  paper: "us-letter",
  font: ("TeX Gyre Pagella",),
  fontsize: 11pt,
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#quarto_super(
kind: 
"quarto-float-fig"
, 
caption: 
[
#strong[Fabricated Article Vignettes utilized as Experimental Stimulus]
]
, 
label: 
<fig-vignettes>
, 
position: 
bottom
, 
supplement: 
"Figure"
, 
subrefnumbering: 
"1a"
, 
subcapnumbering: 
"(a)"
, 
[
#grid(columns: 2, gutter: 2em,
  [
#block[
#figure([
== Treatment Vignette
<treatment-vignette>
#strong[Local Military Veterans Recruited for Election Jobs in Maricopa County] \
\
~~PHOENIX (AP) --- Election officials in Maricopa County, Arizona, announced a program designed to recruit military veterans and their family members from the community to serve as election administrators, including election polling place workers, temporary workers, and full-time staff. As the U.S. general elections in November near, election officials must fill several thousand temporary positions and hundreds of other open positions to ensure sufficient staffing for the 2024 elections and beyond. \
\
~~Army veteran Jordan Braxton just joined the elections workforce. Jordan believes their role is important to ensuring a secure, accurate, and transparent election, "Many places are short on staff this election cycle. I served my country in the Army, and I want to do my part as a veteran and a citizen to ensure that everyone trusts the process and the outcome of the election."

], caption: figure.caption(
separator: "", 
position: bottom, 
[
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-treatment>


]
],
  [
#block[
#figure([
== Control Vignette
<control-vignette>
#strong[Local Residents Recruited for Election Jobs in Maricopa County] \
\
~~PHOENIX (AP) ---Election officials in Maricopa County, Arizona, announced a program to recruit members of the community to serve as election administrators, including election polling place workers, temporary workers, and full-time staff. As the U.S. general elections in November near, election officials must fill several thousand temporary positions and hundreds of other open positions to ensure sufficient staffing for the 2024 elections and beyond. \
\
~~Jordan Braxton just joined the elections workforce. Jordan believes their role is important to ensuring a secure, accurate, and transparent election, "Many places are short on staff this election cycle. I want to do my part as a citizen to ensure that everyone trusts the process and the outcome of the election."

], caption: figure.caption(
separator: "", 
position: bottom, 
[
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-control>


]
],
)
]
)
#pagebreak()
= Survey Items
<survey-items>
#figure([
#show figure: set block(breakable: true)

#let nhead = 1;
#let nrow = 4;
#let ncol = 2;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 1), (0, 2), (0, 3), (0, 4),), align: left, fontsize: 0.8em,),
(pairs: ((1, 1), (1, 2), (1, 3), (1, 4),), align: left, fontsize: 0.65em,),
(pairs: ((0, 0), (1, 0),), ),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      for style in style-array {
        let m = style.pairs.find(k => k.at(0) == it.x and k.at(1) == it.y)
        if m != none {
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
        }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    columns: (70.00%, 30.00%),
    stroke: none,
    align: (x, y) => {
      let sarray = style-array.filter(a => "align" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let sarray = style-array.filter(a => "background" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().background
      }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 5, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[Items], [Response],
    ),

    // tinytable cell content after
[How often do you pay attention to what is going on in government and politics?], [Never \    Sometimes\    About half the time\    Most of the Time\    Always],
[In general, how favorable or unfavorable is your impression of local election officials?], [Strongly unfavorable\    Somewhat unfavorable\    Neither favorable nor unfavorable\    Somewhat favorable\    Strongly favorable],
[Regardless of whom you supported in the 2020 election, do you think Joe Biden's election as president was legitimate, or was he not legitimately elected?], [Legitimate\    Not legitimate],
[Generally speaking, how often can you trust other people?], [Never \    Sometimes\    About half the time\    Most of the Time\    Always],

    // tinytable footer after

  ) // end table

  ]) // end align
], caption: figure.caption(
position: top, 
[
Pre-Treatment Survey Items and Response Options
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-1>


#pagebreak()
#figure([
#show figure: set block(breakable: true)

#let nhead = 1;
#let nrow = 12;
#let ncol = 2;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 8), (0, 9), (0, 10), (0, 11), (0, 12),), align: left, fontsize: 0.8em, indent: 2em,),
(pairs: ((0, 1), (0, 7),), align: left, fontsize: 0.8em, bold: true,),
(pairs: ((1, 1), (1, 7),), align: left, fontsize: 0.65em, bold: true,),
(pairs: ((1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 8), (1, 9), (1, 10), (1, 11), (1, 12),), align: left, fontsize: 0.65em,),
(pairs: ((0, 0), (1, 0),), ),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      for style in style-array {
        let m = style.pairs.find(k => k.at(0) == it.x and k.at(1) == it.y)
        if m != none {
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
        }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    columns: (70.00%, 30.00%),
    stroke: none,
    align: (x, y) => {
      let sarray = style-array.filter(a => "align" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let sarray = style-array.filter(a => "background" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().background
      }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 8, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 13, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[Trust], [Response],
    ),

    // tinytable cell content after
table.cell(colspan: 2)[AZ],
[How confident are you that votes in Maricopa County, AZ will be counted as voters intend in the elections this November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
[How confident are you that election officials, their staff, and volunteers in Maricopa County, AZ will do a good job conducting the elections this November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very committed],
[Think about the election staff and volunteers who handle the administration and conduct of elections in Maricopa County, AZ. How committed do you think they will be to making sure the elections held this November are fair and accurate?], [Not at all committed\    Not too committed\    Somewhat committed\    Very committed],
[How confident are you that the voting process will be fair in Maricopa County, AZ?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
[How confident are you that election systems in Maricopa County, AZ will be secure from hacking and other technological threats?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
table.cell(colspan: 2)[Local Area],
[Think about vote counting throughout your local area, and not just your own personal vote. How confident are you that votes in your community will be counted as voters intend in the elections this November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
[How confident are you that election officials, their staff, and volunteers in your local community will do a good job administering the elections this November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
[Now think about the election staff and volunteers who handle the administration and conduct of elections in your local area. How committed do you think they will be to making sure the elections held this November are fair and accurate?], [Not at all committed\    Not too committed\    Somewhat committed\    Very committed],
[How confident are you that the voting process will be fair in your local area?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
[How confident are you that election systems in your local area are secure from hacking and other technological threats?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
    // tinytable footer after
  ) // end table

  ]) // end align
], caption: figure.caption(
position: top, 
[
Trust Items and Response Options
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-2>


#pagebreak()
#figure([
#show figure: set block(breakable: true)

#let nhead = 1;
#let nrow = 6;
#let ncol = 2;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 2), (0, 3), (0, 4), (0, 5), (0, 6),), align: left, fontsize: 0.8em, indent: 1em,),
(pairs: ((0, 1),), align: left, fontsize: 0.8em, bold: true,),
(pairs: ((1, 1),), align: left, fontsize: 0.65em, bold: true,),
(pairs: ((1, 2), (1, 3), (1, 4), (1, 5), (1, 6),), align: left, fontsize: 0.65em,),
(pairs: ((0, 0), (1, 0),), ),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      for style in style-array {
        let m = style.pairs.find(k => k.at(0) == it.x and k.at(1) == it.y)
        if m != none {
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
        }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    columns: (70.00%, 30.00%),
    stroke: none,
    align: (x, y) => {
      let sarray = style-array.filter(a => "align" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let sarray = style-array.filter(a => "background" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().background
      }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 7, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[Distrust], [Response],
    ),

    // tinytable cell content after
table.cell(colspan: 2)[How likely do you think any or all of the following will happen during this year´s elections in Maricopa County, AZ/your local area?],
[There will be voter fraud, that is, people who are not eligible to vote will vote, or vote more than once], [Not likely at all\    Not too likely\    Somewhat likely\    Very likely],
[Many votes will not actually be counted], [Not likely at all\    Not too likely\    Somewhat likely\    Very likely],
[Many people will show up to vote and be told they are not eligible], [Not likely at all\    Not too likely\    Somewhat likely\    Very likely],
[A foreign country will tamper with the votes cast in this area to change the results], [Not likely at all\    Not too likely\    Somewhat likely\    Very likely],
[Election officials in Maricopa County, Arizona will try to discourage some people from voting], [Not likely at all\    Not too likely\    Somewhat likely\    Very likely],
    // tinytable footer after
  ) // end table

  ]) // end align
], caption: figure.caption(
position: top, 
[
Distrust Items and Response Options
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-3>


#pagebreak()
#figure([
#show figure: set block(breakable: true)

#let nhead = 1;
#let nrow = 6;
#let ncol = 2;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 2), (0, 3), (0, 5), (0, 6),), align: left, fontsize: 0.8em, indent: 2em,),
(pairs: ((0, 1), (0, 4),), align: left, fontsize: 0.8em, bold: true,),
(pairs: ((1, 1), (1, 4),), align: left, fontsize: 0.65em, bold: true,),
(pairs: ((1, 2), (1, 3), (1, 5), (1, 6),), align: left, fontsize: 0.65em,),
(pairs: ((0, 0), (1, 0),), ),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      for style in style-array {
        let m = style.pairs.find(k => k.at(0) == it.x and k.at(1) == it.y)
        if m != none {
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
        }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    columns: (70.00%, 30.00%),
    stroke: none,
    align: (x, y) => {
      let sarray = style-array.filter(a => "align" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let sarray = style-array.filter(a => "background" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().background
      }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 5, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 7, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[Safety], [Response],
    ),

    // tinytable cell content after
table.cell(colspan: 2)[AZ],
[Thinking about Maricopa County, AZ, how concerned should voters feel about potential violence, threats of violence, or intimidation while voting in person at their local polling place?], [Very concerned\    Somewhat concerned\    Not too concerned\    Not at all concerned],
[How confident, if at all, are you that in person polling places in Maricopa County, AZ will be safe places for voters to cast their ballots during the upcoming elections in November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
table.cell(colspan: 2)[Local Area],
[Thinking about your local area, how concerned should voters feel about potential violence, threats of violence, or intimidation while voting in person at their local polling place?], [Very concerned\    Somewhat concerned\    Somewhat unconcerned\    Not at all concerned],
[How confident, if at all, are you that in person polling places in your local area will be safe places for voters to cast their ballots during the upcoming elections in November?], [Not at all confident\    Not too confident\    Somewhat confident\    Very confident],
    // tinytable footer after
  ) // end table

  ]) // end align
], caption: figure.caption(
position: top, 
[
Safety Items and Response Options
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-4>






