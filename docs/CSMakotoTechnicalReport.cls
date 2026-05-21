%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Makoto Technical Report
% LaTeX Class
% Version 1.0 (December 6, 2024)
%
% This class originates from:
% https://www.LaTeXTemplates.com
%
% Author:
% Vel (vel@latextemplates.com)
%
% License:
% CC BY-NC-SA 4.0 (https://creativecommons.org/licenses/by-nc-sa/4.0/)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%	CLASS CONFIGURATION
%----------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{CSMakotoTechnicalReport}[2024/12/06 Creodocs Makoto Technical Report Class v1.0]

\usepackage{etoolbox} % Required for conditional logic and easily changing commands

\newtoggle{unnumberedsections} % Create toggle for a class option
\settoggle{unnumberedsections}{false} % Default value for the class option
\DeclareOption{unnumberedsections}{\settoggle{unnumberedsections}{true}} % Set the class option toggle if the class option was used in the template

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}} % Pass through any extra options specified to the base class
\ProcessOptions\relax % Process class options

\LoadClass{article} % Load the base class

%----------------------------------------------------------------------------------------
%	REQUIRED PACKAGES AND MISC CONFIGURATIONS
%----------------------------------------------------------------------------------------

\usepackage{graphicx} % Required for including images
\graphicspath{{Images/}{./}} % Specifies where to look for included images (trailing slash required)

\setlength{\parindent}{0pt} % Paragraph indentation
\setlength{\parskip}{6pt} % Vertical space between paragraphs

\usepackage[bottom, hang]{footmisc} % Force footnotes to the bottom of the page and to the left margin
\setlength{\footnotemargin}{6pt} % Horizontal space between the footnote marker and text

\usepackage{lastpage} % Required to determine the total number of pages

\AtBeginEnvironment{quote}{\itshape\rmfamily} % Style quotations

\usepackage{appendix} % Required for the appendices section

\usepackage{float} % Allows more precisely positioning floats e.g. \begin{figure}[H]

\usepackage{multicol} % Required for outputting content in multiple columns on demand

%----------------------------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------------------------

\usepackage[
	top=3cm, % Top margin
	bottom=3cm, % Bottom margin
	left=1.75cm, % Left margin
	right=1.75cm, % Right margin
	footskip=1.5cm, % Space from the bottom margin to the baseline of the footer
	headsep=0.8cm, % Space from the top margin to the baseline of the header
	headheight=1.5cm, % Height of the header
	columnsep=0.9cm, % Space between text columns (in twocolumn mode)
	%showframe % Uncomment to show frames around the margins for debugging purposes
]{geometry}

%----------------------------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------------------------

\usepackage{fontspec}
\defaultfontfeatures{Ligatures=TeX}

% Use IBM Plex Sans for everything since that's what you have
\setsansfont{IBM Plex Sans}[
UprightFont = * Light,
BoldFont = * SmBld,
ItalicFont = * Light Italic,
BoldItalicFont = * SmBld Italic
]

% Set sans as default
\renewcommand*\familydefault{\sfdefault}

% For serif, fall back to IBM Plex Sans (or install IBM Plex Serif)
\setmainfont{IBM Plex Sans}[
UprightFont = * Light,
BoldFont = * SmBld,
ItalicFont = * Light Italic,
BoldItalicFont = * SmBld Italic
]

% For mono, fall back to IBM Plex Sans (or install IBM Plex Mono)
\setmonofont{IBM Plex Sans}[
UprightFont = * Light,
BoldFont = * SmBld,
ItalicFont = * Light Italic,
BoldItalicFont = * SmBld Italic
]

% Condensed fallback
\newfontfamily{\plexsanscondensed}{IBM Plex Sans}[
UprightFont = * Light,
BoldFont = * SmBld,
ItalicFont = * Light Italic,
BoldItalicFont = * SmBld Italic
]

\newfontface{\plexsansmedium}{IBM Plex Sans Medm}

%----------------------------------------------------------------------------------------
%	HEADERS AND FOOTERS
%----------------------------------------------------------------------------------------

\usepackage{fancyhdr} % Required for customizing headers and footers
\pagestyle{fancy} % Enable custom headers and footers

\renewcommand{\headrulewidth}{0.5pt} % Top horizontal rule thickness

\fancyhf{} % Clear default headers/footers

\fancyhead[L]{\leftheadercontent} % Left header
\fancyhead[C]{} % Center header
\fancyhead[R]{\rightheadercontent} % Right header

\fancyfoot[L]{} % Left footer
\fancyfoot[C]{\thepage~of \pageref{LastPage}} % Center footer
\fancyfoot[R]{} % Right footer

%----------------------------------------------------------------------------------------
%	SECTIONS
%----------------------------------------------------------------------------------------

\usepackage{titlesec} % Required for modifying sections

\iftoggle{unnumberedsections}{ % Conditional logic for the unnumbered sections class options
	\setcounter{secnumdepth}{0} % Don't number sections at any level
}{
	\setcounter{secnumdepth}{3} % Number sections down to subsubsections
}

\titleformat
	{\section} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\Large\plexsansmedium} % Text formatting of the whole section, i.e. label and title
	{\thesection} % Section label (e.g. number) and its formatting
	{7pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\section}{0pt}{0.75\baselineskip}{4pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\plexsansmedium} % Text formatting of the whole section, i.e. label and title
	{\thesubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsection}{0pt}{0.75\baselineskip}{0pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsubsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\bfseries\small} % Text formatting of the whole section, i.e. label and title
	{\thesubsubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsubsection}{0pt}{0.5\baselineskip}{0pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\paragraph} % Section type being modified
	[runin] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\bfseries\small} % Text formatting of the whole section, i.e. label and title
	{} % Section label (e.g. number) and its formatting
	{0pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[~~~~] % Code after the section title

\titlespacing*{\paragraph}{0pt}{0pt}{0pt} % Spacing around section titles, the order is: left, before and after

%----------------------------------------------------------------------------------------
%	TABLE OF CONTENTS
%----------------------------------------------------------------------------------------

\usepackage{titletoc} % Required for manipulating the table of contents

\setcounter{tocdepth}{1} % Show entries in the table of contents down to sections

\titlecontents{section} % Section type being modified
	[0pt] % Left indentation
	{\vspace{-\parskip}} % Before code
	{\plexsansmedium\thecontentslabel.~~} % Formatting of numbered sections of this type
	{\plexsansmedium} % Formatting of numberless sections of this type
	{\titlerule*[4pt]{.}\plexsansmedium\contentspage} % Formatting of the filler to the right of the heading and the page number
	[] % After code

%----------------------------------------------------------------------------------------
%	BIBLIOGRAPHY
%----------------------------------------------------------------------------------------

\usepackage[
	backend=biber, % Use the biber backend for compiling the bibliography
	citestyle=numeric, % In-text citation style
	bibstyle=numeric, % Bibliography style
]{biblatex}
\addbibresource{references.bib}

%----------------------------------------------------------------------------------------
%	TABLES
%----------------------------------------------------------------------------------------

\usepackage{booktabs} % Required for better horizontal rules in tables

\usepackage{array} % Required for manipulating table columns
\renewcommand{\arraystretch}{1.25} % Increase the height of table rows

\newcolumntype{R}[1]{>{\raggedleft\arraybackslash}p{#1}} % Define a new right-aligned paragraph column type
\newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}} % Define a new left-aligned (no justification) paragraph column type
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}} % Define a new centered paragraph column type

%----------------------------------------------------------------------------------------
%	CAPTIONS
%----------------------------------------------------------------------------------------

\usepackage{caption} % Required for customizing captions

\captionsetup{skip=6pt} % Vertical whitespace between figures/tables and the caption (default is 10pt)
\captionsetup{font={bf,small}} % Define caption font style
\captionsetup{justification=raggedright, singlelinecheck=false} % Left-align captions

%----------------------------------------------------------------------------------------
%	LISTS
%----------------------------------------------------------------------------------------

\usepackage{enumitem} % Required for list customization

\setlist{itemsep=0pt, topsep=0pt} % Customize spacing around and inside lists

\renewcommand{\labelenumi}{\arabic{enumi}.} % Customize the first level numbered list characters
\renewcommand{\labelenumii}{\alph{enumii}.} % Customize the second level numbered list characters
\renewcommand{\labelenumiii}{\roman{enumiii}.} % Customize the third level numbered list characters

\renewcommand{\labelitemi}{\Large\raisebox{-1.5pt}{\textbullet}} % Customize the first level bullet point character
\renewcommand{\labelitemii}{\Large\raisebox{-1.5pt}{--}} % Customize the second level bullet point character
\renewcommand{\labelitemiii}{\Large\raisebox{-1.5pt}{-}} % Customize the third level bullet point character

%----------------------------------------------------------------------------------------
%	LINKS
%----------------------------------------------------------------------------------------

\usepackage{hyperref} % Required for links

\hypersetup{
	colorlinks=true, % Whether to color the text of links
	urlcolor=black, % Color for \url and \href links
	linkcolor=black, % Color for \nameref links
	citecolor=black, % Color of reference citations
}

%----------------------------------------------------------------------------------------
%	CODE LISTINGS
%----------------------------------------------------------------------------------------

\usepackage{listings} % Required for formatted code blocks

\lstset{
	numbers=left, % Line numbers location
	numbersep=6pt, % Whitespace to the right of line numbers
	numberstyle=\tiny\it, % Line numbers font style
	frame=single, % Border
	basicstyle=\tt\scriptsize, % Code font style
	breaklines=true, % Break long lines into multiple lines automatically
	aboveskip=1.5\baselineskip, % Vertical whitespace before the listing
	tabsize=3, % How many space to a tab
}

%----------------------------------------------------------------------------------------
%	CUSTOM COMMANDS
%----------------------------------------------------------------------------------------

\newcommand{\reporttitle}[1]{\renewcommand{\reporttitle}{#1}}
\newcommand{\reportsubtitle}[1]{\renewcommand{\reportsubtitle}{#1}}
\newcommand{\reportauthors}[1]{\renewcommand{\reportauthors}{#1}}
\newcommand{\reportdate}[1]{\renewcommand{\reportdate}{#1}}

\newcommand{\leftheadercontent}[1]{\renewcommand{\leftheadercontent}{#1}}
\newcommand{\rightheadercontent}[1]{\renewcommand{\rightheadercontent}{#1}}
