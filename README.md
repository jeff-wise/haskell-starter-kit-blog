

# Haskell Starter Kit: Blog

Ready to start building *and deploying* applications with Haskell?
This is your Starter Kit for building your own blog using Haskell and
the power of pure, strongly-typed functional programming.

Does this tutorial look too long? Want your blog now? Clone this
repository and follow the instructions in the [Deployment][#deployment] 
section. A few minutes later, you'll be able to create your first
post: 

> Learning Haskell
> 
> Hey! I'm learning Haskell. Stay tuned for updates on my experience.
>

Of course, you can write whatever you'd like.

Learning Haskell can be very difficult, especially when you only have
experience with imperative languages. First, you have to **understand**
a lot of new concepts like algebraic data-types, function currying,
typeclasses, and monads. Then, you have to learn **how to use** algebraic
data-types, currying, typeclasses, and monads to build non-trivial
applications that are both efficient and maintainable. The goal of
this tutorial is not to help you understand specific Haskell concepts,
but to teach you how you can take the concepts you have learned and
use them to build high-quality software.

Many of the lessons here are language-agnostic. Great software will
have similar characteristics, regardless of which language is used to
write it. Of course, each language has its own vision for the best way
to write great software. This project is intended to showcase a little
bit of Haskell's vision. It's only 1000 lines of code, but there's
a lot to learn if we look closely and examine the trade-offs
associated with each decision, big and small. In this process, we can
gain some insight into why learning all those new concepts is worth
the effort.

Perhaps you'll even come to appreciate why Haskell programmers are so
passionate about a language that's never gained widespread traction.

------------------------------------------------------------------------------------

#### Contents

- [Overview](#overview)
  - [Goals](#goals)
  - [Specification](#specification)
  - [Components](#components)
    - [Important Libraries](#important-libraries)
    - [Important Tools](#important-tools)
- [Learning Haskell](#learning-haskell)
- [A Few Principles](#a-few-principles)
  - [Data Types](#a-few-principles)
  - [Functions](#a-few-principles)
  - [Composability](#a-few-principles)
- [Code Review](#code-review)
- [Managing the Project](#managing-the-project)
  - [Generate HTML Documentation](#generate-html-documentation)
- [Deployment](#deployment)
  - [Digital Ocean](#digital-ocean)
  - [Kubernetes (GKE)](#kubernetes-gke)
- [Footnotes](#footnotes)
- [FAQ](#faq)
  - [Which IDE Should I Use?](#which-ide-should-i-use)
  - [Where Can I Get Help?](#where-can-i-get-help)

## Overview

#### Themes

 * Haskell's simplicity makes it complicated.

### Goals

The application is designed to be very simple for two reasons:

  1. The purpose of this project is to teach about building Haskell
     applications in general, but not how to build a specific application. Too many
     details would distract from the more general lessons.
  2. Because it's simple and makes few assumptions in the application
     logic, it can easily be used as a base for a more complicated web
     application.


Goal is to show how complex an application can be to do it beyond just
making it work. how to design it right, make it maintainable, keep the
code organized, deploy it, keep local and prodution code consistent,
test it, bring others onto the project easily. 


engineering. looking at trade-offs. plenty in design of web server and
database application, but also plenty with Haskell.

Want to build and change software with minimum work and maximum
accuracy. Work is time, size of code, readability, how easy to
mentally process code that is new or unfamiliar (forgotten), how easy
to find people proficient in language and tools, degree of coupling, 
function g - time to take code from version A to version B. 

  tradeoffs:
    * cpu vs memory
    * cost to write code (now) vs cost to change code (future)
    * more?

reason why oop code can be easily convoluted. encourages grouping
together functions and data with encapsulation which encourages
putting a lot into one class where should be split out. but since
inheritance doesn't usually work well in practice, this can quickly
become difficult. In haskell, it's not so hard to reason about. It's
easy to create data, easy to create functions, and with out explicit
importing policy, it's easy to bring them into scope. 


scope: 

simple enought for beginning to not feel overwhelmed. can fit entire
codebase in head. also, simple enough to extend to some other type of
web application because existing code doesn't make too many
assumptions about future code, can easy extend to be any other type of
web app.


### Specification

### Components

While the specification is simple, creating a robust implementation
will require more than a few libraries and tools. 

TODO why did we choose these? what can they teach us? 

#### Important Libraries
 
 * [Servant][servant-docs] (Web Framework)
 * [Opalaye][opaleye-stackage] (PostgreSQL ORM)
 * [Aeson][aeson-stackage] (JSON / YAML parsing)
 * [BlazeHtml][blaze-stackage] (HTML combinators)
 * [Optparse-Applicative][optparse-stackage] (Command-line parsing) 

#### Important Tools

 * [Stack][stack-docs] (with GHC)
 * [Docker][docker-docs]
 * [Kubernetes][kubernetes-docs]

## Learning Haskell

When I first started programming, what really captured me were those
moments where I became fluent at telling the computer what I wanted to
do and watching as it did just that, knowing that I now had the power
to bring to life whichever ideas popped into my head. The immediate
feedback you get from creating something is incredibly addictive. My brain was
so stimulated I couldn't stop programming, exploring new domains, and
learning new frameworks. I created Flash games<sup>1</sup>, built websites, configured Linux
until it didn't work, made a Wordpress blog, studied hacking, and
everything else with the desire to be proficient at creating and
manipulating software. 

Eventually, I began to work on bigger and more ambitious projects. But
I slowed down. The think-code-reward loop got longer as it took me
more time to (1) figure out what I actually needed to do, (2) write
the code that did what I wanted, and (3) test that what I built was
correct. I took one step up, poked my head above the clouds, and
realized there was another, much longer staircase. This happens a lot
in software development where adding just one feature multiplies the
complexity because that feature interacts with every pre-existing
feature. There are many proven ways to address this. One way is simply
having a lot of experience and developing a great deal of intuition
with one programming language and framework. Often, the tools don't
matter as much as the person using them (and how they use them). 

My mind works differently though. Instead of focusing with one set of
tools, I wanted to know which tools were the best and why. Like I just
said, there aren't really a set of "best" tools to use, but I didn't
know how to evaluate what the best way to build a particular program
was, and that bothered me. I was writing a lot of object-oriented
code, and I had a lot of questions that I really couldn't find
satisfying answers to such as (1) Why should I organize my code with
the MVC architeture?, (2) Why are there so many design patterns and
shouldn't they seem less arbitrary since programming is mathematical
in nature?, (3) Where do I put this method?, (4) What's the best way
to write re-usable code?, (5) 

Programming languages are a way for humans to translate human ideas 
into instructions a computer can understand, and many of these
solutions appealed to the human side of this translational barrier.
I wanted to understand the mathematics behind software engineering and
answer the question: how can I build better software not with
funny-sounding design patterns or architectures but with mathematical 
structures and theorems. When I started learning Haskell, I didn't
know that it was going to help me answer that question. I didn't think
that question had a good answer. In that respect, Haskell really
changed the way I thought about software engineering.

Now, it's important to say that it took me a long time to become
a proficient Haskell programmer. I didn't come into Haskell with
a strong background in abstract or discrete mathematics. In fact,
I didn't truly appreciate math at all before learning Haskell. Haskell
introduced me into mathematical thinking before I had ever written any
proofs, and it did so in a way that was gradual and intuitive, because
it was through the process of programming. While many that learn
Haskell are attracted to it from their perspectives gained as
mathematicians, many are like me, coming at it from the other
direction. Both paths are valid routes to Haskell, and it's Haskell's
place at this intersection that it makes it both scholarly and
pragmatic, a language that can implement ideas from research papers
and then immmediately put them into practice building high-quality 
software.

At it's core, there's nothing experimental about Haskell. It's an
implementation of the [Polymorphic Lambda
Calculus][poly-lambda-calculus] which itself isn't too hard 
to understand (despite the wordy name). This
simplicity shines through in many applications, but it also stands 
out when the code grows in complexity. You may be doing fine with
Haskell when it comes to writing simple, pure functions, but as soon as
you try to connect your code to the real-world, you may quickly get
overwhelmed. In a way, languages such as Java actually provide useful
abstractions that make writing interactive or graphical applications
easier, in the sense that they abstract over the underlying
mathematical model by providing mutability and free use of I/O in the
code. Haskell chose to keep its model pure, but the trade-off is that
Haskell was forced to model state mutation and IO using only pure
functions, and this is largely where Haskell programming becomes
difficult to learn. I'm talking about Monads.

It's definitely seems like a problem when ubiqutious concepts in
Haskell like Monads are notoriously difficult to understand. It
is true that most Haskell beginners run into trouble with Monads, and
well, they should. The interface itself isn't complicated, but the way
that they are used can be fairly complex. When using monad transformers, 
you may likely be applying functions with types that stretch across
the screen, and you may be doing operations over values that involve
manipulating those complex types either by adding more types, removing
types, or modifying types inside the types. When you start writing
Haskell code, it's a lot to keep track of in your head. But with
practice you will develop the mental model and visualization skills
that you need to easily write code with complex types. And it's worth
it. 

Design patterns in Haskell are mostly just pre-established
mathematical structures and algorithms. The library ecosystem is full
of implementations of really audacious ideas that work fantasically
and would be impossible or impractical to implement in many other
languages. This project demonstrates some examples with
[Aeson][aeson-stackage], [Servant][servant-docs], and
[Opaleye][opaleye-stackage].

## A Few Principles

Before we review the source coee, let's review some high-level idioms in Haskell programming.
These will help you understand why the code is structured the way it
is as well as give you some insight on extending the blog or creating
a brand new Haskell project.

guiding principles on how to strcuture code. I think these should be
used in any language, but because Haskell doesn't give you any
strcuture the way oop does with classes, it's not always obvious how
to build a program. In OOP it is usually obvious, or at leats, you can
put related things in a class and however you do so won't matter too
much for the first few thousand lines of code. 

goal is composability. see how to do that with data types and
functions. that's all haskell has. why it can be hard. why simplicity
can make it complicated.

### Data Types

adding more simple data types is probably the best way to prevent
coupling in your programs. functions are simpler. everything is
explicit. data is only used in one way. unambiguous semantics that
don't depend on context of data. functions then are simpler. can
always compose smaller data or create bigger objects.

article new, article, etc. more type-safe. they *are* different
things. easier to read. change behavior of one, doesn't affect others.
This works in OOP too, but because classes are a unit of encapsulation
and methods are tied to data, it's not as elegant. this is a reason
why OOP code can get messy. 

encourages being explicit. can just make data type for anything. code
is more readible and extensible. Modeling in Haskell is easy. really
helps write good code.

### Functions

Becomes nature to see, but is easy to forget at first. and take
advantage of. 


### Composability

 
## Code Review

choose hard libraries in order to learn. hard for beginners.

but no matter what you choose, you always get some sort of convulted
DSL. if you choose one in Haskell, usually those decisions aren't
arbitrary but consistent as part of an underlying algebra. and they
tend to provide more safety and types, which give more ability to
reason about errors .

Common.hs

### Imports

TODO 

custom prelude
explicit imports for learning

### API (with Servant)

### Database (with Opaleye)

TODO 

usable without understanding complex types
explain arrows / syntax.
type-saftey
composability

### HTML (with Blaze)

TODO

advantage over templating languages
composable HTML

## Managing the Project

A project may consist of multiple components and
each component may have multiple associated actions such as compiling, 
building executables, running tests, deploying, generating documentation,
and so on. 

Onboarding a new developer to a project should be simple. Futheremore, 
onboarding yourselfafter you haven't worked on the project in a month should also be
simple. It's good practice to treat your project as its own entity and
provide a comprehensive, easy-to-use interface for the common project 
workflows.

Command-line utiltiies are easy to implement and very flexible. This
project comes with a script called `dev.hs` which serves as the
primary interface for working with our project. Perhaps we haven't
written a line of code on the project in a few weeks or we are showing
a co-worked how to get started, we can start with `./dev.hs --help`.

[image]

Bash or an interpreted language such as Python are commonly used to
implement simple programs without a project structure like our `dev.hs`. 
Either would be a good choice, but we can do better and just use
Haskell. Stack allows us to write Haskell scripts that can be run as
executables. We get the best of both worlds -- code that can be
written and run in one file and all of the power of Haskell's types
and ecosystem. But the true advantage is that we can re-use the types 
and functions defined in our application inside the script.

### Compilation

TODO

### Docker

TODO

### Images

TODO
alpine image

### Containers

TODO

### API Documentation

TODO

## Deployment

### Kubernetes (GKE)

### Digital Ocean

Kubernetes is potentially overkill for a blog (but it shouldn't be).
GKE is expensive, but you can deploy based on the images manually for
cheap.

## The Hacks

stack script yaml?
no hasclient multipart support

## Footnotes

 1. This section is in the context of the mid-2000s when I started
    programming. Flash games were popular, Ruby on Rails and Django were
    new and exciting, and functional programming had much less momentum
    than it does today.


## FAQ

#### Which IDE Should I Use?

list of some good options. won't be update-to-date

#### Where Can I Get Help?

list of options



[poly-lambda-calculus]: https://en.wikipedia.org/wiki/System_F
[aeson-stackage]: https://www.stackage.org/haddock/lts-9.14/aeson-1.1.2.0/Data-Aeson.html
[servant-docs]: http://haskell-servant.readthedocs.io/en/stable/
[opaleye-stackage]: https://www.stackage.org/package/opaleye
[blaze-stackage]: https://www.stackage.org/package/blaze-html
[optparse-stackage]: https://www.stackage.org/package/optparse-applicative
[stack-docs]: https://docs.haskellstack.org/en/v1.4.0/README/
[docker-docs]: https://docs.docker.com/
[kubernetes-docs]: https://kubernetes.io/docs/home/
