
<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/haskell-starter-kit-blog/master/docs/logo.png" width="70%" />
</div>

----------------------------------------------------------------------

### Ready to start building *and deploying* applications with Haskell?

This Starter Kit will help you create your own blog powered by Haskell and
the power of <ins>pure, strongly-typed functional programming</ins>.

Does this tutorial look too long? Want your blog now? Clone this
repository and follow the instructions in the [Deployment](#deployment)
section. A few minutes later, you'll be able to create your first
post: 

> Learning Haskell
> 
> Hey! I'm learning Haskell. Stay tuned for updates on my experience.
>

Of course, you can write whatever you'd like.

### What's Here?

  * A cloneable, deployable **Haskell web application** (a blog)
  * A **tutorial** with some mini-essays about Haskell, a in-depth code
      review, and instructions for using and managing the project
  * A set of **exercises** of varying difficulty for learning or
      fine-tuning Haskell programming skills
  * **Deployment scripts** that can easily be re-used or extended for use
      in other Haskell projects

### Why?

Learning Haskell is difficult. First, you have to *understand*
a lot of new concepts like algebraic data-types, function currying,
typeclasses, and monads. Then, you have to learn *how to use* algebraic
data-types, currying, typeclasses, and monads to build non-trivial
applications that are both efficient and maintainable. The goal of
this tutorial isn't to help you understand these Haskell concepts 
individually, but to help you use them generally, in the process of building
real-world applications.

Much of this tutorial is language-agnostic. Great software will
have similar characteristics regardless of the implementation language
(or even despite it). Even so, each language has its own vision 
of how to develop software efficiently, and the best langauges are the
ones that support a consistent methodology. This tutorial showcases 
a little bit of Haskell's vision and methodology for software
development. We'll discuss some common problems in software and show
how Haskell's type system and expressiveness lead to some fascinating
solutions.

If you're new to Haskell, I hope that this project will help you to
appreciate Haskell's vision for software development and to understand
why Haskell programmers are so passionate about things like immutablity, 
types, and abstract mathematics. If you already know a little Haskell 
(or maybe a lot), then perhaps this project will be useful as a starting point 
for your next application. Whatever the case, there should be
something here for everyone. 

Haskell pushes the limits of software development. It challenges you
to try new ways of doing something that you already know how to do in
another language. But it's not just a new syntax, it's a new paradigm,
a new way of thinking about how to structure your code and solve
common problems in software engineering. If you put enough effort into
this new way of thinking, there's a good chance that the next time
your coding in another language you'll think of the Haskell solution
and wish you could just write a simple `where` clause, curry
a function, or create a typeclass.

------------------------------------------------------------------------------------

#### Contents

- [Goals](#goals)
- [Overview](#overview)
  - [Specification](#specification)
  - [Components](#components)
- [On Learning Haskell](#on-learning-haskell)
- [A Few Principles](#a-few-principles)
  - [Data Types](#a-few-principles)
  - [Functions](#a-few-principles)
  - [Composability](#a-few-principles)
- [Code Review](#code-review)
- [Managing the Project](#managing-the-project)
  - [Generate HTML Documentation](#generate-html-documentation)
- [Deployment](#deployment)
  - [Digital Ocean](#digital-ocean)
  - [Amazon ECS](#amazon-ecs)
  - [Kubernetes (GKE)](#kubernetes-gke)
- [Exercises](#exercises)
- [Footnotes](#footnotes)
- [FAQ](#faq)
  - [Which IDE Should I Use?](#which-ide-should-i-use)
  - [Where Can I Get Help?](#where-can-i-get-help)

## Goals

Before we jump in, let's take a look at the project's goals. The
source code and this tutorial are written with the goals in mind, so
it will be helpful to understand what we're trying to achieve. For
example, some of the source code could be written more concisely, but that
won't help illuminate Haskell's coding style, and playing code golf
won't help promote a language with a reputation for being hard to
understand. Likewise, we could have chosen simpler libraries, but we
wanted to showcase what Haskell can do differently and better than other
languages, as well as generate some useful discussion and insight on
software engineering in general.

#### Analyze Engineering Real World Applications with Haskell

Is pure functional programming better? When is it better? What are its
advantages when building real world applications? In this tutorial,
we'll discuss the trade-offs associated with engineering applications
in Haskell.

In particular, we'll pursue answers to the following questions:

   * How do you write code that is easy to understand?
   * How do you write code that is easy to extend?
   * How do you write code fast or how do write code that works the
     first or second time its run?
   * How do you write code with minimum errors?
   * How do you create a web application that is easy to deploy?
   * How do you manage a software project's common tasks and metadata?
   * How do you structure the code in a Haskell web application?

#### Serve as an Extensible Base for Haskell Web Applications

This example application is a very basic blog. We could have created
something more interesting, but the code would be longer and more
complex. The brevity and relative simplicity of the blog serves the
project's goals in two ways:

  1. **Generality** The purpose of this project is to teach about building Haskell
     applications in general, but not how to build a specific application. Too many
     details would distract from the more general lessons.
  2. **Extensibility** Because there are few assumptions in the application
     logic, the project can easily be used as a base for a more complicated web
     application.

#### Promote Haskell

Haskell is awesome, but misunderstood. If this project is successful,
then it should help to showcase Haskell's strengths and serve as an
entrypoint to building real Haskell applications.

## Overview

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

## On Learning Haskell

When I first started programming, what really captured me were the 
moments when I became fluent at telling the computer what I wanted to
do. At the time I loved to write and play piano, but once I could
code, programming became my primary means of expression. The immediate
feedback from creating something is incredibly addictive. My brain was
so stimulated I couldn't stop programming, exploring new domains, and
learning new frameworks. I created Flash games<sup>1</sup>, built websites, 
configured Linux until it didn't work, made a Wordpress blog, learned VIM, 
switched to ArchLinux and broke my OS again, studied cybersecurity, and
a lot more with a desire to be proficient in expressing myself through
digital mediums.

Eventually, I began to work on bigger and more ambitious projects. But
my pace slowed down. The think->code->reward loop got longer as it took me
more time to (1) figure out what I actually needed to do, (2) write
the code that did what I wanted, and (3) test that what I built was
correct. I learned that building software is actually very difficult
and oftentimes, incredibly frustrating. I started to do something
weird - I focused less on *what* I was building and more on *how* I was
building it. It was like stopping my car in the middle of the race, 
taking the engine out and trying to tweak it to make it go faster. 
I knew I wasn't going to catch up. But I did find a new hobby.

Programming languages are *human* languages -- they are designed to be
written and read by people. I didn't fully appreciate this until my
second year of college, when I learned about assembly languages and
compilers. A compiler translates one language into another, sometimes
with one or more intermediary languages. Programming langauges are
designed to be translated into machine langauge while being easy to
use by people. Of course, we have to consider who these people are?
Everyone is different, has different ways of thinking, different
goals, and different attitudes about building or managing software.
This is one reason why we end up with so many different programming
languages as well as code editors, tools, frameworks, and libraries.
The other reason is of course technical. Sometimes one way of doing
something is *better* than another way, one tool may be more
*efficient* than another, and we hope that these comparisons are
objective. So how do we know whether we're choosing a tool because it
meshes with our way of thinking or because it's the best?

Now that I had my new hobby -- my quest to find more efficient ways to
build complex software -- I started researching. I was writing a lot
of object-oriented code, so I spent a lot of time investigating design
patterns, inheritance, polymorphism, how to structure and organize
code in general, and how to use all of those concepts to build better
software. What I found was frustrating. I found a lot of "sage advice"
or mottos, quotes or phrases that are often repeated and sound very
good, but are very rarely substantiated. Sometimes I found them to
very well substantiated in specific contexts, but not in general.
I also found a lot of design patterns or "framework" patterns like
MVC. These also looked good initially, were intuitive and appealed to
an aesthetic sense of organization that I had, but I could never find
a good argument as to why they were better. I wasn't aware at the
time, but I was searching for some sort of underlying mathematical 
logic or structure, some set of consistent principles that I could 
use to reason about how to bulild good software, but I couldn't find it.

I realized that a lot of software development is done in a manner that 
appeals to our intuition. It creates structures and processes that are
easy to understand and manipulate. This is a very effective method -- 
it's simply optimizing programming for the natural way that
humans think. Unfortunately -- and I'll be the first to admit -- humans
aren't as smart as they like to believe, and what's intuitive for us is very
rarely optimal. In my search, I just wasn't motivated by intuitive solutions. 
They were fun, sure, and I enjoyed programming with them, but I always
felt like something was missing. I wanted to understand the mathematics 
behind software engineering, if there were any. I wanted some sense of
objectivity about how to build better software. When I discovered
Haskell and started learning it, I didn't know that my search was
over. I didn't believe that there were objective ways to understand
software development. In that respect, Haskell really changed the way
I thought about programming. It put a new ceiling to software
development in my eyes, and I was inspired to reach it. 

Now, it's important to say that it took me a long time to become
proficient with Haskell. I didn't come into Haskell with
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
ML-dialect, and therefore an implementation of the [Polymorphic Lambda
Calculus][poly-lambda-calculus]. It's not trivial to understand, but 
it's also not that difficult. It's a simple model of computation, one
derived in parallel to Turing's abstract machine. This
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

All this being said, if you spend any time reading Haskell discussions
online, you'll see that more than a few problems are far from solved.
You'll also see a smart, passionate community actively engaged in
solving these problems. That passion comes from a confidence in the
power of the langauge to think about these problems and encode elegant
solutions.

## A Few Principles

Before we review the source coee, let's review some high-level idioms in Haskell programming.
These will help you understand why the code is structured the way it
is as well as give you some insight on extending the blog or creating
a brand new Haskell project.

guiding principles on how to strcuture code. I think these should be
used in any language, but because Haskell doesn't give you any
strcuture the way oop does with classes, it's not always obvious how
to build a program. side effect handling complicates this too.
In OOP it is usually obvious, or at leats, you can
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

currying: Becomes nature to see, but is easy to forget at first. and take
advantage of. 

composing.

### Composition

If someone asks me why I think Haskell programs are more concise and
more maintainable than programs written in C, Java, Go, etc... I would
say that *Haskell enables compositional programming*.

## Managing the Project

A software project, even something as simple as this blog, may have
multiple components. Each compenent will have a set of actions that
can be performed on it such as compiling, running tests, generating
documentation, deploying, etc...

Onboarding a new developer to a project, its components, and all the
actions that the developer will need to do with those components
should be simple. It shouldn't take a full day of another developer's
time to walk someone though running and testing a project. That time
should be spent on walking through the code and discussing any design
decisions, goals, or active issues.

Futheremore, onboarding yourself after a few weeks away from the
project should be just as easy. If you can't remember what you ate for
dinner two weeks ago, it's perfectly fine that you also forgot which
exact command compiles the source code, runs the test suite, and spams
your team Slack channel with gifs of baby elephants. This
acceptable amnesia is remedied by a clear, concise interface,
preferably one available on the command line, clearly documented, and
easily extensible.

Our Haskell blog app comes with a command-line interface that
satifies these goals perfectly. It's called `dev.hs`. After you clone
the repository and follow the environment setup tutorial, you can run
`./dev.hs --help`:

```bash
Blog Development Commands

Usage: dev.hs COMMAND
  Run ./dev.hs --help for a list of commands.

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build the [executable, docker images, ...]
  run                      Run the [docker container, executable, ...].
  sh                       Open a shell in a running docker container.
  psql                     Open PSQL in a running docker container.
  init                     Initialize the database.
```

Often, command-line utilities are
implemented a shell language like Bash or an interpreted langauge like
Python. Both are great choices, but lucky for us, we can just
use Haskell! Stack allows us to write *Haskell scripts* that can be
run on the command line as executables. We get the best of both worlds
-- simple, self-contained scripts and the full power of Haskell.
Futheremore, because our script is written in Haskell, it has full
access to the types and functions in our application. This can be very
convenient.

### Getting Started

go from zero to running project

### Compilation

We can compile the web server using stack:

```bash
stack build
```

or using `dev.hs`:

```bash
./dev.hs build --exe
```

If you want to play around with the executable, you can use stack. For
example, `stack exec` will run the web server locally. See the
[stack documentation](stack-docs) for more information and commands.

### Docker

TODO

### Images

TODO
alpine image

### Containers

TODO

### API Documentation

TODO

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

### HTML (with Blaze)

TODO

## Deployment

TODO

### Digital Ocean

TODO

### Amazon ECS

TODO

### Kubernetes (GKE)

TODO


## Exercises

### Beginner :star: 

### Intermediate :star::star: 

### Advanced :star::star::star:


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
