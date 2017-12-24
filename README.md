
<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/haskell-starter-kit-blog/master/docs/images/logo.png" width="70%" />
</div>

<br />

<div align="center">
  <h3><a href="www.haskellblogtutorial.com">www.haskellblogtutorial.com</a></h3>
</div>

----------------------------------------------------------------------

### Ready to start building *and deploying* Haskell web applications?

This tutorial will guide you, step by step, in creating a blog -- just like 
the one you're reading now -- using Haskell, a pure, strongly-typed functional 
programming language.

Does this tutorial look too long? Want your blog now? Clone this
repository and follow the instructions in the [I Want My Blog
Now!](#i-want-my-blog-now) section. In less than fifteen minutes,
you'll be able to publish your first post:

<img src="https://raw.githubusercontent.com/jeff-wise/haskell-starter-kit-blog/master/docs/images/firstpost.png" />

Of course, you can write whatever you'd like.

 
### Learn Functional Programming

From the smallest functions to the largest modules, we'll walk through 
the code and conventions required to build a clean and maintainable web 
application with Haskell.

The tutorial comes with a number of useful resources to help you learn:

 * A set of **exercises** of varying difficulty for learning or
     fine-tuning Haskell programming skills. The exercises are a way to 
     learn while making useful improvements to the blog.
 * Immediately deployable **Docker images** you can use to run your own blog 
    before you even look at the source code. This is a good way to jump in 
    the deep end with Haskell.
 * **Deployment scripts** that can easily be re-used or extended for use
     in other Haskell projects


## Why?

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
to try new ways of doing something you might already know how to do in
another language. But it's not just a different syntax, it's a different way of 
thinking. You'll see old problems in a new way, and the tried-and-true
solutions that you've relied upon before may start to appear a little
rusty. It's happened to all of us -- there we are, programming in another
language, as comfortable and confident as we've always been, when we
find ourselves thinking about the Haskell approach and wishing we could 
write a simple `where` clause, curry a function, or do just a little bit 
of type-level programming.


## Table of Contents

1. [Introduction](#introduction)
     1. [Goals](#goals)
2. [On Learning Haskell](#on-learning-haskell)
3. [Writing Functional Code](#writing-functional-code)
     1. [Data Types](#data-types)
     2. [Functions](#functions)
     3. [Composition](#composition)
4. [Software Structures: Design Patterns
   Revisited](#software-structures-design-patterns-revisited)
     1. [Monoids](#monoids)
     2. [Folds](#folds)
     3. [Functors](#functors)
     4. [Monads](#monads)
5. [The Application](#the-application)
6. [Managing the Application](#managing-the-application)
7. [Code Review: Program
   Organization](#code-review-program-organization)
8. [Code Review: Domain Specific
   Languages](#code-review-domain-specific-languages)
9. [Developing New Features](#developing-new-features)
10. [Deploying Your Blog](#deploying-your-blog)
     - [Digital Ocean](#digital-ocean)
     - [Amazon ECS](#amazon-ecs)
     - [Kubernetes (GKE)](#kubernetes-gke)
11. [I Want My Blog Now!](#i-want-my-blog-now)
12. [Exercises](#exercises)
     - [Easy](#easy)
     - [Medium](#medium)
     - [Difficult](#difficult)
13. [FAQ](#faq)
    - [Which IDE Should I Use?](#which-ide-should-i-use)
    - [Where Can I Get Help?](#where-can-i-get-help)

# Introduction

The code and this tutorial are written with a set of goals in mind.
The primary goal is to instruct. We want to demonstrate what it takes
to build a simple web application using Haskell. As we go through the
code and the tasks involved, there will be a recurring focus on understanding software 
engineering through the particular trade-offs that Haskell makes as a 
pure, strongly-typed functional programming language. We are not just
interested in *how* to build a web application, but *why* we should do
it with these particular tools and processes. Too often programming
tutorials focus on the construction and not the engineering. We will
place equal emphasis on both.

There are many, many ways to build a blog with Haskell. We've done so
in a way that facilitates teaching and discussion.
For example, some of the source code could be written more concisely, but that
won't help explain Haskell's coding style, and playing code golf
won't help promote a language with a reputation for being hard to
understand. Likewise, we could have used less complex libraries, but we
wanted to showcase what Haskell can do differently and better than other
languages, as well as generate some useful discussion and insight on
software engineering in general.

## Goals

#### Analyze Engineering Real World Applications with Haskell

Is pure functional programming better? When is it better? What are its
advantages when building real world applications? In this tutorial,
we'll discuss the trade-offs associated with engineering applications
in Haskell.

In particular, we'll pursue answers to the following questions:

   * How do you write code that is easy to understand?
   * How do you write code that is easy to extend?
   * How do you write code quickly? How do write code that works the
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

Haskell is awesome, but often misunderstood. If successful,
this project will showcase Haskell's strengths and serve as an
entrypoint to building real Haskell applications.

**[⬆ back to top](#table-of-contents)**
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
use to reason about how to bulild good software, but I couldn't find them.

I realized that a lot of software development is done in a manner that 
appeals to our intuition. It creates structures and processes that are
easy to understand and manipulate. This is a very effective method -- 
it's simply optimizing programming for the natural way that
humans think. Unfortunately -- and I'll be the first to admit -- humans
aren't as smart as they like to believe, and what's intuitive for us is very
rarely optimal. In my search, I just wasn't motivated by intuitive solutions. 
They were fun, sure, and I enjoyed programming with them, but I always
felt like something was missing. 

Programming languages are a means of communication between humans and
computers, or more accurately, humans and the universe. That is -- we
don't say "Hi!" or "I love you" to a computer, but rather, we tell it
patterns that it should use to manipulate electrical currents flowing
through the processor and memory. There are infinite languages one could write to
describe these patterns, but there are only so many that humans can
use. Can we categorize these possible languages? Can we make an
assessment of which ones are better than others? We can definitely
categorize them.

It's hard to assess programming languages. One reason is that they are
often made with different goals in mind, and it's no use comparing
apples and oranges. They're both good, but one's sweet and one's
citrus. A more interesting dilemma in comparing languages is also true
with apples and oranges -- they're subjective. It depends on your
taste, or maybe even your mood. While programming languages are built
with the objective to give instructions to a computer, how exactly
those intructions are expressed can vary as much as people themselves
vary. There's a business language, a lazy language, a try-hard language,
a sassy language, a he's-just-going-through-a-phase language, etc... 
Languages and other development tools are made by people, for people. 
This is one reason why we end up with so many different 
languages as well as code editors, tools, frameworks, and libraries.
The other reason is of course technical. Sometimes one way of doing
something *is better* than another way, and we hope that there are
objective means of comparing options. How can we assess these options
objectively?

I wanted to understand the mathematics 
behind software engineering, if there were any. I wanted some sense of
objectivity about how to build better software, it there was any to be
found. When I discovered Haskell and started learning it, I didn't know 
that my search was over (at least for awhile). I didn't believe that 
there were objective ways to understand software development. In that 
respect, Haskell really changed the way I thought about programming. 
It put a new ceiling to software development in my eyes, a new mountain 
to climb, and I was surprised that I was so excited to work hard and 
reach that new persepctive.

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

**[⬆ back to top](#table-of-contents)**

# Writing Functional Code

Before we review the source code, let's review some principles of
Haskell programming. Learning Haskell can be difficult because there 
are so many new concepts and it's hard to figure out to use them when 
you finally sit down to start programming. Fortunately,
while there are a lot of concepts, we can group
them into three simple categories: data types, functions, and
composition. We'll show how understanding these three simple
concepts in Haskell is enough to start writing great software.

While these principles are universal, they're often difficult to apply 
in other programming languages. This is either because other
languages lack certain features such as sum types or function currying
or because those languages encourage a different way of thinking about
or modeling software.

## Data Types

TODO

## Functions

TODO

## Composition

If someone asked me why I think Haskell programs are more concise and
more maintainable than programs written in C, Java, Go, etc... I would
say that *Haskell enables compositional programming*.

TODO

**[⬆ back to top](#table-of-contents)**


# Software Structures: Design Patterns Revisited

**[⬆ back to top](#table-of-contents)**


# The Application

## Specification

TODO

## Components

While the specification is simple, creating a robust implementation
will require more than a few libraries and tools. 

### Libraries
 
 * [Servant][servant-docs] (Web Framework)
 * [Opalaye][opaleye-stackage] (PostgreSQL ORM)
 * [Aeson][aeson-stackage] (JSON / YAML parsing)
 * [BlazeHtml][blaze-stackage] (HTML combinators)
 * [Optparse-Applicative][optparse-stackage] (Command-line parsing) 

### Important Tools

 * [GHC](https://www.haskell.org/ghc/)
 * [Stack][stack-docs]
 * [Docker][docker-docs]


**[⬆ back to top](#table-of-contents)**


# Managing the Application

A software project, even a humble blog, might have a myriad of
components. Each compenent will have a set of actions that
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
your team Slack channel with baby elephant gifs. This
acceptable amnesia is remedied by a clear, concise interface,
preferably one available on the command line, clearly documented, and
easily extensible.

Our Haskell blog app comes with a command-line utility that
satifies these goals perfectly. It's called `dev.hs`. After you clone
the repository and follow the environment setup tutorial, you can run
`./dev.hs --help`:

<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/haskell-starter-kit-blog/master/docs/images/devhelp.png" />
</div>


Often, command-line utilities like `dev.hs` are 
implemented in a shell language like Bash or an interpreted langauge like
Python. Both are great choices, but actually, we can just use Haskell! 
Stack allows us to write *Haskell scripts* that can be
run on the command line as executables. We get the best of both worlds
-- simple, self-contained scripts and the full power of Haskell.
Futhermore, because our script is written in Haskell, it has full
access to the types and functions in our application. We'll see later
how this is really convenient.

## Getting Started

go from zero to running project

## Compilation

We can compile the web server using stack:

```bash
stack build
```

or with `dev.hs`:

```bash
./dev.hs build --exe
```

If you want to play around with the executable, you can use stack. For
example, `stack exec` will run the web server locally. See the
[stack documentation](stack-docs) for more information and commands.

## Docker

TODO

## Images

TODO

## Containers

TODO

## API Documentation

TODO

**[⬆ back to top](#table-of-contents)**

# Code Review: Program Organization

## Imports

TODO 

custom prelude
explicit imports for learning

# Code Review: Domain Specific Languages

## API (with Servant)

TODO

## Database (with Opaleye)

TODO 

## HTML (with Blaze)

TODO

**[⬆ back to top](#table-of-contents)**


# Developing New Features

**[⬆ back to top](#table-of-contents)**


# Deploying Your Blog

*A blog isn't really a blog if no one can read it.*

Writing good code is something to be proud of. It takes time. There
are all the hours searching for silly bugs, the hours re-writing
flawed logic, the minutes spent on one feature and the days spent on
another.

It's the feeling when you get a puppy and you realize that it's going
to be a lot of work after all. You need to train it, feed it, walk it,
play with it, and love it, each and every day, even when you're tired,
and especially when it's in need. 

I wouldn't try too hard to build an analogy around puppies and web
applications, but it is really important to understand that maintaining
active software is going to be a lot of work. It might feel
unexpected, and it might feel undeserved, but it's necessary. What do
we need to do to take care of our web application?

  * Create a process for updating the application.
  * Restart it if it dies (or replicate it so it doesn't).
  * Keep backups of the data.
  * Be vigilant of security vulnerabilities.
  * Upgrade the hardware if it needs more memory or processing power.
  * Optimize the code if the hardware gets expensive.
  * Give it a treat if it does something good. (OK, maybe this is
     just for puppies)

Unfortunately, no matter how much work you put into taking care of
your blog or other web application, it will, from time to time, nip 
you on the ankle or knock you on your back, leaving a bruise. And
living as we are in the age before strong AI, it won't love you back. 
That's ok -- you have your own motivations. If you're not sure what
they are, take a break and have a look around. Once you have them, 
keep them close. Deploy your app, take care of it. 

The fun has just begun.

#### Goals

This section is less opinionated that other sections. That's because
how you deploy your application will vary much more than how you write
it. There are dozens of products and platforms available that you
might want to use. You may want to pay for Docker Cloud, switch to
particular cloud host, pay for a managed database, or even use
a container orchestration service if your application begins to scale.
Some of these products or free, some are cheap, and some are really
expensive. 

There are varying degrees to which you can automate
deployment from completely manual (but well-documented) processes to
a completely automated continuous development cycle. 

This is the graduation. You're on your own now. You will have to evaluate 
your needs and find the best tools for the job, spending as much or as
little money was you want.

These deployment instructions are meant to be as simple and as
extensible as possible. Most steps are manual. It will be up to you to
automate them in a way that's useful for you.

## Digital Ocean

### P1. Create a Docker Droplet

  1. [Create a droplet that runs Docker](do-docker). A droplet is 
     Digital Ocean's name for a virtual server on its platform.

### P2. Create a Docker Cloud Repository

Docker Cloud is Docker's premium service for managing Docker
images and containers. In this tutorial we only use it as a 
means to share our images between our computer and the server. 
Generally, it's useful to have the images persisted somewhere where
they may be easily accessed. You may want to use old images that were
lost, access them from a different computer, or allow other team
members to use them.

Docker Cloud organizes images into repositories
It's free to use for one private repository as well as unlimited public 
repositories. If you don't want to use Docker Cloud, and aren't
concerned about any of the benefits we just listed, feel free to copy
the images manually:

```bash
docker save <image> | bzip2 | pv | ssh user@host 'bunzip2 | docker load'
```

It's good to remember that you almost never *need* to use an official
service to acheive a goal in programming. It's also good to remember
that your time is often more valuable than you think, and a few
dollars can go a long way.

To push our images to the server, you'll need a Docker Cloud account
and a repository. If you already have both, you can continue on to 
[Step 1](#tag-your-docker-images). Otherwise, it will only take you
a few steps to create an account and repository:

  1. If you don't already have a Docker Cloud account, create one
     [here](docker-cloud). 
  2. Once you are logged in to Docker Cloud, you can follow the
     [instructions for creating a repository](docker-cloud-repo).

Now you should be ready to deploy your blog. Let's get started.

### 1. Tag Your Docker Images 

Before we do anything official with our images, we should tag them
with a version number. This will prevent us from confusing images that
belong to different versions of our application. 

Just as with version control, it's not very common to go back to
a previous revision, but when you need to, you're glad that it's
there. Likewise, there are benefits to archiving previous versions of
our application. For example, if something were to go wrong with our web
server, we could always stop the running container and deploy
a container for the image of the version that was last functioning
correctly. We don't have to checkout a previous commit in version
control, recompile anything, or even copy any files. It can all be
done with Docker.

  1. Use the `docker tag` command to tag your images. Of course, make
     sure that you tag the images you want to deploy and update the
     version numbers accordingly. For example:

     `docker tag b0b6fdfe0115 haskell-blog-tutorial/db:1.0`

     `docker tag f77652c4b03e haskell-blog-tutorial/web:1.0`

### 2. Push the Images to Docker Hub

Following the instructions to [push images to Docker
Hub](docker-cloud-push). If it tells you to tag the image, you can
skip that step, since we just did that.

Verify that your images have been uploaded, and then continue to the
next step, where we'll log on to the server. 

### 3. Pull the Images to Your Droplet

  1. **Logon to your droplet**. If you're not sure how, follow these
     [instructions](do-logon-ssh).

  2. **Pull the images from your respository**. You will need the
     namespace (your account name), the name of the repository, and
     the tag. Just check your local images or the repository if you've
     forgotten.

     `docker pull jeffscottwise/haskell-blog-tutorial:db-1.0`

     `docker pull jeffscottwise/haskell-blog-tutorial:web-1.0`

### 4. Run the Application

Once again, we'll take a more manual approach to running the
application so you can make your own adjustments and automations
according to your specific needs.

We'll run the containers for the database and webserver using the
`docker run` command. Since port 80 should be open by default on your
droplet, you'll be able to access your application through a web
browser as soon as the containers are active.

  1. **Create a Network**. In order for the web server container to
     connect to the database container, it will need to resolve the
     database container's address base on the name that is hard-coded
     into the server. Docker Compose does this automatically, but it's
     very simple to do manually.

     If you already have a network for the containers, then you can
     skip this step. If you're not sure, run `docker network ls` to
     check. The `DRIVER` needs to be `bridge`. 

     If you need to create a network, do so now:

     ```bash
     docker network create blog
     ```

  2. **Run the Application Containers**. Start the database container,
     and then start the web server. As before, you will need to
     substitute your own image names, unless you are using the
     official haskell-blog-tutorial images.

     ```bash
     docker run -d --name blog-db --net=blog jeffscottwise/haskell-blog-tutorial:db-0.7.0 
     ```
     ```bash
     docker run -d --name blog-web --net=blog -p 80:80 jeffscottwise/haskell-blog-tutorial:web-0.7.0 
     ```

  3. **Make sure it works**. Open a web browser and enter the IP
     address of the docker droplet. You should see the blog homepage.

### 5. Configure DNS

Configuring your domain name to point to the Digital Ocean droplet is
straightfoward.

  1. Follow the steps in [this tutorial](do-dns).


## Amazon ECS

TODO

## Kubernetes (Google Container Engine)

TODO

**[⬆ back to top](#table-of-contents)**


# I Want My Blog Now!

**[⬆ back to top](#table-of-contents)**


# Exercises

## Easy :star: 

## Medium :star::star: 

## Difficult :star::star::star:

**[⬆ back to top](#table-of-contents)**


# FAQ

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
[do-docker]: https://www.digitalocean.com/products/one-click-apps/docker/
[docker-cloud]: https://cloud.docker.com/
[docker-cloud-repo]: https://docs.docker.com/docker-cloud/builds/repos/
[docker-cloud-push]: https://docs.docker.com/docker-cloud/builds/push-images/
[do-logon-ssh]: https://www.digitalocean.com/community/tutorials/how-to-connect-to-your-droplet-with-ssh
[do-dns]: https://www.digitalocean.com/community/tutorials/how-to-set-up-a-host-name-with-digitalocean
