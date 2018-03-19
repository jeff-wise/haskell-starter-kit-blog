
<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/haskell-starter-kit-blog/master/docs/images/logo.png" width="70%" />
</div>

<br />

<div align="center">
  <h3><a href="http://www.haskellblogtutorial.com">www.haskellblogtutorial.com</a></h3>
</div>

----------------------------------------------------------------------

## Ready to start building *and deploying* Haskell web applications?

This tutorial will guide you, step by step, in creating a blog -- just like 
the one you're reading now -- using Haskell, a pure, strongly-typed functional 
programming language.

Does this tutorial look too long? Want your blog now? Clone this
repository and follow the instructions in the [I Want My Blog
Now!](#i-want-my-blog-now) section. In less than fifteen minutes,
you'll be able to publish your first post.

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
2. [The Application](#the-application)
3. [Managing the Application](#managing-the-application)
4. [Code Review: Program
   Organization](#code-review-program-organization)
5. [Code Review: Domain Specific
   Languages](#code-review-domain-specific-languages)
6. [Developing New Features](#developing-new-features)
7. [Deploying Your Blog](#deploying-your-blog)
     - [Digital Ocean](#digital-ocean)
     - [Amazon ECS](#amazon-ecs)
     - [Kubernetes (GKE)](#kubernetes-gke)
8. [I Want My Blog Now!](#i-want-my-blog-now)
9. [Exercises](#exercises)
     - [Easy](#easy)
     - [Medium](#medium)
     - [Difficult](#difficult)
10. [FAQ](#faq)
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
