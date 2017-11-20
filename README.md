
# Haskell Starter Kit: Blog

reasons for doing:
haskell is hard to get started with.
how to choose libraries.
present one option and explain trade-offs. trade offs are important..

"haskell is not production ready"
deploy your blog now! Docker images and kubernetes config. expand into 
web app. make your own. learn from decisions already made and expand
the architeture how you want. 

Pitch. coding in the real-world.

Pitch. coding in the real-world. point it show haskell, but most of
this is relevant irrespective of programming language.

Goal is to show how complex an application can be to do it beyond just
making it work. how to design it right, make it maintainable, keep the
code organized, deploy it, keep local and prodution code consistent,
test it, bring others onto the project easily. 

shows how to build a haskell application and deploy it. can clone it
and deploy your own blog right now. not just that, but this project
goes into details of engineering softwar ethat has all of the
desirable properties we might want and shows how Haskell can help us
to acheive that goal. goes into everything we need to worry about
beyond the code.

## The App

design considerations.

choose hard libraries in order to learn. hard for beginners.

but no matter what you choose, you always get some sort of convulted
DSL. if you choose one in Haskell, usually those decisions aren't
arbitrary but consistent as part of an underlying algebra. and they
tend to provide more safety and types, which give more ability to
reason about errors .

Common.hs

### Imports

Everything is function can get overwhelming keeping track of names. 
Here make everything explicit (which is always a good default policy).

### API (with Servant)

### Database (with Opaleye)

author wants it to be usable without understanding profunctors etc...

explain arrows / syntax.

### HTML (with Blaze)

A lot of web frameworks implement own templating languages. extra
languages add uncessary complexity. happens alot because main language
isn't powerful enough. Haskell is powerful enough to do almost
anything with the language as a DSL, almost always with many safety
guarantees.

We will just create the HTML in Haskell. It's not that hard!

## Building

### Dev Script

have to create docker network
docker network create --driver bridge blog


### Docker

Using alpine iamge because it's lightweight. Tell stack to build with
the alpine image. Include alpine image.

### Testing (Local Environment)

### Deployment (Production Environment)

Kubernetes is potentially overkill for a blog (but it shouldn't be).
GKE is expensive, but you can deploy based on the images manually for
cheap.


## TODO

image build issue have to install extra libs. don't remember why??



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



adding more simple data types is probably the best way to prevent
coupling in your programs. functions are simpler. everything is
explicit. data is only used in one way. unambiguous semantics that
don't depend on context of data. functions then are simpler. can
always compose smaller data or create bigger objects.

reason why oop code can be easily convoluted. encourages grouping
together functions and data with encapsulation which encourages
putting a lot into one class where should be split out. but since
inheritance doesn't usually work well in practice, this can quickly
become difficult. In haskell, it's not so hard to reason about. It's
easy to create data, easy to create functions, and with out explicit
importing policy, it's easy to bring them into scope. 
