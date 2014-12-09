# The definitive guide to ejabberd modules

Starting with writing ejabberd modules was a real pain for me. There hasn't been much documentation around and most of the articles/blogposts are outdated. 
This guide should cover creating ejabberd modules from scratch with none or little experience in writing erlang. Previous experience in software development is required though.

## Prerequisites

- ejabberd installed on your local machine
- erlang installed
- git installed

## Types of ejabberd modules

### Authentication
Authentication modules make it possible to attach an external API or DB etc. to ejabberd in order to authenticate a user. There [are various authentication modules](http://www.process-one.net/docs/ejabberd/guide_en.html#htoc25) already built-in and there's also the possibility to attach a script not written in erlang to authenticate (`auth_method: external`). This method is not recommended for high performance setups.

### Hook-based
Hook-based modules allow you to interact with ejabberd based on hooks. It is for example possible to react on all `message` stanzas with own handlers.

### Other
You can of course also add other modules to the ejabberd runtime which e.g. add a `HTTP` endpoint to ejabberd.

## Basic module setup
I created a module setup which you can clone and use as a base for your own modules, just run:

```
$ git clone https://github.com/mweibel/ejabberd-module-bootstrap.git
```

And start fiddling.

## Creating an authentication module

Clone the bootstrap into a directory

```
$ git clone https://github.com/mweibel/ejabberd-module-bootstrap.git example-auth && cd example-auth
```

Rename `src/mod_example*` to `src/ejabberd_auth_example*`.

```
$ mv src/mod_example.erl src/ejabberd_auth_example.erl
$ mv src/mod_example.app.src src/ejabberd_auth_example.app.src
```

Edit the `src/ejabberd_auth_example.app.src` to contain the following code:

```
{application, ejabberd_auth_example,
 [
  {vsn, git},
  {description, "Ejabberd authentication example"},
  {applications, [kernel, stdlib, ejabberd]},
  {registered, []},
  {env, []}
 ]}.
```

Now we're going to adapt the source code to do what we want in steps.
First let's create the very basic code we need to have.



