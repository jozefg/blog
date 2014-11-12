---
title: Teaching Python with a Raspberry Pi
tags: teaching
---

This last week I've been volunteering at a summer camp. This camp is
aimed at kids ages 8 to 12 and teaches the basics of Python!

I wanted to write down some of my thoughts and experiences on the
whole process.

## The Curriculum

The curriculum for the camp was based around 3 key components

 - Python
 - Raspberry Pis
 - Minecraft

The camp was spread over 4 days, each 3 hours. Each day introduced
more of Python with more sophisticated programs. Each program actually
interacted with minecraft, building structures, modifying worlds, and
doing cool visible things. We'll talk later about how this was
possible.

Going into the camp, the expected schedule was something like

 1. Introduce the Pi, show how to run Python scripts from terminal
 2. Introduce the basics of Python, mostly variables and conditionals
 3. Apply these basics with the minecraft API
 4. Introduce loops, apply this with a few more advanced programs

In hindsight, this curriculum was a tad bit unrealistic, but what
curriculum isn't.

## The Staff

This was the first time the camp was run, so the staff was a little
inexperienced.

I was the only person familiar with programming but had never taught
young children before, and the two payed staff members were used to
teaching basic science camps but had never taught anything CS-ish.
This meant that a lot of this was a learning experience for us as much
as the kids.

## The Children

The camp was over-capacity with 14 children. None of them has ever
programmed before per-se. But two had done some basic HTML layout and
one 10 year old was quite familiar with unix after 2 years of running
various Linux distributions (I was impressed).

The unfortunate fact was that since the camp was marketed as teaching
with Minecraft, a lot of the kids just showed up to play
Minecraft. This was anticipated but still a little saddening.

## Day 1

On day 1, we get everyone set up with their own Pi, we also included

 - A cheap monitor
 - A very cheap mouse
 - A keyboard

Getting this all set up for 14 kids was a lot smoother than
anticipated. The only hitch was the SD cards we'd purchased were a lot
cheaper than anticipated so we burned through maybe 5 cards that we
just couldn't get a Pi to boot with.

We got everyone successfully to a desktop in about 30 minutes.

The Pis were running a custom operating system called Raspbian. This
OS is very verbose during boot time and shows the entire log from
booting up rather than just displaying an innocent little loading
graphic.

Quite a few of the kids were curious about what was going on so we
explained how little about how OS's work. It was pretty awesome to see
kids being interested in what steps a kernel went through.

Sadly I'm not a super knowledgeable person when it comes to OS's. In light
of this I've ordered a book or two on the subject, something that's
been on my todo list for a while now. I should be better prepared
for questions next time.

Now once we got everyone up and running we had people order 2
programs, LXTerminal and Minecraft. This is when we had some fun
trying to explain what exactly a terminal is.

I eventually started simply saying

> LXTerminal is a program that let's you run other programs. It's like
> a text interface so that you can do what you normally do by clicking
> with typing.
>
> Almost all Unix computers, like OS X and Raspbian, have the same way
> of entering stuff into terminals.

From here we had everyone run `cd play`. Luckily a group of volunteer
engineers had sat down and written a bunch of programs to do various
things in Minecraft. The first one everyone started with just built a
grid of stone blocks.

We then started explaining how to run things with the `python`
program. This turned out to be a bit more of a struggle than
anticipated since typing and spelling are more difficult than
anticipated.

We had a lot of people doing things like

     $ pyton grid.py
     $ pythongrid.py
     $ grid.py
     $ python grid.py # Finally

I really wish we had an overhead project to show everyone written
examples on a teacher machine. This was a big problem as time went on,
simply saying things out loud is not a sufficient method for
communicating about programs.

Now, once this ran there was a satisfying "Whoooaaaa" when everyone
saw that this command had modified the game right before their eyes!

Some people quickly started trying to use this to speed up their
building by automatically creating walls for themselves rather than
doing it by hand. This was exactly the response we were looking for
and it was clear this was starting to spark some interest in
programming.

Finally we had everyone open up IDLE. We used IDLE for all our editing
purposes for exactly two reasons

 1. It's dead simple to use
 2. It's preinstalled

Everyone opened up `grid.py` and had a look at the source code. The
code for `grid.py` was roughly

``` python
    import minecraft
    import block

    mc = minecraft.Minecraft.create() # Our connection to Minecraft

    def buildWall(size, type = block.STONE):
        pos = mc.player.getPos()
        pos.x += 3

        for x in range(size):
            for y in range(size):
                # Set block at these coordinates to type
                mc.setBlock(pos.x + x, pos.y + y, pos.z, type)

    if __name__ = "__main__":
        buildWall(5)
```

We get a pretty nice high level API to minecraft, and the code is
quite simple. Keep in mind, we have taught exactly 0 python at this
point.

Next we explained that we could change `buildWall(5)` to
`buildWall(6)` and our program would make a bigger wall! Again an
overhead was sorely missed at this point since it was very hard to
explain exactly where we were talking about, even in such small code.

Most people than started modifying the code trying to build as big a
wall as possible. This was also the point at which our first syntax
errors started up.

Since I was the only person in the room who
understood what they meant there was a fair bit of running around. I
have to give a lot of credit to the two staff members who essentially
learned the basics of Python syntax by me yelling it to them across
the room!

`grid.py` also included some code to generate a grid with different
blocks. This was another huge success since kids could try to spell
different words in their grid of blocks. I've omitted it from the
above snippet since frankly I don't remember it.

This took up most of the first day, since everyone also got a 30
minute snack breaks (don't you miss snack breaks?).

## Day 2

The next day we were actually aiming to teach some programming! This
had a script written already by the engineers who'd written the
code we'd used yesterday, but upon consulting the script I found

 1. Teach variables
 2. Explain what a value is
 3. Explain if's
 4. Questions

Uh oh. So I ended up writing a few notes down the night before, we
didn't have access to any sort of projector so a lot of my
explanations consisted of scribbling on a giant (2' by 3') post it
note.

This had distinctly mixed results. As I'd expected most kids couldn't
pick up the fundamentals of programming in an hour! This was OK though
since the rest of the day was spent messing around with a simple
program

``` python
    # chat.py
    import minecraft

    mc = minecraft.Minecraft.create()

    message = "Hello Chat"

    mc.putToChat(message)
```

And we used this to introduce the fundamentals of Python. For kids
that were progressing faster, we challenged them to write more
complicated programs like

``` python
    import minecraft

    mc = minecraft.Minecraft.create()

    message = ""

    if 1 + 1 < 2:
        message = "Hello"
    else:
        message = "Goodbye"

    mc.putToChat(message)
```

Not surprisingly, this was really hard to grasp for our kids.
This was when the class started to fragment a bit, some kids were
getting this and really doing awesome while some were having a harder
time with all the new information.

If I had a chance to do this again, I'd definitely split the class
into two groups, one for people who were up and running with basic
concepts to build some programs together with one instructor. The
other two could then stay and give one to one help slowly but
surely. This would prevent us from leaving anyone behind.

In reality I'd say we had about 5 kids who were understanding what was
going on and 8 who were lost. No one had yet given up on programming
luckily, so we were still more or less OK.

## Day 3

Going into this day I knew it wasn't going to be easy

 1. We were starting to lose a bit of interest since it's getting
    later in the week
 2. Some kids were falling behind others

with this in mind, we went about introducing a few new prewritten
programs that built cubes! I'll leave it to your imagination how this
worked, it's pretty similar to `grid.py`.

For the kids who were really clicking, I challenged some of them to
explain parts of the code to me. In this context I taught a few kids
about `for` loops. It's a bit tricky to explain how they work since I
didn't want to explain what an iterable was. Remember, we hadn't
talked about any OO aspects of Python.

I introduced them to loops as something to the effect of

``` python
    for VAR in range(NUMBER):
        STMT
        STMT
        ...
```

With the explanation that

> A loop means we run that list of statements once for each
> number between 0 and `NUMBER` - 1 with `VAR` first being 0,
> then 1, then 2 and so on.

This seemed to click with most of them so quite a few got the hang of
how loops worked.

I'd actually prefer I'd built some sort of abstraction like

``` python
    def allPairs(*dims):
        ...
```

which returned an iterable (generator?) that had a list of all pairs
possible within the given set of numbers. This would eliminate the
need to talk about nested loops, which were a confusing subject for
most people.

The tricky bit is that while I was hopping from person to person, the
slower moving campers where playing with `cube.py` all on their own
and not trying to understand the whole thing but still use it.

This worked surprisingly well actually, we were challenging kids to
think about how to combine `grid.py` and `cube.py` to build things
without ever laying a block by hand. Sadly a few kids just abandoned
the effort and started playing Minecraft. This was not unexpected but
still a little sad.

To keep things going, I wrote a little program which built a cube
where the inside was filled with one thing and the outside was
another. This meant that kids could build an upside down volcano or a
waterfall.

Unfortunately, to get this to all the kids we had to hand write it on
giant post-it notes and they had to manually type it. This is another
case where we desperately needed a projector.

So the third day wasn't nearly as structured as day 2, it was really a
day when kids experimented and we tried to push kids
individually. This actually seemed to be a great help since a few more
kids had some breakthroughs on day 2 materials.

## Day 4

Now, on the final day we opted to try something a little different.

We first tried networking the Raspberry Pis since kids had been asking to
do this since day 1. Despite being able to get this working in
prep time, we had some technical issues that prevented us from getting
it working during the actual camp, very frustrating.

After the kids snack break, we went into a different room with no
computers and put up a post it with the title "Steps for Writing Code"

 1. Define Our Problem
 2. Brainstorm Solutions
 3. Compare Solutions and Choose One
 5. Implement Solution
 6. Test Implementation

Now experts will notice the missing step 5.5, "swear profusely while
implementation doesn't work". We will of course include this in a
second level camp for teaching programming :)

Now I told them that their goal was to create a program which built a
"sphere". I put the quotes there since minecraft is built from blocks
and doesn't have a smooth sphere but you can get pretty close with
bigger and bigger spheres.

So we went on to step 1. and everyone struggled to define what exactly
a sphere was and how one ought to decide what "build it" meant.

We eventually settled on our problem being to build a sphere where

 1. A sphere is a collection of all blocks within a certain distance,
    D, from the center
 2. To "build" a sphere meant we'd place the center 3 + D blocks in
    front of us and we'd color all blocks in our sphere to stone.

Next came the lively discussion on how to actually go about doing
this.

After about 5 minutes, we had a lot of hand-wavy solutions but not
actual concrete procedure for doing this so I tossed out a hint.

I stated that if someone needed a procedure for finding the space
between two blocks, I will implement a function `dist` so that

    dist(x, y)

would return the distance between the `x` block and `y` block in three
dimensions.

Now the solutions got a lot closer, people started listing steps of
what to do. I encouraged them to treat me like the computer and give
me directions. I would then walk around and "color" carpet
squares. This seemed to demonstrate which solutions weren't quite
precise enough.

Eventually, we ended on a simple solution

> Figure out the center by adding D + 3 to the current position.
> For each square in the grid S, if dist(S, Center) < D, color S

Very simple, very inefficient but correct. I then started talking
about pseudo-code and turning this into a more executable form.

The kids who understood loops jumped in and we ended with something
like

``` python
    pos = mc.player.getPos()
    pos.x += 3
    for square in fullWorld():
        if dist(square, pos) <= D:
            mc.setBlock(square.x, square.y, square.z, block.STONE)
```

I let them off the hook here and wrote the rest of the code for them
while they took a brief break.

We then adjourned into the computer room and got started testing!
We had just enough time to have everyone gather round while we built a
Death Star on the teachers machine (I was the fastest typist).

Quite a few of the kids where interested in buying their own Pis and
continuing on their own so we gave everyone their SD cards and
directions on how to acquire a Raspberry Pi. I also gave out my emails
to a few of the kids who wanted to make sure they had someone answer
questions when they were setting up their Pis.

## Recap

So dear reader, where are we left?

Well the place that ran this camp
is running more. I'm not sure if they're full, but if you're a parent
or interested kid, please email me at [jozefg AT cmu.edu].

If you're thinking that you want to run one of these camps yourself,
do it! I only have 4 pieces of advice

 1. Error on being concise and simple rather than comprehensive

     You're not going to teach someone to program in 4 days. You can
     however, make someone hate programming forever in 4 days! If they
     kids want more information, they'll ask.

     I guarantee that you'll end up flooding the kids
     with too much information if you try to be comprehensive.

 2. Always run this with more than one adult present

    Otherwise you'll end up spending the whole camp chasing after kids
    to fix issues and everyone else will be bored.

    It's always good to have more than one adult who knows Python too!
    You can do it with just one I've discovered. It is less than ideal
    however.

 3. Have a good space, with a projector!

    Projectors are great. So great that I'm very seriously considering
    buying one for the next 2 iterations of this camp.

 4. Inspire kids to want to learn more!

    That's the whole point! You'll never teach anything if you're
    fighting the kids. Make this fun and don't sweat it if you feel
    like you're not covering as much material as you'd like. This
    isn't a class, there's no exam at the end, it's supposed to be
    fun!

If anyone has any more specific questions on this camp, please comment
below and I'll respond as soon as I can.
