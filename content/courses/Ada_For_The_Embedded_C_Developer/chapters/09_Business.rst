Argumentation and Business Perspectives
==========================================

.. include:: ../../../global.txt

The technical benefits of a migration from C to Ada are usually relatively
straightforward to demonstrate. Hopefully, this course provides a good basis
for it. However, when faced with an actual business decision to make,
additional considerations need to be taken into account, such as return on
investment, perennity of the solution, tool support, etc. This section will
cover a number of usual questions and provide elements of answers.

What's the expected ROI of a C to Ada transition?
-------------------------------------------------

Switching from one technology to another is a cost, may that be in terms of
training, transition of the existing environment or acquisition of new tools.
This investment needs to be matched with an expected return on investment, or
ROI, to be consistent. Of course, it's incredibly difficult to provide a firm
answer to how much money can be saved by transitioning, as this is highly
dependent on specific project objectives and constraints. We're going to
provide qualitative and quantitative arguments here, from the perspective of a
project that has to reach a relatively high level of integrity, that is to say
a system where the occurrence of a software failure is a relatively costly 
event.

From a qualitative standpoint, there are various times in the software
development life cycle where defects can be found:

#. on the developer's desk

#. during component testing

#. during integration testing

#. after deployment

#. during maintenance

Numbers from studies vary greatly on the relative costs of defects found at
each of these phases, but there's a clear ordering between them. For example,
a defect found while developing is orders of magnitude less expensive to fix
than a defect found e.g. at integration time, which may involve costly
debugging sessions and slow down the entire system acceptance. The whole
purpose of Ada and SPARK is to push defect detection to the developer's desk as
much as possible; at least for all of these defects that can be identified at
that level. While the strict act of writing software may be taking more effort
because of all of the additional safeguards, this should have a significant and
positive impact down the line and help to control costs overall. The exact
value this may translate into is highly business dependent.

From a quantitative standpoint, two studies have been done almost 25 years
apart and provide similar insights:

- Rational Software in 1995 found that the cost of developing software in Ada
  was overall half as much as the cost of developing software in C.

- VDC ran a study in 2018, finding that the cost savings of developing with Ada
  over C ranged from 6% to 38% in savings.

From a qualitative standpoint, in particular with regards to Ada and C from a
formal proof perspective, an interesting presentation was made in 2017 by two
researchers. They tried to apply formal proof on the same piece of code,
developed in Ada/SPARK on one end and C/Frama-C on the other. Their results
indicate that the Ada/SPARK technology is indeed more conducive to formal proof
methodologies.

Although all of these studies have their own biases, they provide a good idea
of what to expect in terms of savings once the initial investment in switching
to Ada is made. This is assuming everything else is equal, in particular that
the level of integrity is the same. In many situations, the migration to Ada is
justified by an increase in terms of integrity expectations, in which case it's
expected that development costs will rise (it's more expensive to develop
better software) and Ada is viewed as a means to mitigate this rise in
development costs.

That being said, the point of this argument is not to say that it's not
possible to write very safe and secure software with languages different than
Ada. With the right expertise, the right processes and the right tools, it's
done every day. The point is that Ada overall reduces the level of processes,
expertise and tools necessary and will allow to reach the same target at a
lower cost.

Who is using Ada today?
-----------------------

Ada was initially born as a DoD project, and thus got its initial customer base
in aerospace and defence (A&D). At the time these lines are written and from
the perspective of AdaCore, A&D is still the largest consumer of Ada today and
covers about 70% of the market. This creates a consistent and long lasting set
of established users as these project last often for decades, using the same
codebase migrating from platform to platform.

More recently however, there has been an emerging interest for Ada in new
communities of users such as automotive, medical device, industrial automation
and overall cyber-security. This can probably be explained by a rise of safety,
reliability and cyber-security requirements. The market is moving relatively
rapidly today and we're anticipating an increase of the Ada footprint in these
domains, while still remaining a technology of choice for the development of
mission critical software.

What is the future of the Ada technology?
-----------------------------------------

The first piece of the answer lies in the user base of the Ada language, as
seen in the previous question. Projects using Ada in the aerospace and defence
domain maintain source code over decades, providing healthy funding foundation
for Ada-based technologies.

AdaCore being the author of this course, it's difficult for us to be fair in
our description of other Ada compilation technologies. We will leave to the
readers the responsibility of forging their own opinion. If they present a
credible alternative to the GNAT compiler, then this whole section can be
considered as void.

Assuming GNAT is the only option available, and acknowledging that this is an
argument that we're hearing from a number of Ada adopters, let's discuss the
"sole source" issue.

First of all, it's worth noting that industries are using a lot of software
that is provided by only one source, so while non-ideal, these
situations are also quite common.

In the case of the GNAT compiler however, while AdaCore is the main maintainer,
this maintenance is done as part of an open-source community. This means that
nothing prevents a third party to start selling a competing set of products
based on the same compiler, provided that it too adopts the open-source
approach. Our job is to be more cost-effective than the alternative, and indeed
for the vast part this has prevented a competing offering to emerge. However,
should AdaCore disappear or switch focus, Ada users would not be prevented from
carrying on using its software (there is no lock) and a third party could take
over maintenance. This is not a theoretical case, this has been done in the
past either by companies looking at supporting their own version of GNAT,
vendors occupying a specific niche that was left uncovered , or hobbyists
developing their own builds.

With that in mind, it's clear that the "sole source" provider issue is a
circumstantial |mdash| nothing is preventing other vendors from emerging if the
conditions are met.

Is the Ada toolset complete?
----------------------------

A language by itself is of little use for the development of safety-critical
software. Instead, a complete toolset is needed to accompany the development
process, in particular tools for edition, testing, static analysis, etc.

AdaCore provides a number of these tools either in through its core or add-on
package. These include (as of 2019):

- An IDE (GNAT Studio)

- An Eclipse plug-in (GNATbench)

- A debugger (GDB)

- A testing tool (GNATtest)

- A structural code coverage tool (GNATcoverage)

- A metric computation tool (GNATmetric)

- A coding standard checker (GNATcheck)

- Static analysis tools (CodePeer, SPARK Pro)

- A Simulink code generator (QGen)

- An Ada parser to develop custom tools (libadalang)

Ada is, however, an internationally standardized language, and many companies
are providing third party solutions to complete the toolset. Overall, the
language can be and is used with tools on par with their equivalent C
counterparts.

Where can I find Ada or SPARK developers?
-----------------------------------------

A common question from teams on the verge of selecting Ada and SPARK is how to
manage the developer team growth and turnover. While Ada and SPARK are taught
by a growing number of universities worldwide, it may still be challenging to
hire new staff with prior Ada experience.

Fortunately, Ada's base semantics are very close to those of C/C++, so that a
good embedded software developer should be able to learn it relatively easily.
This course is definitely a resource available to get started. Online training
material is also available, together with on-site in person training.

In general, getting an engineer operational in Ada and SPARK shouldn't take
more than a few weeks worth of time.

How to introduce Ada and SPARK in an existing code base?
--------------------------------------------------------

The most common scenario when introducing Ada and SPARK to a project or a team
is to do it within a pre-existing C codebase, which can already spread over
hundreds of thousands if not millions lines of code. Re-writing this software
to Ada or SPARK is of course not practical and counterproductive.

Most teams select either a small piece of existing code which deserves
particular attention, or new modules to develop, and concentrate on this.
Developing this module or part of the application will also help in developing
the coding patterns to be used for the particular project and company. This
typically concentrates an effort of a few people on a few thousands lines of
code. The resulting code can be linked to the rest of the C application.
From there, the newly established practices and their benefit can slowly
spread through the rest of the environment.

Establishing this initial core in Ada and SPARK is critical, and while learning
the language isn't a particularly difficult task, applying it to its full
capacity may require some expertise. One possibility to accelerate this initial
process is to use AdaCore mentorship services.
