
===================
Concurrency (CON)
===================

*Goal*
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance: :math:`\checkmark`
   :Security:

Description
   Have a plan for managing the use of concurrency in high-integrity
   applications having real-time requirements.

Rules
   CON01, CON02, CON03

The canonical approach to applications having multiple periodic and aperiodic
activities is to map those activities onto independent tasks, i.e., threads of
control. The advantages for the application are both a matter of software
engineering and also ease of implementation. For example, when the different
periods are not harmonics of one another, the fact that each task executes
independently means that the differences are trivially represented. In
contrast, such periods are not easily implemented in a cyclic scheduler, which,
by definition, involves only one (implicit) thread of control with one frame
rate.

High integrity applications are subject to a number of stringent analyses,
including, for example, safety analyses and certification against rigorous
industry standards. In addition, high integrity applications with real-time
requirements must undergo timing analysis because they must be shown to meet
deadlines prior to deployment -- failure to meet hard deadlines is unacceptable
in this domain.

These analyses are applied both to the application and to the implementation of
the underlying run-time library.  However, analysis of the complete set of
general Ada tasking features is not tractable, neither technically nor in terms
of cost. A subset of the language is required.

The Ravenscar profile [AdaRM2016]_ is a subset of the Ada concurrency
facilities that supports determinism, schedulability analysis, constrained
memory utilization, and certification to the highest integrity levels. Four
distinct application domains are specifically intended:

   * Hard real-time applications requiring predictability,
   * Safety-critical systems requiring formal, stringent certification,
   * High-integrity applications requiring formal static analysis and
     verification,
   * Embedded applications requiring both a small memory footprint and low
     execution overhead.

Those tasking constructs that preclude analysis at the source level or analysis
of the tasking portion of the underlying run-time library are disallowed.

The Ravenscar profile is necessarily strict in terms of what it removes so that
it can support the stringent analyses, such as safety analysis, that go beyond
the timing analysis required for real-time applications. In addition, the
strict subset facilitates that timing analysis in the first place.

However, not all high-integrity applications are amenable to expression in the
Ravenscar profile subset. The Jorvik profile [AdaRM2020]_ is an alternative
subset of the Ada concurrency facilities. It is based directly on the Ravenscar
profile but removes selected restrictions in order to increase expressive
power, while retaining analyzability and performance. As a result, typical
idioms for protected objects can be used, for example, and relative delays
statements are allowed. Timing analysis is still possible but slightly more
complicated, and the underlying run-time library is slightly larger and more
complex.

When the most stringent analyses are required and the tightest timing is
involved, use the Ravenscar profile. When a slight increase in complexity is
tolerable, i.e., in those cases not undergoing all of these stringent analyses,
consider using the Jorvik profile.

.. toctree::
   :maxdepth: 1

   Use the Ravenscar Profile (CON01) <concurrency/con01_use_the_ravenscar_profile.rst>
   Use the Jorvik Profile (CON02) <concurrency/con02_use_the_jorvik_profile.rst>
   Avoid Shared Variables for Inter-task Communication (CON03) <concurrency/con03_avoid_shared_variables_for_inter-task_communication.rst>

