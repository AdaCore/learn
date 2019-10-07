GNAT Community
==============

.. include:: <isopub.txt>

.. include:: <isoamsa.txt>

This chapter presents the steps needed to install the GNAT Community
toolchain and how to use basic commands from the toolchain.

Installation
------------

These are the basics steps to install GNAT Community on all platforms:

    - Go to the
      `AdaCore Community page <https://www.adacore.com/community>`_.

    - Download the GNAT installer.

    - Run the GNAT installer.

        - Leave all options checked on the "Select Components" page.

On Windows platforms, continue with the following steps:

    - Add ``C:\GNAT\2019\bin`` to your ``Path`` environment variable.

        - The environment variables can be found in the
          ``System Properties`` window of the ``Control Panel``.

    - You might need to restart your computer for the settings to take
      effect.

On Linux platforms, perform the following steps:

    - Make sure the GNAT installer has execution permissions before
      running it.

    - Select the directory where you want to install the toolchain.

        - For example: ``/home/me/GNAT/2019``

    - Add the path to the ``bin`` directory (within the toolchain
      directory) as the first directory in your ``PATH`` environment
      variable.

        - For example: ``/home/me/GNAT/2019/bin``.

Basic commands
--------------

Now that the toolchain is installed, you can start using it. From the
command line, you can compile a project using :program:`gprbuild`. For
example:

.. code-block:: sh

    gprbuild -P project.gpr

You can find the binary built with the command above in the *obj*
directory. You can the run it in the same way as you would do with any
other executable on your platform. For example:

.. code-block:: sh

    obj/main

A handy command-line option for :program:`gprbuild` you might want to use
is ``-p``, which automatically creates directories such as ``obj`` if they
aren't in the directory tree:

.. code-block:: sh

    gprbuild -p -P project.gpr

Ada source-code are stored in *.ads* and *.adb* files. To view the
content of these files, you can use the GNAT Programming Studio
(:program:`GPS`). To open :program:`GPS`, double-click on the *.gpr*
project file or invoke :program:`GPS` on the command line:

.. code-block:: sh

    gps -P project.gpr

To compile your project using :program:`GPS`, use the top-level menu to
invoke ``Build`` |srarr| ``Project`` |srarr| ``main.adb`` (or press the
keyboard shortcut ``F4``). To run the main program, click on
``Build`` |srarr| ``Run`` |srarr| ``main`` (or press the keyboard shortcut
``Shift + F2``).
